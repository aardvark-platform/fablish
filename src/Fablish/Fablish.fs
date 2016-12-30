namespace Fablish

open System


open Suave
open Suave.Http
open Suave.Operators
open Suave.Filters
open Suave.Successful
open Suave.Files
open Suave.RequestErrors
open Suave.Logging
open Suave.Utils

open System
open System.Net

open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket

open System.Text

type Script = string
type App<'model,'msg,'view> = 
    {
        initial  : 'model
        update   : 'model -> 'msg -> 'model
        view     : 'model -> 'view
        onRendered : 'model -> 'view -> Script
    }


open System.Threading
type Id = int
type ID() =
    let mutable currentId = 0
    member x.New () =
        Interlocked.Increment(&currentId)
    member x.All = [ 0 .. currentId ]


module Scripts =
    let ignore _ _ = "() => { return {}; }"

module Fablish =
    open Fable.Helpers.Virtualdom
    open Fable.Helpers.Virtualdom.Html

    open Aardvark.Base.Monads.State

    let getBytes (s : String)  = 
        Encoding.UTF8.GetBytes s

    let getByteSegment (s : String)  = 
        ByteSegment(Encoding.UTF8.GetBytes s)

    let getString (b : byte[]) = Encoding.UTF8.GetString b
        

    let render v = 
        let topLevelWrapped = div [] [v] // some libs such as react do crazy shit with top level element
        let (_,r),dom = State.run (0, Map.empty) (Fable.Helpers.Virtualdom.ReactDomRenderer.render ignore topLevelWrapped)
        //printfn "generated code: \n%s" dom
        dom,r

    let (|Int|_|) (s : string) =
        match Int32.TryParse(s) with
            | (true,v) -> Some v
            | _ -> None

    let magic = "This is Aardvark"

    [<Literal>]
    let eventOccurance  = 1
    [<Literal>]
    let renderingResult = 2

    type Event = { eventId : string; eventValue : string }
    type Message = { id : int; data : Event }
    type RenderRequest = { dom : string; script : Script }

    let parseMessage (s : string) =
        try
            Choice1Of2 <| Pickler.json.UnPickleOfString s
        with e -> 
            Choice2Of2 (sprintf "could not parse: %s with: %s" s e.Message)

    let runConnection (app : App<'model, 'msg, DomNode<'msg>>) (webSocket : WebSocket)  =

        let writeString (s : string) =
            socket {
                do! webSocket.send Opcode.Text (getBytes s) true
            }

        let sw = System.Diagnostics.Stopwatch()

        let send (model : 'model) =
            socket {
                sw.Restart()
                let view                = app.view model 
                let vdom, registrations = render view
                sw.Stop()
                printfn "[fablish] rendering performed in %f milliseconds" sw.Elapsed.TotalMilliseconds

                let script = app.onRendered model view
                let bytes = { dom = vdom; script = script } |> Pickler.json.Pickle 

                printfn "[fablish] writing %f kb to client" (float bytes.Length / 1024.0)
                do! webSocket.send Opcode.Text bytes true
                return registrations
            }

        let rec runElmLoop (model : 'model) (registrations) =
            let rec receive registrations = 
                socket {
                    let! msg = webSocket.read()
                    match msg with
                        | (Opcode.Text, data, true) -> 
                            let msg = parseMessage (getString data)
                            match msg with
                                | Choice1Of2 
                                    { id = id; data = { eventId = Int eventId; eventValue = eventValue} } 
                                      // normal event
                                      when id = eventOccurance -> 
                                        match Map.tryFind eventId registrations with
                                            | Some action -> 
                                                let msg = action eventValue
                                                let newModel = app.update model msg
                                                return! runElmLoop newModel registrations
                                            | None -> 
                                                printfn "[fablish] dont understand event. id was: %A" eventId
                                                return! runElmLoop model registrations
                                | Choice1Of2 
                                    { id = id; data = { eventId = Int eventId; eventValue = eventValue} } 
                                      // system event, after rendering view
                                      when id = renderingResult -> 
                                        printfn "[fablish] webPage response: %A" eventValue
                                        return! receive registrations
                                | Choice1Of2 m -> return failwithf "could not understand message: %A" m
                                | Choice2Of2 m ->  return failwithf "protocol error: %s" m

                        | (Opcode.Close, _, _) -> ()
                        | _ -> return failwithf "[fablish] protocol error (Web said: %A instead of text or close)" msg
             }
            socket {
                let! registrations = send model
                return! receive registrations
            }
            
        fun cx -> 
            socket {
                let! msg = webSocket.read()
                match msg with
                    | (Opcode.Text,data,true) -> 
                        let s = getString data
                        if s =  magic then
                            return! runElmLoop app.initial Map.empty
                        else 
                            return failwithf "initial handshake failed. Web should have said: %s" magic
                    | _ -> return! failwith "initial handshake failed (should have received text)"
            }           


    let runPlain app : WebPart =
        path "/ws" >=> handShake (runConnection app)

    let runApp mainPage app : WebPart =
        choose [
            runPlain app
            GET >=> choose [ path "/mainPage" >=> file mainPage; browseHome ];
            GET >=> path "/image" >=> 
            NOT_FOUND "Found no handlers."
        ]

    let logo = """                                                       
,------. ,---.  ,-----.  ,--.   ,--. ,---.  ,--.  ,--. 
|  .---'/  O  \ |  |) /_ |  |   |  |'   .-' |  '--'  | 
|  `--,|  .-.  ||  .-.  \|  |   |  |`.  `-. |  .--.  | 
|  |`  |  | |  ||  '--' /|  '--.|  |.-'    ||  |  |  | 
`--'   `--' `--'`------' `-----'`--'`-----' `--'  `--'"""

    let serve address port app =
        printfn "%s" logo
        let path = "static/index.html"

        let config =
          { defaultConfig with
             bindings = [ HttpBinding.mk HTTP address (Port.Parse port) ]
             //homeFolder = Some @"C:\Aardwork\"
          }
        
        let cts = new CancellationTokenSource()
        let listening,server = startWebServerAsync defaultConfig (runApp path app)
        
        let urla = if IPAddress.IsLoopback address then "localhost" else sprintf "%A" address
        
        let t = Async.StartAsTask(server,cancellationToken = cts.Token)
        listening |> Async.RunSynchronously |> printfn "[Fablish-suave] start stats: %A"
        sprintf "http://%s:%s/mainPage" urla port, t, cts

    let serveLocally port app = serve IPAddress.Loopback port app

    let runLocally port app =
        let url, task, cancel = serveLocally port app
        task.Wait()