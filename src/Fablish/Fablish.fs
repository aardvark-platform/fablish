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

module Scripts =
    let ignore = "() => { return {}; }"

type RenderCallback<'msg> = string -> Option<'msg>
type Reaction<'msg> = { clientSide : Script; serverSide : RenderCallback<'msg>}
type OnRendered<'model,'msg,'view> = 'model -> 'view -> Reaction<'msg>

module OnRendered =
    let ignore : 'model -> 'view -> Reaction<'msg> = fun _ _ -> { clientSide = Scripts.ignore; serverSide = fun _ -> None }

type App<'model,'msg,'view> = 
    {
        // Elm interface
        initial  : 'model
        update   : 'model -> 'msg -> 'model
        view     : 'model -> 'view

        // IO extensions
        onRendered : OnRendered<'model,'msg,'view>
    }


open System.Threading
type Id = int
type ID() =
    let mutable currentId = 0
    member x.New () =
        Interlocked.Increment(&currentId)
    member x.All = [ 0 .. currentId ]

[<AutoOpen>]
module JavascriptInterop = 
    // {"bottom":44,"height":25,"left":0,"right":78.734375,"top":19,"width":78.734375}"
    type ClientRect = { 
        bottom : float
        height : float
        left   : float
        right  : float
        top    : float
        width  : float
    }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ClientRect =
        let ofString (s : string) : ClientRect = Pickler.json.UnPickleOfString s
        let toString (c : ClientRect) = Pickler.json.PickleToString c
        let empty = { bottom = 0.0; height = 0.0; left = 0.0; right = 0.0; top = 0.0; width = 0.0 }

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
        let (s,r),dom = State.run (0, Map.empty) (Fable.Helpers.Virtualdom.ReactDomRenderer.render ignore topLevelWrapped)
        //printfn "generated code: \n%s" dom
        dom,r,s

    let (|Int|_|) (s : string) =
        match Int32.TryParse(s) with
            | (true,v) -> Some v
            | _ -> None

    let magic = "This is Aardvark"

    [<Literal>]
    let eventOccurance  = 1
    [<Literal>]
    let forceRendering = 2

    type Event = { eventId : string; eventValue : string }
    type Message = { id : int; data : Event }
    type RenderRequest = { dom : string; script : Script; id : string }
   
    type Result<'msg> = NoMessage | Message of 'msg | Termination | ForceRendering

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
                let view                   = app.view model 
                let vdom, registrations, s = render view
                sw.Stop()
                printfn "[fablish] rendering performed in %f milliseconds" sw.Elapsed.TotalMilliseconds

                let reaction = app.onRendered model view
                let onRenderedEvt = s + 1
                let bytes = { dom = vdom; script = reaction.clientSide; id = string onRenderedEvt } |> Pickler.json.Pickle 

                printfn "[fablish] writing %f kb to client" (float bytes.Length / 1024.0)
                do! webSocket.send Opcode.Text bytes true
                return Map.add onRenderedEvt (fun a -> reaction.serverSide (unbox a)) registrations
            }


        let rec tryReceiveChannel registrations = 
            socket {
                let! msg = webSocket.read()
                match msg with
                    | (Opcode.Text, data, true) -> 
                        let msg = parseMessage (getString data)
                        match msg with
                            | Choice1Of2 
                                { id = id; data = { eventId = Int eventId; eventValue = eventValue} } 
                                    when id = eventOccurance -> 
                                        match Map.tryFind eventId registrations with
                                            | Some action -> 
                                                match action eventValue with
                                                    | Some msg -> 
                                                        return Message msg
                                                    | _ -> 
                                                        return NoMessage 
                                            | None -> 
                                                printfn "[fablish] dont understand event. id was: %A" eventId
                                                return NoMessage
                            | Choice1Of2 { id = id; data = _ } when id = forceRendering -> 
                                return ForceRendering
                            | Choice1Of2 m -> 
                                return failwithf "could not understand message: %A" m
                            | Choice2Of2 m ->  
                                return failwithf "protocol error: %s" m

                    | (Opcode.Close, _, _) -> 
                        return Termination
                    | _ -> 
                        return failwithf "[fablish] protocol error (Web said: %A instead of text or close)" msg
            }
        
        and tryReceive registrations = 
            tryReceiveChannel registrations

        and receive model registrations = 
            socket {
                let! result = tryReceive registrations
                match result with
                    | Termination -> ()
                    | Message msg -> 
                        let newModel = app.update model msg
                        return! runElmLoop newModel
                    | NoMessage -> 
                        return! receive model registrations 
                    | ForceRendering -> 
                        return! runElmLoop model
            }

        and runElmLoop (model : 'model) =
            socket {
                let! registrations = send model
                return! receive model registrations
            }
            
        fun cx -> 
            socket {
                let! msg = webSocket.read()
                match msg with
                    | (Opcode.Text,data,true) -> 
                        let s = getString data
                        if s =  magic then
                            return! runElmLoop app.initial
                        else 
                            return failwithf "initial handshake failed. Web should have said: %s" magic
                    | _ -> return! failwith "initial handshake failed (should have received text)"
            }           

    let readDefaultIndex mainPage = 
        let c = Console.BackgroundColor
        Console.ForegroundColor <- ConsoleColor.Green
        printfn "[fablish] trying to serve: %s but the file could not be found in the home directory (typically ./static/index.html). Trying to use default index.html from fablish build (using embedded resource)." mainPage
        let info = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream("index.html")
        use s = new System.IO.StreamReader(info)
        Console.ForegroundColor <- c
        s.ReadToEnd()

    let runPlain app : WebPart =
        path "/ws" >=> handShake (runConnection app)


    let runApp (mainPage : string) (app : App<_,_,_>) : WebPart =
        choose [
            runPlain app
            GET >=> choose [ path "/mainPage" >=> file mainPage;  path "/mainPage" >=> OK (readDefaultIndex mainPage); browseHome ];
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