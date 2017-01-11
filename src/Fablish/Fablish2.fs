#if INTERACTIVE
#else
namespace Fablish
#endif


module Fablish2 =

    open System
    open System.Collections.Generic
    open System.Threading
    open System.Net

    open Suave
    open Suave.Sockets
    open Suave.Operators
    open Suave.WebSocket
    open Suave.Files
    open Suave.RequestErrors
    open Suave.Successful
    open Suave.Filters
    open Suave.Sockets.Control

    open Aardvark.Base
    open Aardvark.Base.Incremental

    open Fable.Helpers.Virtualdom
    open Fable.Helpers.Virtualdom.Html

    open Aardvark.Base.Monads.State

    let render v = 
        let topLevelWrapped = div [] [v] // some libs such as react do crazy shit with top level element
        let (s,r),dom = State.run (0, Map.empty) (Fable.Helpers.Virtualdom.ReactDomRenderer.render ignore topLevelWrapped)
        //printfn "generated code: \n%s" dom
        dom,r,s

    let (|Int|_|) (s : string) =
        match Int32.TryParse(s) with
            | (true,v) -> Some v
            | _ -> None
   
    type Result<'msg> = NoMessage | Message of 'msg | Termination | ForceRendering

    let parseMessage (s : string) =
        try
            Choice1Of2 <| Pickler.json.UnPickleOfString s
        with e -> 
            Choice2Of2 (sprintf "could not parse: %s with: %s" s e.Message)


    type RunningApp<'model,'msg>(m : 'model, update : 'model -> 'msg -> 'model) =
        let viewers = HashSet<MVar<'model>>()
        let model = Mod.init m

        member x.AddViewer m =
            lock viewers (fun _ -> 
                viewers.Add m |> ignore
                m.Put model.Value
            )
        member x.EmitMessage msg =
            lock viewers (fun _ -> 
                let newModel = update model.Value msg
                model.Value <- newModel
                for v in viewers do
                    MVar.put v newModel
            )
        member x.UnsafeCurrentModel = model.Value

    let runView (runningApp : RunningApp<_,_>) app (webSocket : WebSocket) : HttpContext -> SocketOp<unit> =

        let writeString (s : string) =
            socket {
                do! webSocket.send Opcode.Text (getBytes s) true
            }

        let sw = System.Diagnostics.Stopwatch()

        let takeModel (m : MVar<_>) = SocketOp.ofAsync (MVar.takeAsync m)

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
                                                printfn "[fablish] dont understand event. id was: %A and content was: %A" eventId eventValue
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

        and receive mvar (runningApp : RunningApp<_,_>) registrations = 
            socket {
                let! result = SocketOp.ofAsync ( Async.Choice2(tryReceive registrations, takeModel mvar) )
                match result with
                    | Some (Choice1Of2(Choice2Of2 e)) -> return! SocketOp.abort e
                    | Some (Choice1Of2(Choice1Of2 r)) -> 
                        match r with
                            | Termination -> ()
                            | Message msg -> 
                                runningApp.EmitMessage msg
                                return! receive mvar runningApp registrations
                            | NoMessage -> 
                                return! receive mvar runningApp registrations
                            | ForceRendering -> 
                                let! registrations = send runningApp.UnsafeCurrentModel
                                return! receive mvar runningApp registrations
                    | Some (Choice2Of2(Choice2Of2 e)) -> return! SocketOp.abort e
                    | Some (Choice2Of2(Choice1Of2 m)) -> 
                        let! registrations = send m
                        return! receive mvar runningApp registrations
                    | None -> return! SocketOp.abort <| Error.InputDataError "could not join"
            }

        and runElmLoop (mvar : MVar<'model>) (runningApp : RunningApp<'model,'msg>) =
            socket {
                let initialModel = MVar.take mvar
                let! registrations = send initialModel
                return! receive mvar runningApp registrations
            }

        fun ctx -> 
            socket {
                let! msg = webSocket.read()
                match msg with
                    | (Opcode.Text,data,true) -> 
                        let s = getString data
                        if s =  magic then
                            let mvar = MVar.empty()
                            runningApp.AddViewer mvar
                            return! runElmLoop mvar runningApp
                        else 
                            return failwithf "initial handshake failed. Web should have said: %s" magic
                    | _ -> return! failwith "initial handshake failed (should have received text)"
            }


    let runPlain runningApp app : WebPart =
        path "/ws" >=> handShake (runView runningApp app)

    let runApp mainPage runningApp app : WebPart =
        choose [
            runPlain runningApp app
            GET >=> choose [ 
                path "/mainPage" >=> file mainPage
                path "/mainPage" >=> OK (EmbeddedResources.extractPage mainPage)
                browseHome ]
            GET >=> path "/image" >=> 
            NOT_FOUND "Found no handlers."
        ]

    let logo = """                                                       
,------. ,---.  ,-----.  ,--.   ,--. ,---.  ,--.  ,--. 
|  .---'/  O  \ |  |) /_ |  |   |  |'   .-' |  '--'  | 
|  `--,|  .-.  ||  .-.  \|  |   |  |`.  `-. |  .--.  | 
|  |`  |  | |  ||  '--' /|  '--.|  |.-'    ||  |  |  | 
`--'   `--' `--'`------' `-----'`--'`-----' `--'  `--' 2.0"""

    let serve address port app =
        let c = Console.ForegroundColor
        Console.ForegroundColor <- ConsoleColor.Green
        printfn "%s" logo
        Console.ForegroundColor <- c
        let path = "static/index.html"

        let config =
          { defaultConfig with
             bindings = [ HttpBinding.mk HTTP address (Port.Parse port) ]
          }

        let runningApp = RunningApp<_,_>(app.initial, app.update)
        
        let cts = new CancellationTokenSource()
        let listening,server = startWebServerAsync defaultConfig (runApp path runningApp app)
        
        let urla = if IPAddress.IsLoopback address then "localhost" else sprintf "%A" address
        
        let t = Async.StartAsTask(server,cancellationToken = cts.Token)
        listening |> Async.RunSynchronously |> printfn "[Fablish-suave] start stats: %A"
        sprintf "http://%s:%s/mainPage" urla port, t, cts, runningApp

    let serveLocally port app = serve IPAddress.Loopback port app

    let runLocally port app =
        let url, task, cancel, app = serveLocally port app
        task.Wait()
        app