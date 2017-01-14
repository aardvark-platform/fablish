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

    type Callback<'msg> = 'msg -> ('msg -> unit) -> unit

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
        let modelSubscriptions = Dictionary<_,_>()
        let messageSubscriptions = Dictionary<_,_>()
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
                for sub in messageSubscriptions.Values do
                    sub msg

                for v in viewers do
                    MVar.put v newModel
            )

        member x.SubscribeModel(f : 'model -> unit) =
            let d = { new IDisposable with member x.Dispose() = lock viewers (fun _ -> modelSubscriptions.Remove x |> ignore) }
            lock viewers (fun _ -> 
                modelSubscriptions.Add(d,f) |> ignore
            )
            d

        member x.SubscribeMessage(f : 'msg -> unit) =
            let d = { new IDisposable with member x.Dispose() = lock viewers (fun _ -> messageSubscriptions.Remove x |> ignore) }
            lock viewers (fun _ -> 
                messageSubscriptions.Add(d,f) |> ignore
            )
            d

        member x.UnsafeCurrentModel = model.Value

    let runView (runningApp : RunningApp<_,_>) (onMessage : Callback<'msg>) (app : App<_,_,_>) (webSocket : WebSocket) : HttpContext -> SocketOp<unit> =

        let writeString (s : string) =
            socket {
                do! webSocket.send Opcode.Text (getBytes s) true
            }

        let sw = System.Diagnostics.Stopwatch()

        let lockObj = obj()

        let mutable currentRegistrations = Map.empty

        let send (model : 'model) =
            socket {
                sw.Restart()
                let view                   = app.view model 
                let vdom, registrations, s = render view
                sw.Stop()
                printfn "[fablish] rendering performed in %f milliseconds" sw.Elapsed.TotalMilliseconds

                let reaction = app.onRendered model view
                let onRenderedEvt = s + 1

                let script =
                    match reaction.clientSide with
                        | JsLambda s -> s
                        | Ignore -> ""

                let bytes = { dom = vdom; script = script; id = string onRenderedEvt } |> Pickler.json.Pickle 

                printfn "[fablish] writing %f kb to client" (float bytes.Length / 1024.0)
                do! webSocket.send Opcode.Text bytes true
                currentRegistrations <- Map.add onRenderedEvt (fun a -> reaction.serverSide (unbox a)) registrations
            }



        let rec tryReceiveChannel () = 
            socket {
                let! msg = webSocket.read()
                match msg with
                    | (Opcode.Text, data, true) -> 
                        let msg = parseMessage (getString data)
                        match msg with
                            | Choice1Of2 
                                { id = id; data = { eventId = Int eventId; eventValue = eventValue} } 
                                    when id = eventOccurance -> 
                                        match Map.tryFind eventId currentRegistrations with
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
       

        and receive (runningApp : RunningApp<_,_>) = 
            socket {
                let! result = tryReceiveChannel ()
                match result with
                    | Termination -> ()
                    | Message msg -> 
                        onMessage msg runningApp.EmitMessage 
                        return! receive runningApp
                    | NoMessage -> 
                        return! receive runningApp
                    | ForceRendering -> 
                        let! _ = lock lockObj (fun _ -> send runningApp.UnsafeCurrentModel)
                        return! receive runningApp
            }

        and runOuterChanges (mvar : MVar<'model>) (runningApp : RunningApp<_,_>) =
            socket {
                let! model = SocketOp.ofAsync <| MVar.takeAsync mvar 
                let! _ = lock lockObj (fun _ -> send model)
                return! runOuterChanges mvar runningApp
            }

        and runElmLoop (mvar : MVar<'model>) (runningApp : RunningApp<'model,'msg>) =
            socket {
                let initialModel = MVar.take mvar
                let! registrations = send initialModel
                return! receive runningApp
            }

        fun ctx -> 
            socket {
                let! ct = Async.CancellationToken
                let! msg = webSocket.read()
                match msg with
                    | (Opcode.Text,data,true) -> 
                        let s = getString data
                        if s =  magic then
                            let mvar = MVar.empty()
                            runningApp.AddViewer mvar
                            let t = runOuterChanges mvar runningApp |> Async.Ignore
                            Async.Start(t,ct)
                            return! runElmLoop mvar runningApp
                        else 
                            return failwithf "initial handshake failed. Web should have said: %s" magic
                    | _ -> return! failwith "initial handshake failed (should have received text)"
            }


    let runPlain runningApp onMessage app : WebPart =
        path "/ws" >=> handShake (runView runningApp onMessage app)

    let runApp mainPage runningApp onMessage app : WebPart =
        choose [
            runPlain runningApp onMessage app
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

//
//    type Elmish<'model,'msg> = {
//        update  : 'model -> 'msg -> 'model
//        view    : 'model -> rapp<'msg> 
//    }

    type FablishResult<'model,'msg> = {
        url : string
        runningTask : Tasks.Task<unit>
        runningApp  : RunningApp<'model,'msg>
        shutdown    : System.Threading.CancellationTokenSource
    }

    let serve (address : IPAddress) (port : string) (onMessage : Option<Callback<'msg>>) (app : App<_,_,_>) =
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

        let self msg send = send msg

        let onMessage = 
            match onMessage with
                | Some m -> m
                | None -> self
        
        let cts = new CancellationTokenSource()
        let listening,server = startWebServerAsync defaultConfig (runApp path runningApp onMessage app)
        
        let urla = if IPAddress.IsLoopback address then "localhost" else sprintf "%A" address
        
        let t = Async.StartAsTask(server,cancellationToken = cts.Token)
        listening |> Async.RunSynchronously |> printfn "[Fablish-suave] start stats: %A"
        {
             url =  sprintf "http://%s:%s/mainPage" urla port
             runningTask = t
             runningApp =runningApp
             shutdown  = cts
        }

    let serveLocally port app = serve IPAddress.Loopback port app

    let runLocally port app =
        let result = serveLocally port None app
        result.runningTask.Wait()
        app
