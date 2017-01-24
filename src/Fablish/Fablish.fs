#if INTERACTIVE
#else
namespace Fablish
#endif

type Fablish = class end // hack to allow module and type with same name

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Fablish =

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


    let runView (instance : FablishInstance<_,_>) (onMessage : Callback<'model,'msg>) (app : App<_,_,_>) (webSocket : WebSocket) : HttpContext -> SocketOp<unit> =

        let writeString (s : string) =
            socket {
                do! webSocket.send Opcode.Text (getBytes s) true
            }

        let sw = System.Diagnostics.Stopwatch()

        let lockObj = obj()

        let mutable currentRegistrations = Map.empty
        let mutable currentSubscription = Sub.NoSub

        let send (model : 'model) =
            socket {
                sw.Restart()
                let view                   = app.view model 
                let sub                    = app.subscriptions model
                let vdom, registrations, s = render view
                sw.Stop()
                printfn "[fablish] rendering performed in %f milliseconds" sw.Elapsed.TotalMilliseconds

                let allSubscriptions = Sub.extract sub |> instance.SendSubs
                currentSubscription <- sub

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
       
        and receive (instance : FablishInstance<_,_>) = 
            socket {
                let! result = tryReceiveChannel ()
                match result with
                    | Termination -> ()
                    | Message msg -> 
                        instance.Run(fun _ -> onMessage instance.UnsafeCurrentModel msg |> ignore) |> ignore
                        return! receive instance
                    | NoMessage -> 
                        return! receive instance
                    | ForceRendering -> 
                        let! _ = lock lockObj (fun _ -> send instance.UnsafeCurrentModel)
                        return! receive instance
            }

        and runOuterChanges (mvar : MVar<'model>) (instance : FablishInstance<_,_>) =
            socket {
                let! model = SocketOp.ofAsync <| MVar.takeAsync mvar 
                let! _ = lock lockObj (fun _ -> send model)
                return! runOuterChanges mvar instance
            }

        and runElmLoop (mvar : MVar<'model>) (instance : FablishInstance<'model,'msg>) =
            socket {
                let initialModel = MVar.take mvar
                let! registrations = send initialModel
                return! receive instance
            }

        fun ctx -> 
            socket {
                let! ct = Async.CancellationToken
                let! msg = webSocket.read()
                match msg with
                    | (Opcode.Text,data,true) -> 
                        let s = getString data
                        if s <>  magic then printfn "[fablish warning] wrong magic first message. should be: %s but was %s. trying to run anyways..." magic s

                        let mvar = MVar.empty()
                        instance.AddViewer mvar
                        let t = runOuterChanges mvar instance |> Async.Ignore
                        Async.Start(t,ct)
                        return! runElmLoop mvar instance
//                        else 
//                            return failwithf "initial handshake failed. Web should have said: %s" magic
                    | _ -> return! failwith "initial handshake failed (should have received text)"
            }


    let runPlain instance onMessage app : WebPart =
        path "/ws" >=> handShake (runView instance onMessage app)

    let runApp mainPage instance onMessage app : WebPart =
        choose [
            runPlain instance onMessage app
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


    type FablishResult<'model,'msg> = {
        localUrl : string
        runningTask : Tasks.Task<unit>
        instance    : FablishInstance<'model,'msg>
        shutdown    : unit -> unit
    }

    let serve (address : IPAddress) (port : string) (onMessage : Option<Callback<'model,'msg>>) (env : Option<Env<'msg>>) (app : App<_,_,_>) =
        let c = Console.ForegroundColor
        Console.ForegroundColor <- ConsoleColor.Green
        printfn "%s" logo
        Console.ForegroundColor <- c
        let path = "static/index.html"

        let config =
          { defaultConfig with
             bindings = [ HttpBinding.mk HTTP address (Port.Parse port) ]
          }

        let instance = new FablishInstance<'model,'msg>(app.initial, env, app.update)

        let onMessage = 
            match onMessage with
                | Some m -> m
                | None -> (fun (model : 'model) msg -> app.update instance.Env model msg |> instance.EmitModel)
        
        let cts = new CancellationTokenSource()
        let listening,server = startWebServerAsync defaultConfig (runApp path instance onMessage app)
        
        let t = Async.StartAsTask(server,cancellationToken = cts.Token)
        listening |> Async.RunSynchronously |> printfn "[Fablish-suave] start stats: %A"
        {
             localUrl =  sprintf "http://localhost:%s/mainPage" port
             runningTask = t
             instance = instance
             shutdown  = fun () -> instance.Dispose(); cts.Cancel(); 
        }


open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.Html
open Fablish
open System.Net

type Fablish with 

    static member Serve(app : App<'model,'msg, DomNode<'msg>>,?address : IPAddress, ?port : string, ?onMessage : Callback<'model,'msg>, ?env : Env<'msg>)=
        let address = defaultArg address IPAddress.Loopback
        let port = defaultArg port "8083"
        Fablish.serve address port onMessage env app 

    static member ServeLocally(app : App<'model,'msg, DomNode<'msg>>, ?port : string, ?onMessage : Callback<'model,'msg>, ?env : Env<'msg>) =
        let port = defaultArg port "8083"
        Fablish.serve IPAddress.Loopback port onMessage env app

    static member RunLocally(app : App<'model,'msg, DomNode<'msg>>, ?port : string, ?onMessage : Callback<'model,'msg>, ?env : Env<'msg>)=
        let port = defaultArg port "8083"
        let r = Fablish.serve IPAddress.Loopback port onMessage env app
        r.runningTask.Wait()

