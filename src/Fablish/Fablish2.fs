#if INTERACTIVE
#else
namespace Fablish
#endif


module WebSocket =

    open System
    open System.Net
    open System.Web
    open System.Text
    open System.Net.WebSockets
    open System.Threading
    open System.Threading.Tasks
    open System.Collections.Concurrent

    let listen (url : string) (handler : WebSocket -> unit) =
    
        let websockets = ConcurrentHashSet()

        let handleRequest (ctx : HttpListenerContext) (ws : WebSocket) =
            async {
                do! Async.SwitchToThreadPool()
                try
                    handler ws
                with e ->
                    printfn "[Fablish.WebSocket] faulted with: %s" e.Message
                    websockets.Remove ws |> ignore
            } 


        let listener = new HttpListener()
        listener.Prefixes.Add(url)

        let cts = new CancellationTokenSource()
        let runner =
            async {
                do! Async.SwitchToThreadPool()
                while true do 
                    let! ctx = listener.GetContextAsync() |> Async.AwaitTask
                    if ctx.Request.IsWebSocketRequest then
                        let! ws = ctx.AcceptWebSocketAsync(null) |> Async.AwaitTask
                        do! handleRequest ctx ws.WebSocket
                    else 
                        printfn "[Fablish.WebSocket] Non websocket request received. ignoring."
            } 
        Async.Start(runner,cts.Token)
        websockets,cts


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

    type SharedState<'model,'msg,'view> =
        {
            model : ref<'model>
            view  : ref<Option<'view*Registrations<'msg>>>
            viewers : List<obj>
        }

    let runView sharedState app (webSocket : WebSocket) : HttpContext -> SocketOp<unit> =
        fun ctx -> 
            socket {
                let! msg = webSocket.read()
                failwith ""
            }

    let runPlain sharedState app : WebPart =
        path "/ws" >=> handShake (runView sharedState app)

    let runApp mainPage sharedState app : WebPart =
        choose [
            runPlain sharedState app
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
`--'   `--' `--'`------' `-----'`--'`-----' `--'  `--'"""

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

        let sharedState = { model = ref app.initial; view = ref None; viewers = List() }
        
        let cts = new CancellationTokenSource()
        let listening,server = startWebServerAsync defaultConfig (runApp path sharedState app)
        
        let urla = if IPAddress.IsLoopback address then "localhost" else sprintf "%A" address
        
        let t = Async.StartAsTask(server,cancellationToken = cts.Token)
        listening |> Async.RunSynchronously |> printfn "[Fablish-suave] start stats: %A"
        sprintf "http://%s:%s/mainPage" urla port, t, cts

    let serveLocally port app = serve IPAddress.Loopback port app

    let runLocally port app =
        let url, task, cancel = serveLocally port app
        task.Wait()