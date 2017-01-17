module Program

open System
open Fablish

open System.Windows.Forms



module PerformanceTest = 

    open Fable.Helpers.Virtualdom
    open Fable.Helpers.Virtualdom.Html
     
     type Model = int

     type Action = Inc | Dec

     let update env (m : Model) (a : Action) =
        match a with
            | Inc -> m + 1
            | Dec -> m - 1

     let view (m : Model) =
        div [] [
            Text (sprintf "%A" m) 
            div [] (List.init m (fun i -> button [onMouseClick (fun _ -> Inc)] [Text (sprintf "%d" i)]))
        ]

    let app =
        {
            initial = 10000
            update = update
            view = view
            subscriptions = Subscriptions.none
            
            onRendered = OnRendered.ignore
        }

module MetroTest =

    open Fable.Helpers.Virtualdom
    open Fable.Helpers.Virtualdom.Html
    
    type Model = int

    //<div data-role="group" data-group-type="multi-state" data-button-style="class">
    //    <button class="button">1</button>
    //    <button class="button">2</button>
    //    <button class="button">3</button>
    //</div>

    let update _ m _ = m


    let view (m : Model) =
        div [] [
            div [attribute "data-role" "group"; attribute "data-group-type" "multi-state"; attribute "data-button-style" "class"] [
                button [attribute "className" "button"] [Text "A"]
                button [attribute "className" "button"] [Text "B"]
                button [attribute "className" "button"] [Text "C"]
            ]
        ]

    let app =
        {
            initial = 10000
            update = update
            view = view
            subscriptions = Subscriptions.none
            onRendered = OnRendered.ignore
        }


open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.Html

module ClientViewportApp =

    
    type Model = { number : int; progress : Option<int> }

    type Action = Inc | Dec | StartExpensive | ReceiveExpensive of int | Progress of int

    let update (env : Env<Action>) (m : Model) (a : Action) =
        printfn "[Test] computing udpate"
        match a with
            | Inc -> { m with number = m.number + 1 }
            | Dec -> { m with number = m.number - 1 }
            | StartExpensive -> 
                async { 
                    for i in 0 .. 100 do
                        async { return Progress i } |> Cmd |> env.run
                        let! r = Async.Sleep 10
                        printfn "computing..."
                    return ReceiveExpensive 1
                } |> Cmd |> env.run
                m
            | Progress a -> 
                { m with progress = Some a }
            | ReceiveExpensive a -> 
                { m with number = a; progress = None }

    let view (m : Model) : DomNode<Action> =
        printfn "[Test] Computing view"
        div [] [
            text (sprintf "current content: %d" m.number)
            br []
            button [onMouseClick (fun dontCare -> Inc); attribute "id" "urdar"] [text "increment"]
            button [onMouseClick (fun dontCare -> Dec)] [text "decrement"]
            button [
                yield onMouseClick (fun dontCare -> StartExpensive); 
                if m.progress.IsSome then 
                    yield attribute "disabled" "disabled" 
                    yield Style ["backgroundColor","grey"]
            ] [text "expensive"]
            br []
            text (
                match m.progress with
                    | None -> sprintf "no operation running"
                    | Some v -> sprintf "progress %d/100" v
            )
            br []
        ]

    let onRendered model view =
        {
            clientSide = JsLambda """() => { 
                var rect = document.getElementById("urdar").getBoundingClientRect();
                return { bottom : rect.bottom.toFixed(), height : rect.height.toFixed(), left : rect.left.toFixed(), right : rect.right.toFixed(), top : rect.top.toFixed(), width : rect.width.toFixed() }; 
            } """   
            serverSide = fun (s : string) -> printfn "clientRect: %A" (ClientRect.ofString s); None
        }

    let initial =  { number = 0; progress = None }

    let app =
        {
            initial =  { number = 0; progress = None }
            update = update 
            view = view
            subscriptions = Subscriptions.none
            onRendered = onRendered //OnRendered.ignore
        }


module NestingApp =

    type Model = list<ClientViewportApp.Model>

    type Action = 
        | Change of int * ClientViewportApp.Action

    let update (env : Env<Action>) (model : Model) (a : Action) =
        match a with
            | Change(i,action) -> List.updateAt i (fun a -> ClientViewportApp.update (Env.map (fun a -> Change(i,a)) env) a action) model

    let view (model : Model) : DomNode<Action> =
        let inner = 
            model |> List.mapi (fun i e ->
                ClientViewportApp.view e |> Html.map (fun innerAction -> Change(i, innerAction))
            )
        div [] inner

    let app : App<_,_,_> = 
        {
            initial = [ ClientViewportApp.initial; ClientViewportApp.initial;ClientViewportApp.initial]
            update = update
            view = view
            subscriptions = Subscriptions.none
            onRendered = OnRendered.ignore
        }


module FileApp =
    
    type Model = list<string>

    type Message = Open | Accept | Deny 

    let update env (m : Model) (msg : Message) =
        match msg with
            | Open -> 
                use dialog = new System.Windows.Forms.OpenFileDialog()
                dialog.Multiselect <- true
                let r = dialog.ShowDialog()
                if r = DialogResult.OK then                    
                    (dialog.FileNames |> Array.toList) @ m
                else m
            | Accept -> m
            | Deny -> []

    let openModalButton =
        div [] [
            
            button [Callback( "onClick", "doit();")] [text "JD"]
        ]
    
    let modal (m) =
        div [clazz "ui modal"] [
            i [clazz "close icon"] []
            div [clazz "header"] [text "Modal Title"]
            div[clazz "image content"] [
                div [clazz "image"][text "image here"]
                div [clazz "description"][button [clazz "ui button"; onMouseClick (fun _ -> Open)][text "add files"]]
                div [][
                    for i in m do
                        yield sprintf "file: %s" i |> text
                ]
            ]
            div [clazz "actions"] [
                div [clazz "ui button deny"; onMouseClick (fun _ -> Deny)] [text "nope"]
                div [clazz "ui button positive"; onMouseClick (fun _ -> Accept)] [text "yes"]
            ]
        ]

    let view (m : Model) =
        div [] [
            yield modal m
            yield openModalButton           
        ]


    let app =
        {
            initial = []
            update = update
            view = view
            subscriptions = Subscriptions.none
            onRendered = OnRendered.ignore
        }

module Surfaces =
    
    type Model = 
        {
            currentlyLoaded : list<string>
            importer : FileApp.Model
        }
    type Msg = Import of list<string> | FileAppMsg of FileApp.Message

    let update env model msg =
        match msg with
            | Import imported -> 
                { importer = []; currentlyLoaded = model.currentlyLoaded @ imported }
            | FileAppMsg a -> { model with importer = FileApp.update env model.importer a }

    let view m =
        div [] [
            yield FileApp.view m.importer |> Html.map (fun a -> match a with | FileApp.Accept _ -> Import m.importer | a -> FileAppMsg a)
            for a in m.currentlyLoaded do yield text (sprintf "loaded: %s" a)
        ]


    let app =
        {
            initial = { currentlyLoaded = []; importer = [] }
            update = update
            view = view
            subscriptions = Subscriptions.none
            onRendered = OnRendered.ignore
        }


module Time =
     
    type Model = { current : DateTime; interval : TimeSpan }

    type Action = Tick of DateTime

    let update e model msg =
        printfn "clock with interval: %A ticked" model.interval.TotalSeconds
        match msg with
            | Tick t ->  { model with current = t }

    let subscriptions m = 
        Time.every m.interval Tick

    let (=>) a b = attribute a b

    let view (model : Model) : DomNode<Action> = 
        let angle = ((float model.current.Second + float model.current.Millisecond / 1000.0) / 60.0) * Math.PI * 2.0
        let handX = 50.0 + 40.0 * cos angle |> string
        let handY = 50.0 + 40.0 * sin angle |> string
    
        div [] [
            svg [ viewBox "0 0 100 100"; width "300px" ] [
                circle [ cx "50"; cy "50"; r "45"; fill "#0B79CE" ] []
                line [ "x1" =>  "50"; "y1" => "50"; "x2" => handX; "y2" => handY; "stroke" => "#023963" ] []
            ]
            //br []
            text (sprintf "refresh: %A" model.interval)
        ]

    let initial s = { current = DateTime.Now; interval = s }

    let app =  
        {
            initial = { current = DateTime.Now; interval = Time.seconds }
            update = update
            view = view
            subscriptions = subscriptions
            onRendered = OnRendered.ignore
        }

module SubscriptionNesting =

    type Model = list<Time.Model>

    type Action = 
        | Change of int * Time.Action

    let update (env : Env<Action>) (model : Model) (a : Action) =
        match a with
            | Change(i,action) -> List.updateAt i (fun a -> Time.update (Env.map (fun a -> Change(i,a)) env) a action) model

    let view (model : Model) : DomNode<Action> =
        let inner = 
            model |> List.mapi (fun i e ->
                Time.view e |> Html.map (fun innerAction -> Change(i, innerAction))
            )
        div [] inner

    let subscriptions (model : Model) =
        let subs = model |> List.mapi (fun i m -> Time.subscriptions m |> Sub.map (fun a -> Change(i,a)))
        Sub.aggregate subs

    let app : App<_,_,_> = 
        {
            initial = [ Time.initial (TimeSpan.FromSeconds 1.0); Time.initial  (TimeSpan.FromSeconds 0.1); Time.initial (TimeSpan.FromSeconds 2.0)]
            update = update
            view = view
            subscriptions = subscriptions
            onRendered = OnRendered.ignore
        }


[<EntryPoint;STAThread>]
let main argv =
    ChromiumUtilities.unpackCef()
    Chromium.init argv

    let app = PerformanceTest.app

    let m : V3dApp.Model = {
            components = [ Numeric.initial; Numeric.initial; Numeric.initial ]            
            }

    let app = NestingApp.app// V3dApp.app m
    //let app = SubscriptionNesting.app
    let runWindow = true        

    if runWindow then
        let browser = Chromium.runControl "8083" app
        use w = new Form()
        w.Controls.Add browser
        w.Width <- 800
        w.Height <- 600
        Application.Run(w) 
    else
        Fablish.RunLocally(app, "8083")

    0
