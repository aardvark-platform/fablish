module Program

open System
open System.Windows.Forms

open Fablish
open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.Html

module ClientViewportApp =

    type Model = { number : int; progress : Option<int> }

    type Action = Inc | Dec | StartExpensive | ReceiveExpensive of int | Progress of int

    let update (env : Env<Action>) (m : Model) (a : Action) =
        match a with
            | Inc -> { m with number = m.number + 1 }
            | Dec -> { m with number = m.number - 1 }
            | StartExpensive -> 
                async { 
                    for i in 0 .. 100 do
                        async { return Progress i } |> Cmd |> env.run
                        let! r = Async.Sleep 10
                        printfn "[Env example] computing: %d/100" i
                    return ReceiveExpensive 1
                } |> Cmd |> env.run
                m
            | Progress a -> 
                { m with progress = Some a }
            | ReceiveExpensive a -> 
                { m with number = a; progress = None }

    let view (m : Model) : DomNode<Action> =
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
            onRendered = onRendered 
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


module Time =
     
    type Model = { current : DateTime; interval : TimeSpan }

    type Action = Tick of DateTime

    let update e model msg =
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

module ChoiceTest =

    type Model = Choice.Model 

    type Action = Set of string | Inner of Choice.Action

    let update (e : Env<_>)(m : Model)  (msg : Action) =
        printfn "%A" msg
        match msg with
            | Set s -> { m with selected = s }
            | Inner a -> Choice.update (Env.map Inner e) m a

    let view model =
        div [] [
            input [onChange (fun s -> Set (unbox s))] 
            Choice.view model |> Html.map Inner
        ]

    let app : App<_,_,_> = 
        {
            initial = Choice.initial
            update = update
            view = view
            subscriptions = Subscriptions.none
            onRendered = OnRendered.ignore
        }

module ToggleTest =

    type Model = Toggle.Model 

    type Action = Toggle | Inner of Toggle.Action

    let update (e : Env<_>)(m : Model)  (msg : Action) =
        printfn "%A" msg
        match msg with
            | Toggle -> { m with isActive = not m.isActive }
            | Inner a -> Toggle.update (Env.map Inner e) m a

    let view model =
        div [] [
            input [onChange (fun s -> Toggle)] 
            Toggle.view model |> Html.map Inner
        ]

    let app : App<_,_,_> = 
        {
            initial = Toggle.initial
            update = update
            view = view
            subscriptions = Subscriptions.none
            onRendered = OnRendered.ignore
        }



module SimpleDrawingApp =

    open Aardvark.Base

    type Polygon = list<V3d>

    type OpenPolygon = {
        cursor         : Option<V3d>
        finishedPoints : list<V3d>
    }
    
    type Model = {
        finished : list<Polygon>
        working  : Option<OpenPolygon>
    }


    type Action =
        | ClosePolygon
        | AddPoint   of V3d
        | MoveCursor of V3d
        |Nop

    let update e (m : Model) (cmd : Action) =
        match cmd with
            | ClosePolygon -> 
                match m.working with
                    | None -> m
                    | Some p -> 
                        { m with 
                            working = None 
                            finished = p.finishedPoints :: m.finished
                        }
            | AddPoint p ->
                match m.working with
                    | None -> { m with working = Some { finishedPoints = [ p ]; cursor = None;  }}
                    | Some v -> 
                        { m with working = Some { v with finishedPoints = m.working.Value.cursor.Value :: v.finishedPoints }}
            | MoveCursor p ->
                match m.working with
                    | None -> { m with working = Some { finishedPoints = []; cursor = Some p }}
                    | Some v -> { m with working = Some { v with cursor = Some p }}
            | Nop -> m


    let (=>) a b = attribute a b
    let viewPolygon (p : list<V3d>) =
        [ for edge in Polygon3d(p |> List.toSeq).EdgeLines do
             yield line [ "x1" => string edge.P0.X; "y1" => string edge.P0.Y; "x2" => string edge.P1.X; "y2" =>  string edge.P1.Y; "stroke" => "#023963" ] []
        ] 

    let onClick = 
        """function(ev) { 
            var e = ev.target;
            var dim = e.getBoundingClientRect();
            var x = ev.clientX - dim.left;
            var y = ev.clientY - dim.top;
            return { x : ev.clientX, y : ev.clientY };
        }"""

    type V2 = { x : int; y : int }

    let readClick (str : string)   =
        let v : V2 = Pickler.json.UnPickleOfString str
        V3d(v.x,v.y,0)

    let view (m : Model) = 
        svg [ width "640px"; height "480px" 
              ClientEvent("onClick", onClick, readClick >> AddPoint)
              ClientEvent("onMouseMove", onClick, readClick >> MoveCursor)
              onDblClick (fun _ -> ClosePolygon)
              Callback("onContextMenu","return false;") ] [

            for p in m.finished do yield! viewPolygon p

            match m.working with
                | Some p when p.cursor.IsSome && p.finishedPoints |> List.isEmpty |> not -> 
                    yield! viewPolygon (p.cursor.Value :: p.finishedPoints)
                | _ -> ()
        ]


    let initial = { finished = [[V3d.OOO;V3d.IOO*50.0;V3d.IIO*50.0;V3d.OOO]]; working = None;  }

    let app =
        {
            initial = initial
            update = update
            view = view 
            subscriptions = Subscriptions.none
            onRendered = OnRendered.ignore
        }

[<EntryPoint;STAThread>]
let main argv =
    ChromiumUtilities.unpackCef()
    Chromium.init argv

    // set verbosity (currently we have diagnostic and info where info is minimal info and diagnostic is rather chatty)
    //Fablish.Utils.Log.verbosity <- Fablish.Utils.Verbosity.Diagnostic

    let app = PerformanceTest.app

    // this one demonstrates asynchronous messages (Env and Env.map) as well as html renderer feedback (onRendered) in order to use bounds of a html element.
    //let app = NestingApp.app
    
    // this one demonstrates nesting and composition
    //let app = V3dApp.app V3dApp.initial
    
    // This one demonstrates nested subscriptions (Sub.map and app subscriptions for subscriptions to external events)
    //let app = SubscriptionNesting.app

    let app = SimpleDrawingApp.app
    //let app = ToggleTest.app

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
