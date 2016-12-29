module Program

open System
open Fablish

open System.Windows.Forms



module PerformanceTest = 

    open Fable.Helpers.Virtualdom
    open Fable.Helpers.Virtualdom.Html
     
     type Model = int

     type Action = Inc | Dec

     let update (m : Model) (a : Action) =
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
            onRendered = Scripts.ignore
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

    let update m _ = m


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
            onRendered = Scripts.ignore
        }


open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.Html

module TestApp =

    
    type Model = int

    type Action = Inc | Dec

    let update (m : Model) (a : Action) =
        match a with
            | Inc -> m + 1
            | Dec -> m - 1

    let view (m : Model) : DomNode<Action> =
        div [] [
            text (sprintf "current content: %d" m)
            br []
            button [onMouseClick (fun dontCare -> Inc); attribute "id" "urdar"] [text "increment"]
            button [onMouseClick (fun dontCare -> Dec)] [text "decrement"]
        ]

    let app =
        {
            initial = 0
            update = update 
            view = view
            onRendered = fun _ _ -> """() => { 
                var rect = document.getElementById("urdar").getBoundingClientRect();
                return { bottom : rect.bottom, height : rect.height, left : rect.left, right : rect.right, top : rect.top, width : rect.width }; 
            } """
        }


module ManyTestThings =

    type Model = list<TestApp.Model>

    type Action = 
        | Change of int * TestApp.Action

    let update (model : Model) (a : Action) =
        match a with
            | Change(i,action) -> List.updateAt i (fun a -> TestApp.update a action) model

    let view (model : Model) : DomNode<Action> =
        let inner = 
            model |> List.mapi (fun i e ->
                TestApp.view e |> Html.map (fun innerAction -> Change(i, innerAction))
            )
        div [] inner

    let app = 
        {
            initial = [ 1; 2; 3 ]
            update = update
            view = view
            onRendered = Scripts.ignore
        }
    

[<EntryPoint;STAThread>]
let main argv =
    ChromiumUtilities.unpackCef()
    Chromium.init argv

    let app = PerformanceTest.app

    let m : V3dApp.Model = {
            components = [ NumericApp.initial; NumericApp.initial; NumericApp.initial ]            
            }

    let app = TestApp.app
    let runWindow = true        

    if runWindow then
        let browser = Chromium.runControl "8083" app
        use w = new Form()
        w.Controls.Add browser
        w.Width <- 800
        w.Height <- 600
        Application.Run(w) 
    else
        Fablish.runLocally "8083" app

    0
