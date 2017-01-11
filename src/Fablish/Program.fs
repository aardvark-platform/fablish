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
            onRendered = OnRendered.ignore
        }


open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.Html

module TestApp =

    
    type Model = int

    type Action = Inc | Dec

    let update (m : Model) (a : Action) =
        printfn "[Test] computing udpate"
        match a with
            | Inc -> m + 1
            | Dec -> m - 1

    let view (m : Model) : DomNode<Action> =
        printfn "[Test] Computing view"
        div [] [
            text (sprintf "current content: %d" m)
            br []
            button [onMouseClick (fun dontCare -> Inc); attribute "id" "urdar"] [text "increment"]
            button [onMouseClick (fun dontCare -> Dec)] [text "decrement"]
        ]

    let onRendered model view =
        {
            clientSide = """() => { 
                var rect = document.getElementById("urdar").getBoundingClientRect();
                return { bottom : rect.bottom.toFixed(), height : rect.height.toFixed(), left : rect.left.toFixed(), right : rect.right.toFixed(), top : rect.top.toFixed(), width : rect.width.toFixed() }; 
            } """   
            serverSide = fun (s : string) -> printfn "clientRect: %A" (ClientRect.ofString s); None
        }

    let app =
        {
            initial = 0
            update = update 
            view = view
            onRendered = onRendered
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
            onRendered = OnRendered.ignore
        }


module FileApp =
    
    type Model = list<string>

    type Message = Open | Accept | Deny 

    let update (m : Model) (msg : Message) =
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
            onRendered = OnRendered.ignore
        }

module Surfaces =
    
    type Model = 
        {
            currentlyLoaded : list<string>
            importer : FileApp.Model
        }
    type Msg = Import of list<string> | FileAppMsg of FileApp.Message

    let update model msg =
        match msg with
            | Import imported -> 
                { importer = []; currentlyLoaded = model.currentlyLoaded @ imported }
            | FileAppMsg a -> { model with importer = FileApp.update model.importer a }

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

    let app = Surfaces.app
    let runWindow = true        

    if runWindow then
        let browser = Chromium.runControl "8083" TestApp.app
        use w = new Form()
        w.Controls.Add browser
        w.Width <- 800
        w.Height <- 600
        Application.Run(w) 
    else
        Fablish.runLocally "8083" app

    0
