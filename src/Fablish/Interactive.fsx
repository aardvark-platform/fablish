#I @"..\..\bin\Debug"
#r "Fablish.exe"


open System
open System.Windows.Forms

open Fablish
open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.Html

module HelloWorld =

    type Model = int

    type Action = Inc | Dec

    let update (m : Model) (a : Action) =
        match a with
            | Inc -> m + 1
            | Dec -> m - 1

    let view (m : Model) : DomNode<Action> =
        div [] [
            text (sprintf "curre aaaant content: %d" m)
            br []
            button [onMouseClick (fun dontCare -> Inc)] [text "increment"]
            button [onMouseClick (fun dontCare -> Dec)] [text "decrement"]
        ]

    let app =
        {
            initial = 0
            update = update 
            view = view
        }

let browser = Chromium.runControl "8083" HelloWorld.app
let w = new Form()
w.Controls.Add browser
w.Width <- 800
w.Height <- 600
w.Show()

