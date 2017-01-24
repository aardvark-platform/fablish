namespace Fablish


open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.Html

module SemanticUI =

    let openModalButton () =
        div [] [
            button [Callback( "onClick", "doit();")] [text "JD"]
        ]
    
    let modal m accept deny =
        div [clazz "ui modal"] [
            i [clazz "close icon"] []
            div [clazz "header"] [text "Modal Title"]
            div[clazz "image content"] [
                div [clazz "image"][text "image here"]
//                div [clazz "description"][button [clazz "ui button"; onMouseClick (fun _ -> Open)][text "add files"]]
//                div [][
//                    for i in m do
//                        yield sprintf "file: %s" i |> text
//                ]
            ]
            div [clazz "actions"] [
                div [clazz "ui button deny"; onMouseClick (fun _ -> accept)] [text "nope"]
                div [clazz "ui button positive"; onMouseClick (fun _ -> deny)] [text "yes"]
            ]
        ]