#I @"..\..\bin\Debug"
#r "Aardvark.Cef.dll"
#r "Xilium.CefGlue.dll"
#r "Xilium.CefGlue.WindowsForms.dll"
#r "Aardvark.Base.Essentials.dll"
#r "Aardvark.Base.dll"
#r "Aardvark.UI.Fablish.exe"


open System
open System.Windows.Forms

open Aardvark.Base
open Fablish
open System.Windows.Forms


module HelloWorld =
    open Fable.Helpers.Virtualdom
    open Fable.Helpers.Virtualdom.Html
    let container a c = div ([clazz "ui container"] @ a) c

    let ribbon() = 
        div [Style [ "border", "10px solid #000000"]] [
            div [clazz "ui top attached tabular menu"] [
                a [clazz "item active"; attribute "data-tab" "first"] [text "First"]
                a [clazz "item"; attribute "data-tab" "second"] [text "Second"]
                a [clazz "item"; attribute "data-tab" "third"] [text "Third"]
            ]
            div [clazz "ui bottom attached tab segment active"; attribute "data-tab" "first"] [text "First"] 
            div [clazz "ui bottom attached tab segment"; attribute "data-tab" "second"] [text "Second"] 
            div [clazz "ui bottom attached tab segment"; attribute "data-tab" "third"] [text "Third"] 
        ]


    type Model = SideBarApp.Model
    type Action = Inc | Dec
    // interactive module reloading fablish....

    let (=>) k v = attribute k v

    let canvas()  = elem "div" [ Style [ "border", "10px solid #000000"] ] []
    let update m a = SideBarApp.update m a
    let view m = 
        div [] [
            table [Style [ "width", "100%"]] [
                tbody[] [
                    tr [] [ td [Style [ "border", "1px solid black"]] [ribbon()]]
                    tr [] [ td [Style [ "border", "1px solid black"]] [                        
                                table [clazz "ui celled table unstackable small"; Style [ "width", "100%"]] [
                                    tbody[] [
                                        tr [] [
                                            td [Style [ "border", "1px solid black"]] [text "left"]
                                            td [clazz "collapsing"; Style [ "border", "1px solid black"]] [SideBarApp.view m]
                                        ]
                                    ]

                                ]
                            ]
                    ]
                ]
            ]            
            //clazz "ui celled table unstackable";


            
            // table [clazz "ui celled table"] [
            //     tbody[] [
            //         tr [] [
            //             td [] [canvas]
            //             td [clazz "collapsing"] [SideBarApp.view m]
            //         ]
            //     ]
            // ]                       
        ]
    let app = 
        {
            initial = SideBarApp.initial
            update = update
            view = view
        }

let wd = System.IO.Path.Combine(__SOURCE_DIRECTORY__,@"..\..\bin\Debug")
System.Environment.CurrentDirectory <- wd
Aardvark.Base.IntrospectionProperties.CustomEntryAssembly <- 
    System.Reflection.Assembly.LoadFile(System.IO.Path.Combine(wd, "Aardvark.UI.Fablish.exe"))

let browser = Chromium.runControl "8083" HelloWorld.app

let w = new Form()

w.Controls.Add browser
//w.Width <- 800
//w.Height <- 600
w.Show()

