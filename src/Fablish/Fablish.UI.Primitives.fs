namespace Fablish

open System
open Suave

open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.Html

[<AutoOpen>]
module DomHelpers =
    let clazz v = attribute "className" v

    let container c = div [clazz "ui container"] c

    let labelled label c = 
        container [ div [clazz "ui label"] [ text label ]; c ]

    let accordion text' icon active content' =
        let title = if active then "title active" else "title"
        let content = if active then "content active" else "content"
        div [clazz "ui accordion fluid"] [
            div [clazz title] [
                    a [clazz "ui label large"] [
                        i [clazz (icon + " icon circular inverted")] []
                        text text'
                    ]
            ]
            div [clazz content] content'
        ]

module NumericApp = 
    open Aardvark.Base

    type Model = {
        value : float
        min   : float
        max   : float
        step  : float
    }

    type Action = 
        | Increment
        | Decrement
        | Set of string

    let initial = {
        value = 1.0
        min = -1.0
        max = 5.0
        step = 0.5
    }

    let update (model : Model) (action : Action) =
        match action with
            | Increment -> { model with value = min (model.value + model.step) model.max } // immutable record update syntax
            | Decrement -> { model with value = max (model.value - model.step) model.min }
            | Set s     ->
                let parsed = 0.0
                match Double.TryParse(s) with
                    | (true,v) -> { model with value = Fun.Clamp(v, model.min, model.max) }
                    | _ -> 
                        printfn "validation failed: %s" s
                        model    

    let view (model : Model) : DomNode<Action> =
            div [clazz "ui input"] [
                input [
                    Style ["text-align","right"]
                    attribute "value" (String.Format("{0:0.00}", model.value)) // custom number formatting
                    attribute "type" "text"; attribute "placeholder" "numeric";
                    attribute "size" "6"
                    onChange (fun s -> Set (unbox s))
                ]
                button [clazz "ui icon button"; onMouseClick (fun _ -> Increment)] [i [clazz "angle up icon"] []]
                button [clazz "ui icon button"; onMouseClick (fun _ -> Decrement)] [i [clazz "angle down icon"] []]
            ]                                  
    //<div class="ui input">
    //  <input type="text">
    //</div>

    let app = {
        initial = initial
        update = update
        view = view
        onRendered = Scripts.ignore
    }

module V3dApp = 
    open DomHelpers    

    type Model = {
        components : list<NumericApp.Model>        
    }   

    type Action = Change of int * NumericApp.Action

    let update (model : Model) (action : Action) =
        match action with
            | Change(index,action) -> 
                { model with components = List.updateAt index (fun x -> NumericApp.update x action) model.components}    

    let view (model : Model) : DomNode<Action> =
        let numericViews = 
            model.components |> List.mapi (fun i a -> NumericApp.view a |> Html.map (fun a -> Change(i, a)))
              
        div [] [
            for n in numericViews do 
                yield div [] [n]
        ]

    let app initial = {
            initial = initial
            view = view
            update = update
            onRendered = Scripts.ignore
    }

module TrafoApp = 
    open DomHelpers

    type Model = {
        vectors : list<V3dApp.Model>
    }

    type Action = Change of int * V3dApp.Action

    let initial = { vectors = List.init 3 (fun _ -> { components = List.init 3 (fun _ -> NumericApp.initial) })}

    let update (model : Model) (action : Action) =
        match action with
            | Change(index, action) ->
                { model with vectors = List.updateAt index (fun x -> V3dApp.update x action) model.vectors}

    let view (model : Model) : DomNode<Action> =
        let vectorViews =
            model.vectors |> List.mapi ( fun i a -> V3dApp.view a |> Html.map (fun a -> Change(i,a)))

        div[] [
            for n in vectorViews do
                yield div [] [n]
        ]

    let app = { initial = initial; view = view; update = update; onRendered = Scripts.ignore }

module V3dApp2 =
    open DomHelpers

    type Model = {
        x : NumericApp.Model
        y : NumericApp.Model
        z : NumericApp.Model
    }

    type Action = 
        | Set_X   of NumericApp.Action
        | Set_Y   of NumericApp.Action    
        | Set_Z   of NumericApp.Action

    let update (model : Model) (action : Action) =
        match action with
            | Set_X a -> { model with x = NumericApp.update model.x a }
            | Set_Y a -> { model with y = NumericApp.update model.y a }
            | Set_Z a -> { model with z = NumericApp.update model.z a }

    let view (model : Model) : DomNode<Action> =
        table [clazz "ui celled table"] [
                    tbody [] [
                        tr [] [
                            td [clazz "collapsing"] [text "X:"];
                            td [clazz "right aligned"] [NumericApp.view model.x |> Html.map Set_X]
                        ]
                        tr [] [
                            td [clazz "collapsing"] [text "Y:"];
                            td [clazz "right aligned"] [NumericApp.view model.y |> Html.map Set_Y]
                        ]
                        tr [] [
                            td [clazz "collapsing"] [text "Z:"];
                            td [clazz "right aligned"] [NumericApp.view model.z |> Html.map Set_Z]
                        ]
                    ]
        ]

module TrafoApp2 =
    open DomHelpers

    type Model = {
        translation : V3dApp2.Model
        rotation    : V3dApp2.Model
        scale       : V3dApp2.Model
    }

    type Action = 
        | Set_Translation   of V3dApp2.Action
        | Set_Rotation      of V3dApp2.Action    
        | Set_Scale         of V3dApp2.Action

    let update (model : Model) (action : Action) =
        match action with
            | Set_Translation a -> { model with translation = V3dApp2.update model.translation a }
            | Set_Rotation a -> { model with rotation = V3dApp2.update model.rotation a }
            | Set_Scale a -> { model with scale = V3dApp2.update model.scale a }

    let view (model : Model) : DomNode<Action> =
        table [clazz "ui celled table"] [
                    tbody [] [
                        tr [] [
                            td [clazz "collapsing"] [text "Translation:"];
                            td [clazz "right aligned"] [V3dApp2.view model.translation |> Html.map Set_Translation]
                        ]
                        tr [] [
                            td [clazz "collapsing"] [text "Rotation:"];
                            td [clazz "right aligned"] [V3dApp2.view model.rotation |> Html.map Set_Rotation]
                        ]
                        tr [] [
                            td [clazz "collapsing"] [text "Scale:"];
                            td [clazz "right aligned"] [V3dApp2.view model.scale |> Html.map Set_Scale]
                        ]
                    ]
        ]            

type Choice = {     
        choices     : list<string>
        selected    : string
    }

module ChoiceHelper = 
    open System
    open FSharp.Reflection
    let toString (x:'a) = 
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name

    let fromString<'a> (s:string) =
        match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
        | [|case|] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
        |_ -> None

    let toChoice<'a> (selected : 'a) : Choice =
        { choices = FSharpType.GetUnionCases typeof<'a> |> Array.map (fun a -> a.Name) |> Array.toList; selected = toString selected }

    let fromChoice<'a> (choice : Choice) : 'a Option = 
        fromString choice.selected
   


module TEst = 
    type Test = Nil  | Cons
    let a = ChoiceHelper.toChoice (Cons )
    let b : Test Option = ChoiceHelper.fromChoice a


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Choice = 
    open DomHelpers

    type Model = Choice

    type Action = Select of string

    let initial = {
        choices = ["Lamber"; "Phong"; "Oren"]
        selected = "Phong";
    }

    let update (model : Model) (action : Action) =
        match action with
            | Select x -> { model with selected = x }
  
    let view<'a when 'a : equality> (model : Model) : DomNode<Action> =
        let onChangeEvt o = o |> unbox |> Select
        
        select [clazz "ui dropdown"; attribute "defaultValue" model.selected; onChange onChangeEvt] [
            for case in model.choices do yield option [] [text (sprintf "%s" case)]
       ]  

module ToggleApp = 
    type Model = {
        active : bool
        label  : string
    }

    type Action = 
        | Toggle
    
    let update (model : Model) (action : Action) =
        match action with
            | Toggle -> { model with active = not model.active }

    let view (model : Model) : DomNode<Action> =         
        let active = if model.active then "ui checked toggle checkbox" else "ui toggle checkbox"
        let active' = if model.active then "checked" else ""
        div [clazz active] [
            input [attribute "type" "checkbox"; attribute "checked" active'; onMouseClick(fun _ -> Toggle)]
            label [] [text ""]
            
        ]
        //button ([clazz checked; onMouseClick (fun _ -> Toggle) ]) [text model.label]

//        <div class="ui left floated compact segment">
//  <div class="ui fitted toggle checkbox">
//    <input type="checkbox">
//    <label></label>
//  </div>
//</div>

    let app initial = {
            initial = initial
            view = view
            update = update
            onRendered = Scripts.ignore
    }

module ValueApp = 
    open DomHelpers

    type Value = 
        | TextInput of string
        | ComboBox of Choice.Model
        | NumericInput of NumericApp.Model
        //add toggleapp as boolean input
        //add vector app

    type Model = {
        value    : Value
        readonly : bool
    }
   
    type Action =
        | TextChange of string
        | ComboChange of Choice.Action
        | NumericChange of NumericApp.Action

    let update model action =
        match action, model.value with
            | TextChange a, TextInput _ -> { model with value = TextInput a }
            | ComboChange a, ComboBox m -> { model with value = ComboBox <| Choice.update m a }
            | NumericChange a, NumericInput m -> { model with value = NumericInput <| NumericApp.update m a }
            | _ -> failwith "property not supported"    

    let view (model : Model) : DomNode<Action> =
        container [
                match model.value with
                    | TextInput a -> 
                        yield input [clazz "ui input"; attribute "value" a; onChange (fun v -> v |> unbox |> TextChange)]
                    | ComboBox a -> yield Choice.view a |> Html.map ComboChange
                    | NumericInput a -> yield NumericApp.view a |> Html.map NumericChange
                ]

