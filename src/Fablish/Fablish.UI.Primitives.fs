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

type Toggle = {
    isActive : bool        
}

type Text = {
    content : string
}

type Numeric = {
    value : float
    min   : float
    max   : float
    step  : float
}

type Choice = {     
    choices     : list<string>
    selected    : string
}

type Vector3d = {
    x : Numeric
    y : Numeric
    z : Numeric
}

type Transformation = {
    translation : Vector3d
    rotation    : Vector3d
    scale       : Vector3d
}


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Text = 
    type Model = Text

    type Action = 
        | Change of string

    let update (model : Model) (action : Action) =
        match action with
            | Change x -> { model with content = x }

    let view (model : Model) : DomNode<Action> =
        div [clazz "ui input"] [
            input [
                //Style ["text-align","right"]
                attribute "value" model.content
                attribute "type" "text"; //attribute "placeholder" "numeric";
                //attribute "size" "6"
                onChange (fun s -> Change (unbox s))
            ]
    ]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Numeric = 
    open Aardvark.Base

    let numeric f =
        { value = f; min = Double.MinValue; max = Double.MaxValue; step = 0.1 }

    type Model = Numeric

    type Action = 
        | Increment
        | Decrement
        | Set of string

    let initial = {
        value   = 1.0
        min     = -1.0
        max     = 5.0
        step    = 0.5
    }

    let update env (model : Model) (action : Action) =
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
        subscriptions = Subscriptions.none
        onRendered = OnRendered.ignore
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vector3d =
    open DomHelpers

    type Model = Vector3d

    type Action = 
        | Set_X   of Numeric.Action
        | Set_Y   of Numeric.Action    
        | Set_Z   of Numeric.Action

    let update env (model : Model) (action : Action) =
        match action with
            | Set_X a -> { model with x = Numeric.update env model.x a }
            | Set_Y a -> { model with y = Numeric.update env model.y a }
            | Set_Z a -> { model with z = Numeric.update env model.z a }

    let view (model : Model) : DomNode<Action> =
        table [clazz "ui celled table"] [
                    tbody [] [
                        tr [] [
                            td [clazz "collapsing"] [text "X:"];
                            td [clazz "right aligned"] [Numeric.view model.x |> Html.map Set_X]
                        ]
                        tr [] [
                            td [clazz "collapsing"] [text "Y:"];
                            td [clazz "right aligned"] [Numeric.view model.y |> Html.map Set_Y]
                        ]
                        tr [] [
                            td [clazz "collapsing"] [text "Z:"];
                            td [clazz "right aligned"] [Numeric.view model.z |> Html.map Set_Z]
                        ]
                    ]
        ]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Transformation =
    open DomHelpers

    type Model = {
        translation : Vector3d.Model
        rotation    : Vector3d.Model
        scale       : Vector3d.Model
    }

    type Action = 
        | Set_Translation   of Vector3d.Action
        | Set_Rotation      of Vector3d.Action    
        | Set_Scale         of Vector3d.Action

    let update env (model : Model) (action : Action) =
        match action with
            | Set_Translation a -> { model with translation = Vector3d.update env model.translation a }
            | Set_Rotation a -> { model with rotation = Vector3d.update env model.rotation a }
            | Set_Scale a -> { model with scale = Vector3d.update env model.scale a }

    let view (model : Model) : DomNode<Action> =
        table [clazz "ui celled table"] [
                    tbody [] [
                        tr [] [
                            td [clazz "collapsing"] [text "Translation:"];
                            td [clazz "right aligned"] [Vector3d.view model.translation |> Html.map Set_Translation]
                        ]
                        tr [] [
                            td [clazz "collapsing"] [text "Rotation:"];
                            td [clazz "right aligned"] [Vector3d.view model.rotation |> Html.map Set_Rotation]
                        ]
                        tr [] [
                            td [clazz "collapsing"] [text "Scale:"];
                            td [clazz "right aligned"] [Vector3d.view model.scale |> Html.map Set_Scale]
                        ]
                    ]
        ]            

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

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Toggle = 
    type Model = Toggle

    type Action = 
        | Toggle
    
    let update (model : Model) (action : Action) =
        match action with
            | Toggle -> { model with isActive = not model.isActive }

    let view (model : Model) : DomNode<Action> =         
        let active = if model.isActive then "ui defaultChecked toggle checkbox" else "ui toggle checkbox"
        let active' = if model.isActive then "checked" else ""
        div [clazz active] [
            input [attribute "type" "checkbox"; attribute "defaultChecked" active'; onMouseClick(fun _ -> Toggle)]
            label [] [text ""]
            
        ]        

module ValueApp = 
    open DomHelpers

    type Value = 
        | TextInput of string
        | ComboBox of Choice.Model
        | NumericInput of Numeric.Model
        //add toggleapp as boolean input
        //add vector app

    type Model = {
        value    : Value
        readonly : bool
    }
   
    type Action =
        | TextChange of string
        | ComboChange of Choice.Action
        | NumericChange of Numeric.Action

    let update env model action =
        match action, model.value with
            | TextChange a, TextInput _ -> { model with value = TextInput a }
            | ComboChange a, ComboBox m -> { model with value = ComboBox <| Choice.update m a }
            | NumericChange a, NumericInput m -> { model with value = NumericInput <| Numeric.update env m a }
            | _ -> failwith "property not supported"    

    let view (model : Model) : DomNode<Action> =
        container [
                match model.value with
                    | TextInput a -> 
                        yield input [clazz "ui input"; attribute "value" a; onChange (fun v -> v |> unbox |> TextChange)]
                    | ComboBox a -> yield Choice.view a |> Html.map ComboChange
                    | NumericInput a -> yield Numeric.view a |> Html.map NumericChange
                ]

module V3dApp = 
    open DomHelpers    

    type Model = {
        components : list<Numeric.Model>        
    }   

    type Action = Change of int * Numeric.Action

    let update env (model : Model) (action : Action) =
        match action with
            | Change(index,action) -> 
                { model with components = List.updateAt index (fun x -> Numeric.update env x action) model.components}    

    let view (model : Model) : DomNode<Action> =
        let numericViews = 
            model.components |> List.mapi (fun i a -> Numeric.view a |> Html.map (fun a -> Change(i, a)))
              
        div [] [
            for n in numericViews do 
                yield div [] [n]
        ]

    let initial = { components = [ Numeric.initial; Numeric.initial; Numeric.initial ] }

    let app initial = {
            initial = initial
            view = view
            update = update
            subscriptions = Subscriptions.none
            onRendered = OnRendered.ignore
    }

module TrafoApp = 
    open DomHelpers

    type Model = {
        vectors : list<V3dApp.Model>
    }

    type Action = Change of int * V3dApp.Action

    let initial = { vectors = List.init 3 (fun _ -> { components = List.init 3 (fun _ -> Numeric.initial) })}

    let update env (model : Model) (action : Action) =
        match action with
            | Change(index, action) ->
                { model with vectors = List.updateAt index (fun x -> V3dApp.update env x action) model.vectors}

    let view (model : Model) : DomNode<Action> =
        let vectorViews =
            model.vectors |> List.mapi ( fun i a -> V3dApp.view a |> Html.map (fun a -> Change(i,a)))

        div[] [
            for n in vectorViews do
                yield div [] [n]
        ]

    let app = { initial = initial; view = view; update = update; onRendered = OnRendered.ignore; subscriptions = Subscriptions.none }