namespace Fablish
    
[<AutoOpen>]
module Tokens =
    let magic = "This is Aardvark"

    [<Literal>]
    let eventOccurance  = 1
    [<Literal>]
    let forceRendering = 2

type Script = 
    | JsLambda of string
    | Ignore

module Scripts =
    let ignore = Ignore

type RenderCallback<'msg> = string -> Option<'msg>
type Reaction<'msg> = { clientSide : Script; serverSide : RenderCallback<'msg>}
type OnRendered<'model,'msg,'view> = 'model -> 'view -> Reaction<'msg>

module OnRendered =
    let ignore : 'model -> 'view -> Reaction<'msg> = fun _ _ -> { clientSide = Scripts.ignore; serverSide = fun _ -> None }

type App<'model,'msg,'view> = 
    {
        // Elm interface
        initial  : 'model
        update   : 'model -> 'msg -> 'model
        view     : 'model -> 'view

        // IO extensions
        onRendered : OnRendered<'model,'msg,'view>
    }


type Event = { eventId : string; eventValue : string }
type Message = { id : int; data : Event }
type RenderRequest = { dom : string; script : string; id : string }

type Registrations<'msg> = Map<int,(obj->'msg)>