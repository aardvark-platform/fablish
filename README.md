# fablish
This library provides utilities for building Elm style [1] applications in .NET.
In contrast to fable [2], which uses a F# to JS compiler, this library runs directly in your favorite CLR. 
However, in order to stay somewhat compatible we reused fable-archs API but replaced the virtualdom backend
for technickal reasons with a custom codegenerator which creates the HTML dom via react [3].

The overall architecture is as such:

## Write Elm Style Application using similar API to fable:

```F#
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
            button [onMouseClick (fun _ -> Inc)] [text "increment"]
            button [onMouseClick (fun _ -> Dec)] [text "decrement"]
        ]

    let app =
        {
            initial = 0
            update = update 
            view = view
            onRendered = Script.ignore
        }

```

## Since by default we use chromium for rendering UIs, initialize chromium:
```F#
    ChromiumUtilities.unpackCef()
    Chromium.init argv
```

## Run your app

Either use application setup to spawn a win forms window with embedded chrome and your UI:
```F#
let browser = Chromium.runControl "8083" app
use w = new Form()
w.Controls.Add browser
w.Width <- 800
w.Height <- 600
Application.Run(w) 
```

or use a standalone server:
```F#
Fablish.runLocally "8083" app
```

In both cases your application can be debugged using chrome debugging tools:

## Notes 

Note that this project is in very early stage of development. Still, help in building rich user interface libraries
is welcome ;)

 [1]: https://guide.elm-lang.org/architecture/
 
 [2]: http://fable.io/fable-arch/
 
 [3]: https://facebook.github.io/react/
