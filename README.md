[![Build Status](https://travis-ci.org/aardvark-platform/fablish.svg?branch=master)](https://travis-ci.org/aardvark-platform/fablish)
[![Build status](https://ci.appveyor.com/api/projects/status/caihtu3i3697i6o2/branch/master?svg=true)](https://ci.appveyor.com/project/haraldsteinlechner/fablish/branch/master)

[Wiki](https://github.com/aardvark-platform/aardvark.docs/wiki) | 
[Gallery](https://github.com/aardvarkplatform/aardvark.docs/wiki/Gallery) | 
[Quickstart](https://github.com/aardvarkplatform/aardvark.docs/wiki/Quickstart-Windows) | 
[Status](https://github.com/aardvarkplatform/aardvark.docs/wiki/Status)

**Fablish** is part of the open-source [Aardvark platform](https://github.com/aardvark-platform/aardvark.docs/wiki) for visual computing, real-time graphics and visualization. 

Fablish is the **predecessor** of [aardvark.media](https://github.com/aardvark-platform/aardvark.media) and it demonstrates the basic concepts of functional user interfaces creation.

# Fablish
This library provides utilities for building Elm-style [[1]] applications in .NET.
In contrast to Fable [[2]], which uses a F# to JS compiler, this library runs directly in your favorite CLR. 
To stay compatible, we reused Fable-arch's API and replaced the virtualdom backend
with a custom code generator, which creates the HTML DOM via React [[3]].

## When to use

Fablish is useful if your use case looks like:
 - you want purely functional user interfaces
 - you want to profit from web technology and tooling
 - you want solid technology for rendering your ui - browsers are superfast ;)
 - you still want native .net programs since your application state cannot be run in the broser alone (e.g. you are rendering several GBs of data, as we do in other projects, based on the Aardvark rendering platform [[10]].

## Writing an Elm-style Application using an API similar to Fable:

```F#
module TestApp =

    type Model = int

    type Action = Inc | Dec

    let update (env : Env<Action>) (m : Model) (a : Action) =
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
            subscriptions = Subscriptions.none
        }

```

## Architecture

The overall architecture is:
 - Elm architecture similar to fable-arch [[8]]
 - Fable-style API for building views
 - JavaScript code generator for building React DOM
 - WebSocket via Suave [[7]] provides DOM to static webpage
 - the webpage utilizes WebSocket and React to update and render the DOM
 - the overall application can either be fully hosted inside a browser, or by using a chromium window (which is included).
 
Differences to Fable:
  - altough the signature is very similar to fable-arch (in fact i copied the Fable.Helpers.Virtualdom and changed some bits), some features of fable-arch are implemented differently (mostly for historical reasons). Still, fablish should feel quite similar to fable-arch and learning fablish is therefore much easier,
 
Differences to elm:
 - instead of returning commands, in fablish commands can be pushed into a environment which is passed into the update function. The effects of this change are not totally clear yet, however for the moment it works quite good.

## Since by default we use Chromium for rendering UIs, we have to initialize Chromium:
```F#
ChromiumUtilities.unpackCef() // downloads CEF build if not already present
Chromium.init argv            // initialize CEF runtime (mind argv being the executable arguments as usual)
```

## Run your application

Either use application setup to spawn a window with embedded Chrome and your UI:
```F#
let browser = Chromium.runControl "8083" app // start websocket hosting app on port 8083, i.e. UI is available at localhost:8083/mainPage and can be debugged by using chrome on localhost:1337
use w = new Form()
w.Controls.Add browser
w.Width <- 800
w.Height <- 600
Application.Run(w) 
```

or use a stand-alone server:
```F#
Fablish.runLocally "8083" app // run websocket on port 8083 and serve website requests on localhost:8083 while a debug web page is available for chrome on localhost:1337
```

In both cases your application can be debugged using Chrome debugging tools:

## Building

- run ``build.cmd`` or ``build.sh`` (F# 4.0 [[9]] and .net or mono needs to be installed)
- you can also use the prebuilt package currently available here: https://vrvis.myget.org/feed/aardvark_public/package/nuget/Fablish

## Debugging

![alt text](docs/teaser.png)

## Interactive development

Fablish-hmr [[4]] provides webpack-style [[5]] interactive programming by utilizing F# compiler services.
The following video shows off this feature: [youtube video on hmr](https://www.youtube.com/watch?v=if5Natbyx0s&feature=youtu.be)

 [1]: https://guide.elm-lang.org/architecture/
 
 [2]: http://fable.io/fable-arch/
 
 [3]: https://facebook.github.io/react/
 
 [4]: https://github.com/vrvis/fablish/tree/master/src/fablish-hmr
 
 [5]: https://webpack.github.io/

 [6]: https://www.youtube.com/watch?v=if5Natbyx0s&feature=youtu.be

 [7]: https://suave.io/

 [8]: http://fable.io/fable-arch/
 
 [9]: https://www.microsoft.com/en-us/download/details.aspx?id=48179
 
 [10]: https://github.com/vrvis/aardvark.rendering
