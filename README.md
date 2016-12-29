# fablish
This library provides utilities for building Elm style [1] applications in .NET.
In contrast to fable [2], which uses a F# to JS compiler, this library runs directly in your favorite CLR. 
However, in order to stay somewhat compatible we reused fable-archs API but replaced the virtualdom backend
with a custom codegenerator which creates the HTML dom via react.

Note that this project is in very early stage of development. Still, help in building rich user interface libraries
is welcome ;)

[1] https://guide.elm-lang.org/architecture/
[2] http://fable.io/fable-arch/
[3] https://facebook.github.io/react/
