namespace Fablish

[<AutoOpen>]
module CommonTypes =

    open System

    type Sub<'msg> = 
        | NoSub
        | TimeSub of TimeSpan * (DateTime -> 'msg) 
        | TwoTime of Sub<'msg> * Sub<'msg>


    type Cmd<'msg> =
        | NoCmd
        | Cmd of Async<'msg>

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Cmd =
        let none = NoCmd

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Sub =
        let none = NoSub
        let combine l r = TwoTime(l,r)
        let rec map (f : 'a -> 'b) (s : Sub<'a>) : Sub<'b> =
            match s with
                | NoSub -> NoSub
                | TimeSub(ts,g) -> TimeSub(ts,fun t -> f (g t))
                | TwoTime(l,r) -> TwoTime(map f l, map f r)
        let rec extract (f : Sub<'msg>) : list<TimeSpan * (DateTime -> 'msg)> =
            match f with
                | NoSub -> []
                | TimeSub(ts,g) -> [ts,g]
                | TwoTime(l,r) -> extract l @ extract r
        let rec aggregate xs = xs |> List.fold combine NoSub


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Subscriptions =
        let none = fun _ -> Sub.none


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Time =

        let seconds = TimeSpan.FromSeconds 1.0
        let every (interval : TimeSpan) (f : DateTime -> 'msg) = TimeSub(interval,f)


    type Env<'msg> = { run : Cmd<'msg> -> unit }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Env =
        let map (f : 'b -> 'a) (a : Env<'a>) : Env<'b> =
            let run cmd =
                match cmd with
                    | Cmd.NoCmd -> ()
                    | Cmd.Cmd c -> 
                        async {
                            let! c = c
                            return f c
                        } |> Cmd.Cmd |> a.run
            { run = run }

    type Callback<'model,'msg> = 'model -> 'msg -> 'model