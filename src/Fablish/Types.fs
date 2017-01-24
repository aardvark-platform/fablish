namespace Fablish

open System
open System.Collections.Generic
open Aardvark.Base
open Aardvark.Base.Incremental
    
[<AutoOpen>]
module Tokens =
    let magic = "This is Fablish"

    [<Literal>]
    let eventOccurance  = 1
    [<Literal>]
    let forceRendering = 2

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
type Script = 
    | JsLambda of string
    | Ignore

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Scripts =
    let ignore = Ignore

type RenderCallback<'msg> = string -> Option<'msg>
type Reaction<'msg> = { clientSide : Script; serverSide : RenderCallback<'msg>}
type OnRendered<'model,'msg,'view> = 'model -> 'view -> Reaction<'msg>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OnRendered =
    let ignore : 'model -> 'view -> Reaction<'msg> = fun _ _ -> { clientSide = Scripts.ignore; serverSide = fun _ -> None }


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


type FablishInstance<'model,'msg>(m : 'model, env : Option<Env<'msg>>, update : Env<'msg> -> 'model -> 'msg -> 'model) as this =
    let viewers = HashSet<MVar<'model>>()
    let modelSubscriptions = Dictionary<_,_>()
    let messageSubscriptions = Dictionary<_,_>()
    let model = Mod.init m
    let currentTimer = new Dictionary<TimeSpan,ref<list<DateTime -> 'msg> * list<IDisposable>> * System.Timers.Timer>()

    let emit cmd = 
        match cmd with
            | NoCmd -> ()
            | Cmd cmd -> 
                async {
                    let! msg = cmd
                    this.EmitMessage msg
                } |> Async.Start

    let env =
        match env with
            | None -> { run = emit }
            | Some e -> e

    member x.Env = env

    member x.EmitModel newModel =
        lock viewers (fun _ ->
            if System.Object.ReferenceEquals(newModel,model.Value) || Unchecked.equals newModel model.Value then model.Value
            else
                model.Value <- newModel
                for sub in modelSubscriptions.Values do
                    sub newModel

                for v in viewers do
                    MVar.put v newModel

                model.Value
        )

    member x.Run(f : unit -> 'b) = lock viewers (fun _ -> f ())

    member x.AddViewer m =
        lock viewers (fun _ -> 
            viewers.Add m |> ignore
            m.Put model.Value
        )

    member x.EmitMessage msg =
        lock viewers (fun _ -> 
            let newModel = update env model.Value msg
            model.Value <- newModel
            for sub in messageSubscriptions.Values do
                sub msg

            for v in viewers do
                MVar.put v newModel
        )

    member x.SubscribeModel(f : 'model -> unit) =
        let d = { new IDisposable with member x.Dispose() = lock viewers (fun _ -> modelSubscriptions.Remove x |> ignore) }
        lock viewers (fun _ -> 
            modelSubscriptions.Add(d,f) |> ignore
        )
        d

    member x.SubscribeMessage(f : 'msg -> unit) =
        let d = { new IDisposable with member x.Dispose() = lock viewers (fun _ -> messageSubscriptions.Remove x |> ignore) }
        lock viewers (fun _ -> 
            messageSubscriptions.Add(d,f) |> ignore
        )
        d

    member x.UnsafeCurrentModel = model.Value

    member x.SendSubs (xs : list<TimeSpan * (DateTime -> 'msg)>) =
        lock viewers (fun _ -> 
            let newSubs = xs |> List.groupBy fst |> List.map (fun (k,xs) -> k,List.map snd xs) |> Dictionary.ofList
            let toRemove = List<_>()
            for (KeyValue(k,(r,timer))) in currentTimer do
                match newSubs.TryGetValue k with
                    | (true,v) -> 
                        let oldSubs = snd !r
                        oldSubs |> List.iter (fun a -> a.Dispose())
                        r := (v, v |> List.map (fun a -> timer.Elapsed.Subscribe(fun _ -> x.EmitMessage (a DateTime.Now))))
                    | _ -> 
                        !r |> snd |> List.iter (fun a -> a.Dispose())
                        timer.Dispose()
                        toRemove.Add(k)
            
            for (KeyValue(t,actions)) in newSubs do
                match currentTimer.TryGetValue t with
                    | (true,v) -> ()
                    | _ -> 
                        // new
                        let timer = new System.Timers.Timer()
                        timer.Interval <- t.TotalMilliseconds
                        let disps = actions |> List.map (fun a -> timer.Elapsed.Subscribe(fun _ -> x.EmitMessage (a DateTime.Now)))
                        timer.Start()
                        currentTimer.Add(t,(ref (actions,disps), timer))
            
            for i in toRemove do currentTimer.Remove i |> ignore
            Log.diagnostic "currently active timers: %A" currentTimer.Count
        )


    member x.Dispose() =
        for (KeyValue(k,(r,timer))) in currentTimer do
            !r |> snd |> List.iter (fun a -> a.Dispose())
            timer.Stop()
            timer.Dispose()
        Log.info "closed timers"

    interface IDisposable with
        member x.Dispose() = x.Dispose()

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
type App<'model,'msg,'view> = 
    {
        initial       : 'model
        update        :  Env<'msg> -> 'model    -> 'msg -> 'model
        view          : 'model     -> 'view
        subscriptions : 'model     ->  Sub<'msg>

        // IO extensions
        onRendered : OnRendered<'model,'msg,'view>
    }


type Event = { eventId : string; eventValue : string }
type Message = { id : int; data : Event }
type RenderRequest = { dom : string; script : string; id : string }

type Registrations<'msg> = Map<int,(obj->'msg)>