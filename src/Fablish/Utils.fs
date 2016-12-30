namespace Fablish

[<AutoOpen>]
module Utils =

    module Pickler = 
        open MBrace.FsPickler
        open MBrace.FsPickler.Json

        let binary = FsPickler.CreateBinarySerializer()
        let json = FsPickler.CreateJsonSerializer(false, true)
    
        let ctx =
            System.Runtime.Serialization.StreamingContext()


        let init() =
            let t0 : list<int> = [1;2;3] |> binary.Pickle |> binary.UnPickle
            let t1 : list<int> = [1;2;3] |> json.PickleToString |> json.UnPickleOfString
            if t0 <> t1 then
                failwith "[CEF] could not initialize picklers"


    module List =
        let rec updateAt (i : int) (f : 'a -> 'a) (xs : list<'a>) = 
            match xs with
                | x::xs -> 
                    if i = 0 then (f x) :: xs
                    else x :: (updateAt (i-1) f xs)
                | [] -> []

module AsyncExtensions = 

    open System
    open System.Threading
    open System.Threading.Tasks

    type Async with
        static member Choice(tasks : Async<'T option> seq) : Async<'T option> = async {
            match Seq.toArray tasks with
            | [||] -> return None
            | [|t|] -> return! t
            | tasks ->

            let! t = Async.CancellationToken
            return! Async.FromContinuations <|
                fun (sc,ec,cc) ->
                    let noneCount = ref 0
                    let exnCount = ref 0
                    let innerCts = CancellationTokenSource.CreateLinkedTokenSource t

                    let scont (result : 'T option) =
                        match result with
                        | Some _ when Interlocked.Increment exnCount = 1 -> innerCts.Cancel() ; sc result
                        | None when Interlocked.Increment noneCount = tasks.Length -> sc None
                        | _ -> ()

                    let econt (exn : exn) =
                        if Interlocked.Increment exnCount = 1 then 
                            innerCts.Cancel() ; ec exn

                    let ccont (exn : OperationCanceledException) =
                        if Interlocked.Increment exnCount = 1 then
                            innerCts.Cancel(); cc exn

                    for task in tasks do
                        ignore <| Task.Factory.StartNew(fun () -> Async.StartWithContinuations(task, scont, econt, ccont, innerCts.Token))
        }