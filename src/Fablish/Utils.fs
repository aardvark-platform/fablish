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

    [<AutoOpen>]
    module Ids =
        open System.Threading

        type Id = int
        type ID() =
            let mutable currentId = 0
            member x.New () =
                Interlocked.Increment(&currentId)
            member x.All = [ 0 .. currentId ]

    [<AutoOpen>]
    module NetExtensions =
        open System
        open System.Text
        open System.Net
        open Suave.Sockets
        open Suave.Sockets.Control

        let getBytes (s : String)  = 
            Encoding.UTF8.GetBytes s

        let getByteSegment (s : String)  = 
            ByteSegment(Encoding.UTF8.GetBytes s)

        let getString (b : byte[]) = Encoding.UTF8.GetString b

        type SocketMonad with
            member this.Bind(x : Async<'a>, f : 'a -> SocketOp<'b>) : SocketOp<'b> = 
                async {
                    let! result = x
                    return! f result
                }

module EmbeddedResources =
    
    open System
    open System.IO

    let extractPage page = 
        let plainName = Path.GetFileName page
        let c = Console.BackgroundColor
        Console.ForegroundColor <- ConsoleColor.Green
        printfn "[fablish] trying to serve: %s but the file could not be found in the home directory (typically ./static/index.html). Trying to use default index.html from fablish build (using embedded resource)." plainName
        let info = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream plainName
        if isNull info then
            failwithf "[fablish] extractPage could not extract embedded resource of name: %s" plainName
        else
            use s = new System.IO.StreamReader(info)
            Console.ForegroundColor <- c
            s.ReadToEnd()

[<AutoOpen>]
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

        static member Choice2(a : Async<'a>, b : Async<'b>) : Async<Option<Choice<'a,'b>>> =   
            let a' = async { let! a = a in return Some (Choice1Of2 a) }
            let b' = async { let! b= b in  return Some (Choice2Of2 b) }
            Async.Choice [| a'; b' |]