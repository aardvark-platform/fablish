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
module JavascriptInterop = 
    // {"bottom":44,"height":25,"left":0,"right":78.734375,"top":19,"width":78.734375}"
    type ClientRect = { 
        bottom : float
        height : float
        left   : float
        right  : float
        top    : float
        width  : float
    }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ClientRect =
        let ofString (s : string) : ClientRect = Pickler.json.UnPickleOfString s
        let toString (c : ClientRect) = Pickler.json.PickleToString c
        let empty = { bottom = 0.0; height = 0.0; left = 0.0; right = 0.0; top = 0.0; width = 0.0 }
