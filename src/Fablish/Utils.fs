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