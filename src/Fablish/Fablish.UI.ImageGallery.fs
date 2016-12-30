namespace Fablish

open System
open System.IO
open Suave

open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.Html

open DomHelpers

module GalleryApp =
    
    type Image = {
        Path             : string
     //   ContainingFolder : string
    }

    type Model = {
        Folders      : string[]
        Images       : Image[]
        CurrentImage : Image
    }

    type Action = 
        | Next 
        | Previous
        | Show of Image

    let update (model : Model) (action : Action) =
        match action with
            | Next      -> failwith ""
            | Previous  -> failwith ""
            | Show img  -> { model with CurrentImage = img }

    let getRandArrElement =
        let rnd = Random()
        fun (arr : Image[]) -> arr.[rnd.Next(arr.Length)]

    let view (model : Model) : DomNode<Action> =
        container [
            img [clazz "ui image"; attribute "src" model.CurrentImage.Path; onMouseClick (fun _ -> Show (model.Images |> getRandArrElement))]
        ]

    let app model = {
        initial = model
        update = update
        view = view
        onRendered = OnRendered.ignore
    }

    let rec visitor dir = 
        [ yield! Directory.GetFiles(dir)
          for subdir in Directory.GetDirectories(dir) do yield! visitor subdir ]

   

    let getImages dir =
        let files = visitor dir
        files 
            |> List.map (fun x -> { Path = x.[1..x.Length-1] }) 
            |> List.toArray
  
        