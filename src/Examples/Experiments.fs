module Examples

module RadialDiagram =

    open Aardvark.Base
    open Fablish
    open Fable.Helpers.Virtualdom
    open Fable.Helpers.Virtualdom.Html
     
    type Data = list<string * float> * int
    type Model = { values : Data; animation : Option<float * Data * Data> }

    // wrongly taken from: http://prcweb.co.uk/radialbarchart/
    let nineteen10 = 
        [
            "January", 2.6
            "February", 3.9
            "March", 5.4
            "April", 6.0
            "May", 10.1
            "June", 13.3
            "July", 13.2
            "August", 14.1
            "September", 11.8
            "October", 9.9
            "November", 2.8
            "December", 5.5
         ], 1910

    let twentytwelf =
        [
            "January", 4.7
            "February", 4.2
            "March", 7.7
            "April", 6.3
            "May", 10.5
            "June", 12.3
            "July", 14.1
            "August", 15.3
            "September", 11.9
            "October", 8.2
            "November", 5.8
            "December", 3.8 
        ], 2012

    type Action = StartTween of Data | TimeStep of float

    let lerp alpha original target =
        List.map2 (fun (k,v) (_,v')-> (k,v + (v'-v)*alpha)) original target

    let update e model msg =
        match msg with
            | StartTween d -> { model with animation = Some (0.0,model.values,d) }
            | TimeStep ms -> 
                match model.animation with 
                    | None -> model
                    | Some (l,(current,cs),(target,ts)) -> 
                        let step = ms * 0.01
                        if l + step >= 1.0 then { model with values = (target,ts); animation = None }
                        else
                            let interp =  lerp (l+step) current target 
                            { model with animation = Some ( l+step, (interp,cs), (target,cs) )}


    let (=>) a b = attribute a b

    let inline (~~) v = string v

    let view (model : Model) : DomNode<Action> = 
        let count = model.values |> fst |> List.length
        let center = V2i(60,60)
        let radius = 50.0

        let circle (center : V2i) (radius : float) =
            circle [ cx (string center.X); cy (string center.Y); r (string radius); "stroke" => "black";  "strokeDasharray" => "1,1"; "fill" => "none"; "strokeWidth" => "0.1"] []

        let circles = 
            [ for i in 5 .. 5 .. 20 do yield circle center ((float i / 20.0) * radius) ]

        let segment i (k,v) =                    
            let f = v / 20.0
            let scale = f * radius
            let color = if i%2 = 0 then "fill" => "#D2F4FF" else "fill" => "#76E783"
            let p =  (float i / (float count)) * System.Math.PI * 2.0
            let next =  (float (i+1) / (float count)) * System.Math.PI * 2.0
            let x a max = sin a * (max * radius) + float center.X
            let y a max = float center.Y - cos a * (max * radius)
            [
                line [ "x1" =>  ~~center.X; "y1" => ~~center.Y; "x2" => ~~(x p 1.0); "y2" => ~~(y p 1.0); "stroke" => "black"; "strokeWidth" => "0.1"; "strokeDasharray" => "1,1" ] []
                path [ "d" => sprintf "M %d %d L %f %f A %f %f 0 0 1 %f %f L %d %d" center.X center.Y (x p f) (y p f) scale scale (x next f) (y next f) center.X center.Y; color;] []
                path [ "d" => sprintf "M %f %f A %d %d 0 0 1 %f %f"  (x p 1.05) (y p 1.05) center.X center.Y  (x next 1.05) (y next 1.05); "stroke" => "green"; "id" => sprintf "%s" k; "visibility" => "hidden"] [ ]
            ]

        div [] [
            text "testing animation and svg"
            svg [ viewBox "0 0 120 120"; width "500px";  ] [
                yield! circles
                match model.animation with
                    | Some (_,(current,_),_) -> yield! current |> List.mapi segment |> List.concat
                    | _ ->  yield! model.values |> fst |> List.mapi segment |> List.concat
                yield Svg.svgElem "text" ["font-family" => "Verdana"; "fontSize" => "3.5"; "textAnchor" => "middle" ] [
                    for (k,v) in model.values |> fst do
                        yield Svg.svgElem "textPath" ["startOffset" => "50%"; "xlinkHref" => sprintf "#%s" k] [text (string k)]
                ]
            ]
            button [onMouseClick (fun _ -> StartTween twentytwelf)] [text "2012"]
            button [onMouseClick (fun _ -> StartTween nineteen10)] [text "1912"]
        ]

    let subscriptions m =
        match m.animation with
            | Some _ ->  Time.everyMs 10.0 (fun _ -> TimeStep 10.0)
            | _ -> Sub.none

    let initial = { values = nineteen10; animation = None }

    let app =  
        {
            initial = initial
            update = update
            view = view
            subscriptions = subscriptions
            onRendered = OnRendered.ignore
        }