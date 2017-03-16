#if INTERACTIVE
#r "../../packages/Aardvark.Base/lib/net45/Aardvark.Base.dll"
#else
#endif
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
        printfn "rock it."
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
                match model.animation with
                    | Some (_,(current,_),_) -> yield! current |> List.mapi segment |> List.concat
                    | _ ->  yield! model.values |> fst |> List.mapi segment |> List.concat
                yield Svg.svgElem "text" ["font-family" => "Verdana"; "fontSize" => "3.5"; "textAnchor" => "middle" ] [
                    for (k,v) in model.values |> fst do
                        yield Svg.svgElem "textPath" ["startOffset" => "50%"; "xlinkHref" => sprintf "#%s" k] [text (string k)]
                ]
                yield! circles
            ]
            button [onMouseClick (fun _ -> StartTween twentytwelf)] [text "2012"]
            button [onMouseClick (fun _ -> StartTween nineteen10)] [text "1912"]
        ]

    let subscriptions m =
        match m.animation with
            | Some _ ->  Time.everyMs 10.0 (fun _ -> TimeStep 10.0)
            | _ -> Sub.none

    let initial = { values = nineteen10; animation = None }

    let app () =  
        {
            initial = initial
            update = update
            view = view
            subscriptions = subscriptions
            onRendered = OnRendered.ignore
        }

module AngularHistogram =
    
    open Aardvark.Base
    open Fablish
    open Fable.Helpers.Virtualdom
    open Fable.Helpers.Virtualdom.Html
    open FSharp.Data

    type Measures = CsvProvider<"cape_desire.csv">
   
    let measurements () = [
        Measures.Load("C:\Users\ortner\Desktop\cape_desire.csv")
        Measures.Load("C:\Users\ortner\Desktop\garden_city.csv")
        Measures.Load("C:\Users\ortner\Desktop\unnamed_sol318.csv")
    ]
    
    module Binning =
        
        let halfShift n data =
            let min = data |> Seq.min
            let max = data |> Seq.max
            let bucketSize = (max - min) / (float n)

            data |> Seq.map (fun x -> (x - bucketSize / 2.0))

        let indices n data =  
            let min = data |> Seq.min
            let max = data |> Seq.max
            let bucketSize = (max - min) / (float n)
            
            data |> Seq.map (fun x -> int ((x - min) /bucketSize))

        let bin n indices = 
            let mutable buckets = [0 .. n-1]
            
            for i in indices do
                buckets <- List.updateAt i (fun x -> x + 1) buckets

            let maxBucket = buckets |> Seq.max
            buckets |> List.toSeq |> Seq.map (fun x -> float x / float maxBucket)
                       
    let radToDeg r = (r * 180.0) / System.Math.PI
    let degToRad d = (d * System.Math.PI) / 180.0

    let buckets = 24   
    let angleLabel angle =
        match angle with
            | 0 -> "N"
            | 90 -> "E"
            | 180 -> "S"
            | 270 -> "W"
            | _ -> sprintf "%A°"angle

    let generateLabels noOfbuckets =
        let step = 360 / noOfbuckets
        let angles = [0 .. step .. 360]
        angles |> List.map (fun x -> angleLabel x)

    let makeData rows =
        rows
         |> Seq.filter (fun (x : Measures.Row) -> x.MeasurementType = "DipAndStrike") 
         |> Seq.map (fun x -> (x.DipAzimuth)) |> Binning.halfShift buckets

    let angles2 () = measurements () |> Seq.map (fun s -> makeData s.Rows)

    let angles () = (measurements()).[0].Rows 
                    |> Seq.filter (fun x -> x.MeasurementType = "DipAndStrike") 
                    |> Seq.map (fun x -> (x.DipAzimuth)) |> Binning.halfShift buckets

    let keys = generateLabels buckets
    let dipData' () = angles () |> Binning.indices buckets |> Binning.bin buckets |> Seq.mapi (fun i x -> (keys.[i],x)) |> Seq.toList
    let dipData () = dipData'(), 1910    
     
    type Data = list<string * float> * int
    type Model = { values : Data; animation : Option<float * Data * Data> }

    // wrongly taken from: http://prcweb.co.uk/radialbarchart/

    let twentytwelf =
        [
            "N",    2.6
            "30°",  6.9
            "60°",  5.4
            "E",    3.0
            "120°", 11.1
            "150°", 8.3
            "S",    13.2
            "210°", 14.1
            "240°", 9.8
            "W",    1.9
            "300°", 2.8
            "330°", 1.5
        ], 2012

    type Action = StartTween of Data | TimeStep of float

    let lerp alpha original target =
        List.map2 (fun (k,v) (_,v')-> (k,v + (v'-v)*alpha)) original target

    let update e model msg =
        printfn "rock it."
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
        let halfOffset = System.Math.PI / float count

        let circle (center : V2i) (radius : float) =
            circle [ cx (string center.X); cy (string center.Y); r (string radius); "stroke" => "black"; "fill" => "none"; "strokeWidth" => "0.1"] []

        let circles = 
            [ for i in 5 .. 5 .. 20 do yield circle center ((float i / 20.0) * radius) ]

        let segment i (k,v) =                    
            let f = v
            let scale = f * radius
            let color = "fill" => "#AAAAAA" // if i%2 = 0 then "fill" => "#D2F4FF" else "fill" => "#76E783"            
            let p =  (float i / (float count)) * System.Math.PI * 2.0 - halfOffset
            let next =  (float (i+1) / (float count)) * System.Math.PI * 2.0 - halfOffset
            let x a max = sin a * (max * radius) + float center.X
            let y a max = float center.Y - cos a * (max * radius)
            [
                line [ "x1" =>  ~~center.X; "y1" => ~~center.Y; "x2" => ~~(x p 1.0); "y2" => ~~(y p 1.0);
                       "stroke" => "black"; "strokeWidth" => "0.1";] [] 
                path [ "d" => sprintf "M %d %d L %f %f A %f %f 0 0 1 %f %f L %d %d" center.X center.Y (x p f) (y p f) scale scale (x next f) (y next f) center.X center.Y; color;] []
                path [ "d" => sprintf "M %f %f A %d %d 0 0 1 %f %f"  (x p 1.05) (y p 1.05) center.X center.Y  (x next 1.05) (y next 1.05); "stroke" => "green"; "id" => sprintf "%s" k; "visibility" => "hidden"] [ ]
            ]

        div [] [
            text "testing animation and svg"
            svg [ viewBox "0 0 120 120"; width "500px";  ] [
                match model.animation with
                    | Some (_,(current,_),_) -> yield! current |> List.mapi segment |> List.concat
                    | _ ->  yield! model.values |> fst |> List.mapi segment |> List.concat
                yield Svg.svgElem "text" ["font-family" => "Verdana"; "fontSize" => "3.5"; "textAnchor" => "middle" ] [
                    for (k,v) in model.values |> fst do
                        yield Svg.svgElem "textPath" ["startOffset" => "50%"; "xlinkHref" => sprintf "#%s" k] [text (string k)]
                ]
                yield! circles
            ]
            button [onMouseClick (fun _ -> StartTween twentytwelf)] [text "2012"]
            button [onMouseClick (fun _ -> StartTween (dipData ()))] [text "capeDesire"]
        ]

    let subscriptions m =
        match m.animation with
            | Some _ ->  Time.everyMs 10.0 (fun _ -> TimeStep 10.0)
            | _ -> Sub.none

    let initial () = { values = dipData(); animation = None }

    let app () =  
        {
            initial = initial ()
            update = update
            view = view
            subscriptions = subscriptions
            onRendered = OnRendered.ignore
        }


module OrderedRects =

    open Aardvark.Base
    open Fablish
    open Fable.Helpers.Virtualdom
    open Fable.Helpers.Virtualdom.Html

    

    type Model = {
        selected : option<int>
        objs : list<string>
        drag : V2d
        }

    let isSelected i m =
            match m.selected with
                | None -> false
                | Some k -> i = k 

    type Action = 
        | Select of int
        | Deselect
        | Drag of int * V2d 
        | Drop of V2d

    let (=>) a b = attribute a b

    let onMove (f : Aardvark.Base.V2d -> 'msg) =
        let clientMove = 
            """function(ev) { 
                return { X : ev.clientX.toFixed(), Y : ev.clientY.toFixed() };
            }"""
        let serverMove (str : string) : Aardvark.Base.V2d = 
            Pickler.json.UnPickleOfString str // up is down in mouse wheel events

        ClientEvent("onMouseMove", clientMove, serverMove >> f)

    let update e (m : Model) a =
        match a with
        | Select i -> 
            printf "Selected: %A Text: %A \n" i m.objs.[i]            
            let sel = match m.selected with
                        | None ->  Some i
                        | Some k -> if k = i then None else Some i            
            { m with selected = sel }
        | Deselect ->             
            {m with selected = None}
        | Drag(i,d) -> 
            
            if isSelected i m 
                then printf "x: %A \n" d.X
                     {m with drag=d}                      
                else m
        | _ -> m

    let colors = ["#edf8fb"; "#b2e2e2"; "#66c2a4";"#2ca25f";"#006d2c" ]

    let view (m : Model) : DomNode<Action> =    
        let cnt = 1.0 / float (List.length m.objs)

        let left,right = List.splitAt (List.length m.objs / 2) m.objs

        let w = 1000.0
        let y = 50.0
        let center = w / 2.0
        let height = 20.0
        let elements xs = 
            xs |> List.mapi (fun i e -> float i * cnt * w)

       

        let color m i =
            if isSelected i m 
                then "#feb24c"
                else List.item i colors
                                                         
        let boxes =
            elements m.objs |> List.mapi (fun i x -> 
                let o = m.objs.[i]                
                let x = if isSelected i m then string (m.drag.X) else string (x+50.0)

                elem "svg" [clazz "svg-rect noselect"; 
                            "width" => "100"; 
                            "height" => string height; 
                            "x" => string x; 
                            "y" => string y;
                            onMouseDown (fun _ -> Select i)
                            onMove(fun d -> Drag(i,d))
                 //           onMouseOut (fun _ -> Deselect)
                            onMouseUp (fun _ -> Deselect)] [

                        rect [ "width" => "100%"; "height" => "100%"
                               "rx" => "10"; "ry" => "10" 
                               "fill" => color m i] []

                        elem "text" ["x" => "50%"; "y" => "50%" 
                                     "pointerEvents" => "none"
                                     "alignmentBaseline" => "middle"; 
                                     "textAnchor" => "middle";
                                     "fill" => "black" ] [text (string o)]
                ]
             )

        svg [ viewBox "0 0 1000 200"; width "100%"; ] boxes
        

    let initial = {
        selected = None
        objs = [ "a"; "b"; "c"; "d"; "e" ]
        drag = V2d.Zero
        }

    let app  =  
        {
            initial = initial
            update = update
            view = view
            subscriptions = Subscriptions.none
            onRendered = OnRendered.ignore
        }