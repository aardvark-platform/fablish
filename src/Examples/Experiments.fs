﻿#if INTERACTIVE
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
        objs    : list<string>
        
        // currently dragging something
        drag    : Option<int*V2d>

        // where is the mouse
        currentTrafo : Option<int*V2d>
    }


    type Action = 
        | StartDragging of int * V2d
        | StopDragging
        | MouseMove of V2d 

    let (=>) a b = attribute a b

    let getPositionOn (evtName : string) (f : Aardvark.Base.V2d -> 'msg) =
        let clientScript = 
            """function(evt) { 

                var rect = document.getElementById("rect").getBoundingClientRect();
    
                var r = { X : (ev.clientX-rect.left).toFixed(), Y : (ev.clientY-rect.top).toFixed() }
                return r;
            }"""
        let serverReaction (str : string) : Aardvark.Base.V2d = 
            Pickler.json.UnPickleOfString str 

        ClientEvent(evtName, clientScript, serverReaction >> f)

    let getPositionOnHack (evtName : string) (relativeTo : string) (f : Aardvark.Base.V2d -> 'msg) =
        let clientScript = 
            """function(evt) { 

                    var e = evt.target;
                    var dim = e.getBoundingClientRect();
                    var x = evt.clientX - dim.left;
                    var y = evt.clientY - dim.top;

                    return { X : x.toFixed(), Y : y.toFixed() };
            }""" 
        let serverReaction (str : string) : Aardvark.Base.V2d = 
            Pickler.json.UnPickleOfString str 

        ClientEvent(evtName, clientScript, serverReaction >> f)


    let update e (m : Model) a =
        match a with
            | StartDragging(i,d) -> { m with drag = Some (i,d) }
            | StopDragging       -> { m with drag = None } // do snapping
            | MouseMove mousePos -> 
                match m.drag with
                    | Some (i,d) ->                         
                        { m with currentTrafo = Some (i, mousePos-d); }
                    | _ -> m

    let colors = ["#edf8fb"; "#b2e2e2"; "#66c2a4";"#2ca25f";"#006d2c" ]

    let view (m : Model) : DomNode<Action> =    
        let cnt = 1.0 / float (List.length m.objs)

        let left,right = List.splitAt (List.length m.objs / 2) m.objs

        let w = 1000.0
        let y = 50.0
        let center = w / 2.0
        let height = 40.0
        let elements xs = 
            xs |> List.mapi (fun i e -> float i * cnt * w)

    
                                                         
        let boxes =
            elements m.objs |> List.mapi (fun i x -> 
                let o = m.objs.[i]                                       

                let color = 
                    match m.drag with
                        | Some (h,_) when h = i -> "#feb24c"
                        | _ -> List.item i colors

                let position =
                    match m.currentTrafo with
                        | Some(elem,s) when elem = i -> s
                        | _ -> V2d(x,y)

                let id = sprintf "box_%d" i

                elem "svg" [clazz "svg-rect noselect"; 
                            "width" => "100"; 
                            "height" => string height; 
                            "stroke" => "black"
                            "x" => string position.X; 
                            "y" => string position.Y;    
                           ] [

                        rect [ "width" => "100%"; "height" => "100%"
                               "rx" => "10"; "ry" => "10" 
                               "fill" => color
                               "id" => id                        
                               getPositionOnHack "onMouseDown" id (fun p -> StartDragging(i,p))] []

                        elem "text" ["x" => "50%"; "y" => "50%" 
                                     "pointerEvents" => "none"
                                     "alignmentBaseline" => "middle"; 
                                     "textAnchor" => "middle";
                                     "fill" => "black" ] [text (sprintf "%i %i" (int position.X) (int position.Y))]
                ]
             )

        div [] [            
            svg [ attribute "id" "rect"; viewBox "0 0 1000 1000"; width "1000px"; "height" => "1000px";
            getPositionOn "onMouseMove" MouseMove; onMouseUp (fun _ -> StopDragging);  ] boxes            
        ]
        

    let initial = {
        drag = None
        objs = [ "a"; "b"; "c"; "d"; "e" ]
        currentTrafo = None
        }

    let app  =  
        {
            initial = initial
            update = update
            view = view
            subscriptions = Subscriptions.none
            onRendered = OnRendered.ignore
        }

module HalfOrderedRects =

    open Aardvark.Base
    open Fablish
    open Fable.Helpers.Virtualdom
    open Fable.Helpers.Virtualdom.Html
    
    type Index = int * int

    type Data = Map<string,V2d>
    type Animation = { from : Data; target : Data; current : Data; t : double }

    type Model = {
        
        data    : list<string>
        ordering : list<list<string>>
        
        // currently dragging something
        drag    : Option<Index*V2d>

        // where is the mouse
        currentTrafo : Option<Index*V2d>

        animation : Option<Animation>
    }
   
    type Action = 
        | StartDragging of Index * V2d
        | StopDragging of V2d
        | MouseMove of V2d 
        | TimeStep of double

    let (=>) a b = attribute a b

    let getPositionOn (evtName : string) (f : Aardvark.Base.V2d -> 'msg) =
        let clientScript = 
            """function(evt) { 

                var rect = document.getElementById("rect").getBoundingClientRect();
    
                var r = { X : (ev.clientX-rect.left).toFixed(), Y : (ev.clientY-rect.top).toFixed() }
                return r;
            }"""
        let serverReaction (str : string) : Aardvark.Base.V2d = 
            Pickler.json.UnPickleOfString str 

        ClientEvent(evtName, clientScript, serverReaction >> f)

    let getPositionOnHack (evtName : string) (f : Aardvark.Base.V2d -> 'msg) =
        let clientScript = 
            """function(evt) { 

                    var e = evt.target;
                    var dim = e.getBoundingClientRect();
                    var x = evt.clientX - dim.left;
                    var y = evt.clientY - dim.top;

                    return { X : x.toFixed(), Y : y.toFixed() };
            }""" 
        let serverReaction (str : string) : Aardvark.Base.V2d = 
            Pickler.json.UnPickleOfString str 

        ClientEvent(evtName, clientScript, serverReaction >> f)


    let bin (x : int) (w : int) = (int x / w) 
    let raster (x : int) (w : int) = (bin x w) * w

    let removeAtIndex xs i = xs |> Seq.zip (Seq.initInfinite id) |> Seq.filter (fun (eid,e) -> eid <> i) |> Seq.map snd |> Seq.toList

    let rec insertAt i xs v =
        if i = 0 then v :: xs
        else
            match xs with
                | x::xs -> x::insertAt (i-1) xs v
                | [] ->[v]

    let moveElem (xs : list<list<'a>>) ((x,y):Index) ((nx,ny) : Index) =
        if nx = x then xs
        else
            let overlap = (nx - List.length xs)+1
            let xs = if overlap > 0 then xs @ List.replicate overlap [] else xs
            let movedElement = List.item y (List.item x xs)
            xs |> List.mapi (fun i d -> 
                if i = x then removeAtIndex d y
                elif i = nx then  d @ [movedElement] //insertAt ny d movedElement 
                else d
            ) |> List.filter (not << List.isEmpty)


    let genKeyFrame (m : Model) =
        m.ordering 
            |> List.mapi (fun i d -> 
                let x = i * (100 + 20)            
                d |> List.mapi (fun j k ->
                    let index = i,j
                    let y = j * (40 + 5)                 
                    let x = raster x 120

                    match m.currentTrafo with
                        | Some(elem,s) when elem = index -> 
                            None
                        | _ -> Some( k, V2d(x,y))
                ) |> List.choose id
            ) |> List.concat |> Map.ofList

        

    let update e (m : Model) a =
        match a with
            | StartDragging(i,d) -> { m with drag = Some (i,d) }
            | StopDragging p -> 
                match m.drag with
                    | Some (oldIndex,_) -> 
                        let newIndex = bin (int p.X) 120, bin (int p.Y) 45
                        printf "dragged: %A to %A" oldIndex newIndex

                        let newModel = { m with drag = None; ordering = moveElem m.ordering oldIndex newIndex; currentTrafo = None }
                        let oldKeyFrame = genKeyFrame m
                        let newKeyFrame = genKeyFrame newModel 
                         
                        { newModel with animation = Some { from = oldKeyFrame; target = newKeyFrame; t = 0.0; current = oldKeyFrame } }
                        
                    | _ -> { m with drag = None; currentTrafo = None }
            | MouseMove mousePos -> 
                match m.drag with
                    | Some (i,d) ->                         
                        { m with currentTrafo = Some (i, mousePos-d); }
                    | _ -> m

            | TimeStep dt -> 
                match m.animation with
                    | None -> m
                    | Some a -> 
                        let positions =
                            [
                                for d in m.data do
                                    match Map.tryFind d a.from, Map.tryFind d a.target with
                                        | Some f, Some t -> 
                                            let interpolated = f + (t-f)*a.t
                                            yield Some (d, interpolated)
                                        | _ -> yield None
                            ] |> List.choose id |> Map.ofList
                  //      printfn "%A %A" a.t positions
                        let t = min 1.0 (a.t + (dt * 0.03))// * (1.0-a.t)*(1.0-a.t)))
                        if t >= 1.0 then { m with animation = None }
                        else { m with animation = Some { a with t = t; current = positions }}


    //let colors = ["#ffffd4"; "#fed98e"; "#fe9929";"#d95f0e";"#993404" ]
    let colors = ["rgb(255,255,212)"; "rgb(254,217,142)"; "rgb(254,153,41)";"rgb(217,95,14)";"rgb(153,52,4)" ]

    let view (m : Model) : DomNode<Action> =    
          
        let w = 1000.0
        let y = 50.0
        let spacing = 20
        let center = w / 2.0
        let height = 40.0
                                                                     
        
        let drawElem (id : Index) (x:int,y:int) (color:string) t = 
            let color = 
                match m.drag with
                    | Some (h,_) when h = id -> "#feb24c"
                    | _ -> List.item (fst id) colors

            let x = raster x 120

            let position, zOrder =
                match m.currentTrafo with
                    | Some(elem,s) when elem = id -> s, 1
                    | _ -> 
                        match m.animation with
                            | None -> V2d(x,y), 0
                            | Some a -> 
                                match Map.tryFind t a.current with
                                    | None -> V2d(x,y), 0
                                    | Some p -> p,0



            elem "svg" [
                clazz "svg-rect noselect"; 
                "width" => "100"; 
                "height" => string height; 
                //"stroke" => "black"
                //"strokeWidth" => "2px"
                "x" => string position.X
                "y" => string position.Y
                ] [
                    rect [ "width" => "100%"; "height" => "100%"; "rx" => "10"; "ry" => "10"; "fill" => (List.item (fst id) colors);
                            getPositionOnHack "onMouseDown" (fun p -> StartDragging(id,p))] []

                    elem "text" ["x" => "50%"; "y" => "50%" 
                                 "pointerEvents" => "none"
                                 "alignmentBaseline" => "middle"; 
                                 "textAnchor" => "middle";
                                 "fill" => "black" ] [text t] //[text (sprintf "%i %i" (int position.X) (int position.Y))]
            ], zOrder

        let colums = 
            m.ordering 
             |> List.mapi (fun i d -> 
                    let x = i * (100 + 20)            
                    d |> List.mapi (fun j k ->
                        let index = i,j
                        let y = j * (40 + 5)                 
                        drawElem index (x,y) colors.[i] k
                    )
                ) 
             |> List.concat |> List.sortBy snd |> List.map fst
 

        div [] [            
            svg [ attribute "id" "rect"; viewBox "0 0 1000 1000"; width "1000px"; "height" => "1000px";
            getPositionOn "onMouseMove" MouseMove; getPositionOn "onMouseUp" StopDragging ] colums            
        ]
        
    let subscriptions m =
        match m.animation with
            | Some _ ->  Time.everyMs 5.0 (fun _ -> TimeStep 5.0)
            | _ -> Sub.none

    let initial = {
            drag = None
            data = [ "a"; "b"; "c"; "d"; "e" ]
            ordering = [ [ "a"; "b"; "c"];["d"; "e"];]
            currentTrafo = None
            animation = None
        }

    let app  =  
        {
            initial = initial
            update = update
            view = view
            subscriptions = subscriptions
            onRendered = OnRendered.ignore
        }