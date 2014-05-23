// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

#light
open System
open System.Drawing
open System.Drawing.Imaging
open System.Runtime.InteropServices
open System.Windows.Forms
open System.Drawing.Drawing2D
open System.IO
open System.Runtime.Serialization
open System.Runtime.Serialization.Json
open System.ComponentModel
type Vector = System.Windows.Vector
type HttpUtility = System.Web.HttpUtility

let inline id a = a 
let inline const1 value _ = value
let inline applyNTimes f n s =
    let fs = Seq.init n <| const1 f
    Seq.fold (>>) id fs s
//let inline curry2 f t = f (fst t) (snd t)
let inline uncurry2 f l r  = f (l,r)
let inline flip f x y = f y x
let inline isEqp v1 v2 p = abs(v1-v2) <= p
let inline isEq v1 v2 = isEqp v1 v2 0.00000001
//let inline isNEq v1 v2 = not <| isEq v1 v2
let inline isEqPtf (x,y) (x2,y2) = isEq x x2 && isEq y y2
let inline list_equality_adjacent_items_remove isEq items = List.fold (fun l i -> match l with
                                                                                    | [] -> [i]
                                                                                    | i_::_ when isEq i_ i -> l
                                                                                    | _::_ -> i::l  ) [] items
                                                            |> List.rev

let inline errorf format = Printf.ksprintf (fun s -> Console.WriteLine(s);Environment.Exit(1);Unchecked.defaultof<_>) format
let inline callback_and_return f result = 
    ignore f
    result
[<DataContract>]
type CodeGeneratePattern = 
    struct
        [<DataMember()>]
        [<DefaultValueAttribute("")>] 
        val mutable prologue: string;
        [<DataMember()>]
        val mutable code_generate_pattern: string;
        [<DataMember()>]
        [<DefaultValueAttribute("")>] 
        val mutable code_generate_pattern_joint: string;
        [<DataMember()>]
        [<DefaultValueAttribute("")>] 
        val mutable epilogue: string;
    end
let readJsonObject<'JsonData> filename =
    try
        use fs = new FileStream(filename, FileMode.OpenOrCreate)
        let serializer = DataContractJsonSerializer typeof<'JsonData>
        (serializer.ReadObject fs) :?> 'JsonData |> Some
    with
        e -> None
let readCodeGeneratePattern = readJsonObject<CodeGeneratePattern>
let imageView (image:_[,]) point = let x,y = point in image.[y,x]
let imageViewf image getpoint = imageView image <| getpoint()
let loadImage name = 
    try
        use b = Bitmap.FromFile(name) :?> Bitmap
        Some <| array2D[|for y in 0..b.Height-1 -> 
                            [|for x in 0..b.Width-1 -> 
                                b.GetPixel(x,y)|]|]
    with 
        | _ -> None
let imageToBoolImage image level =
   Array2D.map (fun (c:Color) -> c.R>level && c.G>level && c.G>level && c.A>level) image
let boolImageToMonochrome image =
    Array2D.map (fun c -> if c then Color.White else Color.Black) image
let imageToSystemImage (image:Color[,]) =
    let result = new Bitmap(image.GetLength(1), image.GetLength(0), PixelFormat.Format32bppArgb)
    try
       for y in 0..result.Height-1 do
           for x in 0..result.Width-1 do 
               result.SetPixel(x, y, image.[y,x])
    with _ -> 
        result.Dispose()
        reraise()
    result
let drawPolyLineAtSystemImage image (color:Color) (polyline:Point[]) =
    use g = Graphics.FromImage(image)
    use p = new Pen(color)
    g.DrawLines(p, polyline)
(*let toRowColumnImage (image:_[,]) = 
    [|for y in 0..image.GetUpperBound(0) -> 
        [|for x in 0..image.GetUpperBound(1) -> 
            image.[y,x]|]|]*)
let replaceBorder borderColor (image:_[,]) =
    Array2D.mapi (fun y x c -> if x=0 || y=0 || x=image.GetUpperBound(1) || y=image.GetUpperBound(0) then borderColor else c) image
let array2DIndicies (a:_[,]) = 
    seq{for y in a.GetLowerBound(0)..a.GetUpperBound(0) do 
            for x in a.GetLowerBound(1)..a.GetUpperBound(1) -> (x, y)}
let seqLast s = Seq.toList s |> List.rev |> List.head//Seq.last replacement.
let findXYWithFirstWhiteBeforeBlack (image:bool[,]) =
    let yb = image.GetUpperBound(0)
    let xb = image.GetUpperBound(1)
    let arrayIndiciesBeforeBlack = Seq.takeWhile (imageView image) <| array2DIndicies image
    match arrayIndiciesBeforeBlack with
        | is when Seq.isEmpty is -> None
        | is when seqLast is = (xb, yb) -> None
        | is -> Some <| seqLast is
type Direction = D10 | D01 | Dm10 | D0m1 with
    member this.rightRotate = match this with
                                | D10 -> D01
                                | D01 -> Dm10
                                | Dm10 -> D0m1
                                | D0m1 -> D10
    member this.leftRotate = applyNTimes (fun (this_:Direction) -> this_.rightRotate) 3 this
    member this.getXYIdentityVector() = match this with
                                        | D10 -> (1, 0)
                                        | D01 -> (0, 1)
                                        | Dm10 -> (-1, 0)
                                        | D0m1 -> (0, -1)
    member this.addToXY xy = 
        let ivx, ivy = this.getXYIdentityVector()
        fst xy + ivx, snd xy + ivy
type Point_t = (int*int)
type Pattern (pointXY:Point_t, direction:Direction) = 
    member this.direction() = direction
    member this.pointXY() = pointXY
    new (pattern:Pattern, point:Point_t) = Pattern(point, pattern.direction())
    new (pattern:Pattern, direction:Direction) = Pattern(pattern.pointXY(), direction)
    member this.getForwardXY() = direction.addToXY pointXY
    member this.getLeftXY() = direction.leftRotate.addToXY pointXY
    member this.getDiagonalXY() = direction.leftRotate.addToXY <| direction.addToXY pointXY
    member this.rightRotate() = Pattern(this, direction.rightRotate)
    member this.leftRotate() = Pattern(this, direction.leftRotate)
    (*member this.pointInPattern (*xy*) = function
        |p when p = this.pointXY() -> true
        |p when p = this.getForwardXY() -> true
        |p when p = this.getLeftXY() -> true
        |p when p = this.getDiagonalXY() -> true
        |_->false*)
    member this.seeDiagonal image = imageViewf image this.getDiagonalXY
    member this.seeForward image = imageViewf image this.getForwardXY
    member this.seeLeft image = imageViewf image this.getLeftXY
    member this.seeCurrent image = imageViewf image this.pointXY
let (|Pattern|) (pattern:Pattern) = (pattern.pointXY(), pattern.direction())
let makeSilhuettePointsList image directionToSilhouette (firstPositionNearSilhouette:Point_t) = 
    let nextPattern (pattern:Pattern) =
        assert(pattern.seeCurrent image)
        let up = pattern.seeForward image
        let diagonal = pattern.seeDiagonal image
        let left = pattern.seeLeft image
        //printf "%A : %A : %A \n" (pattern.pointXY()) (pattern.direction()) (up, diagonal, left)
        match up, diagonal, left with
            |true, false, true
            |true, false, false
            |true, true, false
            |true, true, true -> pattern.rightRotate()
            |false, true, true -> Pattern(pattern, pattern.getDiagonalXY())
            |false, false, true -> Pattern(pattern, pattern.getLeftXY())
            |false, false, false
            |false, true, false -> pattern.leftRotate()
    let firstPattern = Pattern(firstPositionNearSilhouette, directionToSilhouette)
    assert (firstPattern.seeCurrent image)
    assert (not <| firstPattern.seeForward image)
    firstPattern |>
    nextPattern |>
    Seq.unfold (function
        |Pattern(p,_) when p=firstPositionNearSilhouette -> None
        |pattern -> let nextPatternValue = nextPattern pattern in Some (nextPatternValue.pointXY(), nextPatternValue)
    ) |> 
    Seq.toList |> 
    uncurry2 List.Cons firstPositionNearSilhouette |>
    list_equality_adjacent_items_remove (=)
let getSilhouettePointsListForBorderedImage (sourceImage:bool[,]) positionAtNearLeftOfSilhuette =
    assert(replaceBorder true sourceImage = sourceImage)
    makeSilhuettePointsList sourceImage Direction.D10 positionAtNearLeftOfSilhuette
type Line2DEquation = 
    | LineEquation of (*k:*)float * (*b:*)float
    | Vertical of (*x:*)float
    static member FromPoints ((x1,y1), (x2,y2)) = 
        match isEq x1 x2, isEq y1 y2 with
            | true, true -> None
            | true, false -> Some <| Vertical(x1)
            | _ -> let dx = x1-x2 in Some <| LineEquation((y1-y2)/dx, (x1*y2-x2*y1)/dx)
    member l1.intersection l2 = 
        match l1, l2 with
            | Vertical xl, Vertical xr -> None
            | Vertical xp, LineEquation (k, b)
            | LineEquation (k, b), Vertical xp -> Some (xp, k*xp+b)
            | LineEquation (k1, _), LineEquation (k2, _) when isEq k1 k2 -> None
            | LineEquation (k1, b1), LineEquation (k2, b2) -> 
                let dk = k1-k2 
                Some ((b2-b1)/dk, (b2*k1-b1*k2)/dk)
    member this.perpendicularFromPoint (xp,yp) = 
        match this with
            | Vertical tx when isEq xp tx -> None
            | LineEquation (k, b) when isEq yp <| k*xp+b -> None
            | LineEquation (k, _) when isEq k 0.-> Some <| Vertical xp
            | LineEquation (k, b) -> 
                let nk = -1./k
                let nb = yp + xp/k
                Some <| LineEquation (nk, nb)
            | Vertical tx -> Some <| LineEquation (0., yp)
let pointAtLine (line:Line2DEquation) quality point = 
    let perpendicularToLineFromPoint = line.perpendicularFromPoint point
    if Option.isNone perpendicularToLineFromPoint then
        true
    else
        let projectionOfTestingPointToLine = Option.get <| line.intersection (Option.get perpendicularToLineFromPoint)
        let intersection = Vector(fst projectionOfTestingPointToLine, snd projectionOfTestingPointToLine)
        let perpendicularRadiusVector = intersection - Vector(fst point, snd point)
        let squaredDistanceBetweenLineAndPoint = perpendicularRadiusVector.LengthSquared
        squaredDistanceBetweenLineAndPoint<=(quality*quality)
let atLine points (((p1x,p1y), (p2x,p2y)) as line) quality = 
    let line = Line2DEquation.FromPoints line
    if Option.isNone line then
        //printf "%A" ((p1x,p1y), (p2x,p2y))
        //assert false//Error at algorithm. It is can drop long thick thin lines.
        List.forall (fun (px,py) -> isEq p1x px && isEq p1y py) points
    else
        List.forall (pointAtLine (Option.get line) quality) points
let lastAndOther list = 
    let last = seqLast list
    last, (Seq.toList list |> List.rev |> List.tail |> List.rev)
let onLine quality firstPointAtLine points = 
    let last, other = lastAndOther points
    (*if isEq (fst last) (fst firstPointAtLine) && isEq (snd last) (snd firstPointAtLine) then
        printf "%A" <| Seq.length points
        Seq.iter (printf "%A") points
        printf "\n"
        assert false*)
    atLine other (firstPointAtLine, last) quality
let growGroups (l:_ seq) =
    Seq.init (Seq.length l + 1 ) (flip Seq.take l)
let getNextLineAndRestPoints quality firstPointAtLine points =
    let pointsGroups = Seq.skip 1 <| growGroups points
    let onLinePointsGroups = Seq.takeWhile (onLine quality firstPointAtLine) pointsGroups
    let onLineBiggestPointsGroup = seqLast onLinePointsGroups |> Seq.toList
    let lengthOfPointsOnLine = List.length onLineBiggestPointsGroup
    let restPoints = Seq.skip lengthOfPointsOnLine points
    let endOfLine = seqLast onLineBiggestPointsGroup
    endOfLine, restPoints, lengthOfPointsOnLine
let generateNextPointAtPolylineAndRestPoints progressViewer allPointsAmount quality (firstPointAtLine, restPoints, pointCounter) =
    progressViewer pointCounter allPointsAmount
    match restPoints with
        | s when Seq.isEmpty s -> None
        //| s when Seq.isEmpty <| Seq.skip 1 s -> let endOfLine = Seq.exactlyOne s in Some <| (endOfLine, ((0.,0.), Seq.empty, pointCounter+1))
        | _ -> let endOfLine, restPoints_, pointsOnLine = getNextLineAndRestPoints quality firstPointAtLine restPoints
               Some <| (endOfLine, (endOfLine, restPoints_, pointCounter+pointsOnLine))
let pointsToLines progressViewer quality (points:_ list) =
    let pointsAmount = List.length points
    let firstPoint = Seq.head points
    let otherPoints = Seq.skip 1 points
    Seq.unfold (generateNextPointAtPolylineAndRestPoints progressViewer pointsAmount quality) (firstPoint, otherPoints, 0) |>
    Seq.toList |>
    uncurry2 List.Cons firstPoint |>
    list_equality_adjacent_items_remove isEqPtf
let inline XYtoFloatXY (x,y) = float(x), float(y)
let inline XYtoPoint (x,y) = Point (int x, int y)
let inline XYtoPointF (x,y) = PointF (float32 x, float32 y)
let (|Prefix|_|) p (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None
type OptoParser private(switchArgs, parameterizedArgs) = 
    let switchArgs = switchArgs
    let parameterizedArgs = parameterizedArgs
    let mutable mRequested = List.Empty
    member this.requested
         with get() = mRequested
         and set(value) = mRequested <- value
    //member val private requested = List.Empty with get,set
    new (switchArgsList, parameterizedArgsList, argv) = 
        assert(not <| Set.contains "" switchArgsList)
        assert(not <| Set.contains "" parameterizedArgsList)
        let keysIntersection = Set.intersect switchArgsList parameterizedArgsList
        if not keysIntersection.IsEmpty then
            errorf "Keys at both argsLists: %A." <| Set.toList keysIntersection
        let lastKey = ref None
        let switchArgs = ref Set.empty
        let parameterizedArgs = ref Map.empty
        //printfn "%A" argv
        argv |>
        Array.iter (function
            | ""  -> ()
            | "/" -> errorf "Empty command line option error: /"
            | Prefix "/" arg -> 
                match Set.contains arg switchArgsList, Set.contains arg parameterizedArgsList, Set.contains arg !switchArgs, Map.containsKey arg !parameterizedArgs with
                    | true, false, false, false -> switchArgs := Set.add arg !switchArgs
                    | true, false, true, false
                    | false, true, false, true -> errorf "Duplicate command line option /%s" arg
                    | false, true, false, false -> 
                        match !lastKey with
                            | Some lk -> errorf "You should specify value for command line option %s like \"/%s some_value\" " lk lk
                            | None -> lastKey := Some arg
                    | false, false, false, false -> errorf "Unknown command line option /%s" arg
                    | state -> failwithf "Command line options parser algorithm error with state %A %A %A %A." state !lastKey !switchArgs !parameterizedArgs
            | arg -> match !lastKey with
                        | Some lk -> 
                            parameterizedArgs := Map.add lk arg !parameterizedArgs
                            lastKey := None
                        | None -> errorf "You should specify options name at command line like \"/option_name %s\" " arg
        )
        OptoParser(!switchArgs, !parameterizedArgs)
    member this.switchArg name = 
        this.requested <- name::this.requested 
        Set.contains name switchArgs
    member this.parameterizedArg name = 
        this.requested <- name::this.requested 
        Map.tryFind name parameterizedArgs
    member this.getUnusedArguments() =
        Map.toList parameterizedArgs |>
        List.unzip |>
        fst |>
        Set.ofList |>
        Set.union switchArgs |>
        flip Set.difference (Set.ofList this.requested) |>
        Set.toList
    member this.isEmptyCommandLine() =
        Map.isEmpty parameterizedArgs && Set.isEmpty switchArgs
    member this.checkUnusedArguments() =
        let ua = this.getUnusedArguments()
        if not <| ua.IsEmpty then
            printf "Some command line options has no affect: "
            List.iter (printf "%s ") ua
let floatToColorByteComponent v = byte <| (min (max v 0.) 1.) * 255.
let imageIsSmall (image: _[,]) = 
    image.GetLength(1) < 6 || image.GetLength(0) < 6

//You MUST use expression (lazy expression) in ifFstThenSnd parameter lazyConstructed!!!
let inline ifFstThenSnd v (lazyConstructed:_ Lazy) = match v with (true, v) -> v | (false, _) -> lazyConstructed.Force()

let inline tryParseIdentity v = (true, v)
let inline readOptionWithDefault (commandLineParser:OptoParser) optionName defaultValue tryParse = 
    let cmdOption = commandLineParser.parameterizedArg optionName
    if cmdOption.IsNone then
        defaultValue
    else
        ifFstThenSnd (tryParse cmdOption.Value) <| lazy errorf "Wrong /%s option value: \"%s\"" optionName cmdOption.Value
let showFormWithImage (commandLineParser:OptoParser) image =
    if not<| commandLineParser.switchArg "disable-view-preprocessed-image" then
        use form1 = new Form(Width = 800, Height = 600, Text = "View result")
        use pictureBox1 = new PictureBox()
        pictureBox1.SizeMode <- PictureBoxSizeMode.StretchImage
        pictureBox1.Image <- image
        pictureBox1.Dock <- DockStyle.Fill 
        form1.Controls.Add(pictureBox1)
        form1.Show()
        Application.Run(form1)
let generateCodeFile (commandLineParser:OptoParser) edgesPoints longLinesPoints (destination_file_path:string) =
    printfn "Code generating..."
    let code_generate_pattern_file = commandLineParser.parameterizedArg "code-generate-pattern-file"
    if code_generate_pattern_file.IsNone then
        errorf "Use command line option /code-generate-pattern-file \"path_to_file.json\" with path to file with pattern for code generation."
    let code_generate_pattern_file = code_generate_pattern_file.Value
    let code_generate_pattern_settings = readCodeGeneratePattern code_generate_pattern_file
    if code_generate_pattern_settings.IsNone then
        errorf "Can't load file \"%s\" with pattern for code generation. Check path and file format at parameter /code-generate-pattern-file \"path_to_file.json\"." code_generate_pattern_file
    let code_generate_pattern_settings = code_generate_pattern_settings.Value
    let code_generate_pattern = code_generate_pattern_settings.code_generate_pattern
    let pointsSource = if Option.isNone longLinesPoints then edgesPoints else Option.get longLinesPoints
    let inline readOptionWithDefault n = readOptionWithDefault commandLineParser n
    let prologue = code_generate_pattern_settings.prologue
    let epilogue = code_generate_pattern_settings.epilogue
    let code_generate_pattern_joint = code_generate_pattern_settings.code_generate_pattern_joint
    let item_number_multiplier = readOptionWithDefault "item-number-multiplier" 1 Int32.TryParse
    let item_number_shift = readOptionWithDefault "item-number-shift" 0 Int32.TryParse
    let m11 = readOptionWithDefault "scale-x" 1.f Single.TryParse
    let m12 = readOptionWithDefault "matrix-m12" 0.f Single.TryParse
    let m21 = readOptionWithDefault "matrix-m21" 0.f Single.TryParse
    let m22 = readOptionWithDefault "scale-y" 1.f Single.TryParse
    let dx = readOptionWithDefault "shift-x" 0.f Single.TryParse
    let dy = readOptionWithDefault "shift-y" 0.f Single.TryParse
    let mutable pointsSource = List.map XYtoPointF pointsSource |> List.toArray
    do 
        use matrix = new Matrix (m11, m12, m21, m22, dx, dy)        
        matrix.TransformPoints pointsSource
    let pointsSource = List.ofArray pointsSource |> List.map (fun p -> (p.X, p.Y))
    try
        use stream = new StreamWriter (destination_file_path)
        stream.Write prologue
        pointsSource |> List.iteri (fun i (x,y) -> 
            if i>0 then 
                stream.Write code_generate_pattern_joint
            let item_number = item_number_shift + i*item_number_multiplier
            let code_item = 
                code_generate_pattern.Replace("$#{number}", string item_number).Replace("$#{x}", string x).Replace("$#{y}", string y)
            stream.Write code_item            
        )
        stream.Write epilogue
        stream.Flush()
    with _ -> errorf "Can't write generated code to file \"%s\"." destination_file_path
    ()
[<EntryPoint>]
let main argv = 
    printfn "Started..."
    let commandLineParser = OptoParser(set["?";"disable-view-preprocessed-image";"disable-view-result";"disable-line-processing";"disable-view-silhouette-finding";"disable-view-silhouette-edges";"disable-view-silhouette-long-lines"],set["image-source";"save-preprocessed-image-to-file";"preprocess-image-border-value";"save-generated-code-to-file";"code-generate-pattern-file";"item-number-multiplier";"item-number-shift";"scale-x";"matrix-m12";"matrix-m21";"scale-y";"shift-x";"shift-y";"line-quality"], argv)
    if commandLineParser.isEmptyCommandLine() then
        errorf "Program for code generation from silhouette on image.\n\
        Also program suitable for simple preprocess some image to white background and black silhouette image.\n\
        Use option /? for help about command line options."
    if commandLineParser.switchArg "?" then
        commandLineParser.checkUnusedArguments()
        errorf "Empty options on command line - about program;\n\n\
        /? - this help;\n\n\
        /disable-progress-bar - disable progress view of image processing;\n\n\
        /disable-view-preprocessed-image - disable view image after silhouette preprocessing;\n\n\
        /disable-view-result - disable view marks on image after image preprocessing;\n\n\
        Also some marks on viewed image can be disabled individually:\n\
        /disable-view-silhouette-finding\n\
        /disable-view-silhouette-edges\n\
        /disable-view-silhouette-long-lines\n\n\
        /disable-line-processing - disable recognizing long line primitives at image;\n\n\
        /line-quality value - long line primitives recognizing quality. Value measurement is points amount from line to edge. Default value is 3.0 (3,0 on some cultures);\n\n\
        /image-source \"path_to_file.jpg\" - source image for preprocessing, recognizing and code generation.\n\n\
        /save-preprocessed-image-to-file \"path_to_file.jpg\" - silhouetted image save to file path_to_file. Can be used for correct silhouette;\n\n\
        /preprocess-image-border-value value - silhouette recognize border used for detect edges. Color is white when any color component of image more than value, otherwise color is black. Limits of border value from 0.0 to 1.0. Default value is 0.5. White color should be background and black color should be silhouette edge.\n\n\
        /save-generated-code-to-file \"path_to_file.txt\" - generated code will be save to file path_to_file;\n\n\
        /code-generate-pattern-file \"path_to_file_with_pattern.json\" - generated code patterns.\n\
        \tGenerated code patterns format json with members:\n\
        \t\t\"prologue\" : \"pattern\" - generated code prologue;\n\
        \t\t\"epilogue\" : \"pattern\" - generated code epilogue;\n\
        \t\t\"code_generate_pattern\" : \"pattern\" - generated code single item pattern;\n\
        \t\t\"code_generate_pattern_joint\" : \"pattern\" - pattern for joint between adjacent single item patterns.\n\
        \tYou can use at members json special characters representation for complex pattern writing, e.g. new line characters \"\\r\\n\" and many others.\n\
        \tYou can use few substitution patterns at pattern code_generate_pattern.\n\
        \tSubstitution patterns:\n\
        \t\t$#{number} - number of generated item;\n\
        \t\t$#{x} - X coordinate of point;\n\
        \t\t$#{y} - Y coordinate of point;\n\n\
        /item-number-multiplier value - number of generated item multiplier. Default value is 1;\n\n\
        /item-number-shift - start value for number of generated item. Default value is 0;\n\n\
        Another options:\n\
        \t/scale-x value - Default value is 1.0\n\
        \t/matrix-m12 value - Default value is 0.0\n\
        \t/matrix-m21 value - Default value is 0.0\n\
        \t/scale-y value - Default value is 1.0\n\
        \t/shift-x value - Default value is 0.0\n\
        \t/shift-y value - Default value is 0.0\n\
        \tAll point coordinates will multiply to matrix M before generate code. Matrix M is equal to:\n\
        \t[scale-x    matrix-m12 0.0]\n\
        \t[matrix-m21 scale-y    0.0]\n\
        \t[shift-x    shift-y    1.0]\n"
    let image_source_path = commandLineParser.parameterizedArg "image-source"
    if image_source_path.IsNone then
        errorf "Nothing to do. Use option \'/image-source \"path_to_file.jpg\"\' for specify source image for preprocessing, recognizing or code generation."
    let image_source_path = Option.get image_source_path
    printfn "Some step of task can spend few minutes or more. Please be patient."
    printfn "Loading image \"%s\"..." image_source_path
    let image_source = loadImage image_source_path
    if image_source.IsNone then
        errorf "Can't load image \"%s\"." image_source_path
    let image_source = Option.get image_source
    let inline readOptionWithDefault n = readOptionWithDefault commandLineParser n
    let image_preprocessing_border = readOptionWithDefault "preprocess-image-border-value" 0.5 Double.TryParse
    printfn "Image preprocessing with border value: %f..." image_preprocessing_border
    let bool_image_source = imageToBoolImage image_source <| floatToColorByteComponent image_preprocessing_border
    if imageIsSmall bool_image_source then
        errorf "Source image \"%s\" too small." image_source_path
    let preprocessed_image_path = commandLineParser.parameterizedArg "save-preprocessed-image-to-file"
    if preprocessed_image_path.IsSome then
        let preprocessed_image_path = preprocessed_image_path.Value
        printfn "Saving preprocessed image to \"%s\"..." preprocessed_image_path
        use monochromed_system_image = imageToSystemImage <| boolImageToMonochrome bool_image_source
        try
            monochromed_system_image.Save preprocessed_image_path
        with _ -> errorf "Can't save silhouette preprocessed image to file \"%s\". " preprocessed_image_path
    printfn "Prepere to silhouette finding. Draw white rectangle on all image edges..."
    let bool_image_source = replaceBorder true bool_image_source
    use image_for_view = imageToSystemImage (boolImageToMonochrome bool_image_source)
    printfn "Silhouette finding..."
    let silhouetteStartPoint = findXYWithFirstWhiteBeforeBlack bool_image_source
    let showFormWithImage = showFormWithImage commandLineParser
    if silhouetteStartPoint.IsNone then
        printfn "Error. Can't find silhouette. Try change value by command line option /preprocess-image-border-value."
        showFormWithImage image_for_view
        errorf "Exit with failure."
    let silhouetteStartPoint = silhouetteStartPoint.Value
    printfn "Silhouette edges detecting..."
    let edgesPoints_int = getSilhouettePointsListForBorderedImage bool_image_source silhouetteStartPoint
    let edgesPoints = List.map XYtoFloatXY edgesPoints_int
    let edgesPoints_Point = List.map XYtoPoint edgesPoints_int |> List.toArray
    let longLinesPoints = 
        if commandLineParser.switchArg "disable-line-processing" then
            None
        else
            printfn "Silhouette long lines detecting..."
            let lastProgress = ref 0
            let progressViewer = if commandLineParser.switchArg "disable-progress-bar" then
                                        (fun _ _ -> ())
                                    else
                                        (fun processedPoints pointsAmount -> 
                                            let currentProgress = int <| 100. * float processedPoints/float pointsAmount 
                                            if !lastProgress<>currentProgress then 
                                                lastProgress:=currentProgress
                                                printf "\rProcessed: %i%%" currentProgress)
            let line_quality = readOptionWithDefault "line-quality" 3. Double.TryParse
            pointsToLines progressViewer line_quality edgesPoints |> 
            Some |>
            callback_and_return (printf "\r                             \r")
    let longLinesPoints_Point = Option.map (List.map XYtoPoint >> List.toArray) longLinesPoints
    Option.iter (generateCodeFile commandLineParser edgesPoints longLinesPoints) (commandLineParser.parameterizedArg "save-generated-code-to-file")
    if not <| commandLineParser.switchArg "disable-view-result" then 
        printfn "Drawing..."
        if not <| commandLineParser.switchArg "disable-view-silhouette-finding" then 
            drawPolyLineAtSystemImage image_for_view Color.Aqua [|Point(fst silhouetteStartPoint, snd silhouetteStartPoint); Point(0,0)|]
        if not <| commandLineParser.switchArg "disable-view-silhouette-edges" then 
            drawPolyLineAtSystemImage image_for_view Color.Green edgesPoints_Point
        if not <| commandLineParser.switchArg "disable-view-silhouette-long-lines" then 
            Option.iter (drawPolyLineAtSystemImage image_for_view Color.Red) longLinesPoints_Point
    commandLineParser.checkUnusedArguments()
    showFormWithImage image_for_view
    printfn "Successfull exit."
    0