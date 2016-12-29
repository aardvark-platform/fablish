open System
open System.IO
open System.Text
open System.Threading

open System.IO.Compression

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Interactive.Shell

let stronglyReadFile (fileName : string) =
    if not <| File.Exists fileName then failwithf "interactive file not found: %s" fileName
    let mutable worked = None
    while Option.isNone worked do
        try 
            worked <- Some <| File.ReadAllText fileName
        with e -> 
            printfn "failed. retry."
            ()
    worked.Value

module FSI =
    open System.Diagnostics
    let defaultFsi = @"C:\Program Files (x86)\Microsoft SDKs\F#\4.0\Framework\v4.0\FsiAnyCPU.exe"

    let spawn (path : string) =
        let procStart = new ProcessStartInfo()
        procStart.FileName <- path
        procStart.RedirectStandardOutput <- true
        procStart.RedirectStandardInput <- true
        procStart.RedirectStandardError <- true
        procStart.RedirectStandardInput <- true
        let proc = Process.Start(procStart)
        proc.OutputDataReceived.Add(fun s -> printfn "[fsi out] %s" s.Data)
        proc.ErrorDataReceived.Add(fun s -> printfn "[fsi err] %s" s.Data)
        proc.StandardInput, proc

    type FSI(path : string) =
        let stdin,proc = spawn path

        member x.Execute(code : string) =
            stdin.Write(code)
            stdin.Write(";;")

        member x.Kill() = proc.Kill()

let archPlatform () =
    let arch = if IntPtr.Size = 8 then "AMD64" else "x86"
    let plat = 
        match Environment.OSVersion.Platform with
            | PlatformID.MacOSX -> "mac"
            | PlatformID.Unix -> "linux"
            | _ -> "windows"
    plat, arch
            
let unpackNativeDependencies (assemblyFileName : string) (baseDir : string) =
   let ass = Mono.Cecil.AssemblyDefinition.ReadAssembly assemblyFileName
   let nativeResources =
         ass.MainModule.Resources 
         |> Seq.toList
         |> List.choose (fun a -> 
            match a with
                | :? Mono.Cecil.EmbeddedResource as r when a.Name = "native.zip" -> 
                    Some <| r
                | _ -> None
         )
   match nativeResources with
    | [] -> printfn "no native resources found for assembly: %s" assemblyFileName
    | native :: [] -> 
        let platform,arch = archPlatform()
        let copyPath = sprintf "%s/%s" platform arch
        printfn "found native resources"
        use stream = native.GetResourceStream()
        let zip = new ZipArchive(stream)
        for e in zip.Entries do
            let name = e.FullName.Replace('\\','/')
            if name.StartsWith copyPath then
                let name = name.Substring(copyPath.Length)
                let localComponents = name.Split([|'/';'\\'|], StringSplitOptions.RemoveEmptyEntries)
                if localComponents.Length <> 0 then
                    let localTarget = Path.Combine(localComponents)
                    let outputPath = Path.Combine(baseDir, localTarget)
                    let d = Path.GetDirectoryName outputPath
                    if Directory.Exists d |> not then Directory.CreateDirectory d |> ignore
                    e.ExtractToFile(outputPath)
                    printfn "extracting: %s -> %s" e.FullName outputPath
            else
                printfn "skipping: %s" e.FullName
        ()
    | _ -> printfn "more than one native resource embedding not supported."


let run (fileName : string) (workingDirectory : string) =
    
    printfn "watching: %s in workingDirectory: %s" fileName workingDirectory

    let sbOut = new StringBuilder()
    let sbErr = new StringBuilder()
    let inStream = new StringReader("")
    let outStream = new StringWriter(sbOut)
    let errStream = new StringWriter(sbErr)
    System.Environment.CurrentDirectory <- workingDirectory
    let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
   
    let sourceDir = Path.GetDirectoryName fileName
    let argv = [| Path.GetDirectoryName fileName |]
    let fsharpCorePath = @"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\FSharp.Core.dll"
    let mscorlib = @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\mscorlib.dll"
    let allArgs = 
        Array.append argv [| "--gui-"; "--noninteractive"; "--noframework"; 
                             "-r"; mscorlib
                             "-r"; fsharpCorePath
                             "-r"; "System.Windows.Forms.dll" |]
    let fsiSession = 
        try
            FsiEvaluationSession.Create(fsiConfig, allArgs, inStream, outStream, errStream)  
        with e -> 
            errStream.ToString() |> failwithf "could not construct session: %A"

    let fsiSession = FsiEvaluationSession.Create(fsiConfig, allArgs, inStream, outStream, errStream)  

    let getCode () =
        let code = stronglyReadFile fileName
        let patched = code.Replace("#I @\"..\\..\\bin\\Debug\"", sprintf "#I @\"%s\"" workingDirectory)
        let replacedDir = patched.Replace("__SOURCE_DIRECTORY__", sprintf "@\"%s\"" sourceDir)
        printfn "final patch code: %s" replacedDir
        replacedDir

    try
        let result = fsiSession.EvalInteraction (getCode())
        printfn "%s \n~> %A" (outStream.ToString()) result
    with e -> 
        failwithf "initial evaluation failed %s: %A" (errStream.ToString()) e.Message

    let evalExpressionTyped (text) = 
        match fsiSession.EvalExpression(text) with
        | Some value -> value.ReflectionValue |> unbox<'T>
        | None -> failwith "evalExpressionTyped failed."

    let eval () = 
        try
            let initialCode = getCode()
            let code        = initialCode.Replace("let w = new Form()","")
            printfn "cleaning up..."
            fsiSession.EvalInteraction("w.Controls.Clear() ") |> printfn "%A"
            fsiSession.EvalInteraction("browser.Dispose() ")|> printfn "%A"

            printfn "cleanup: %s" (outStream.ToString())
            printfn "evacuating..."
            let location : System.Drawing.Point = evalExpressionTyped "w.Location"
            System.Windows.Forms.Application.DoEvents()
            printfn "injecting..."
            let result = fsiSession.EvalInteraction code
            printfn "%s \n~> %A" (outStream.ToString()) result
        with e -> 
            printfn "eval() failed. error stream: %s" (errStream.ToString())

    let mutable work = 0
    let nfilter = NotifyFilters.LastWrite 
    use watcher = new FileSystemWatcher(Path.GetDirectoryName(fileName))
    watcher.NotifyFilter <- nfilter
    watcher.EnableRaisingEvents <- true
    let mutable lastEvt = DateTime.Now
    watcher.Changed.Add(fun s -> 
        if DateTime.Now - lastEvt > TimeSpan.FromSeconds(1.0) then
            lastEvt <- DateTime.Now
            if s.FullPath = Path.GetFullPath(fileName) then
                printfn "new work: %A" <| Interlocked.CompareExchange(&work,1,0) 
            
    )


    while true do
        if Interlocked.CompareExchange(&work,0,1) = 1 then
            eval ()
        else
            System.Windows.Forms.Application.DoEvents()

open Fablish

[<EntryPoint>]
let main argv = 

    ChromiumUtilities.unpackCef()
    Chromium.init argv
    System.Diagnostics.Debug.WriteLine(sprintf "URDAR: %A" argv)
    //let file = @"C:\Development\aardvark-fablish\src\Aardvark.UI.Fablish\Interactive.fsx"
    //let workingDirectory = @"C:\Development\aardvark-fablish\bin\Release"
    match argv with
        | [|file;outputDir|] -> 
            let path = Path.GetFullPath outputDir
            System.Environment.CurrentDirectory <- path
            run ( Path.GetFullPath(file) ) path
        | _ -> () //failwith "usage: file.fsx outputDir"
            
    0 
