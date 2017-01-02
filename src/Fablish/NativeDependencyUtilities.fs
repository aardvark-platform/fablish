namespace Fablish

open System
open System.IO
open System.Diagnostics
open System.Net
open System.Collections.Generic

open Xilium.CefGlue.Wrapper
open Xilium.CefGlue


open ICSharpCode.SharpZipLib.Core;
open ICSharpCode.SharpZipLib;
open ICSharpCode.SharpZipLib.Tar;
open ICSharpCode.SharpZipLib.BZip2

module ChromiumUtilities =

    let getCurrentArch () =
        let arch = if IntPtr.Size = 8 then 64 else 32
        let plat = 
            match Environment.OSVersion.Platform with
                | PlatformID.MacOSX -> "mac"
                | PlatformID.Unix -> "linux"
                | _ -> "windows"
        plat, arch
        
    let unbz2 (tarbz2 : string)  = 
        let tar = Path.Combine [| Path.GetDirectoryName tarbz2; Path.GetFileNameWithoutExtension tarbz2 |]
        printfn "[install] unbz2: %s to %s" tarbz2 tar
        use fs = new FileStream(tarbz2, FileMode.Open, FileAccess.Read)
        use fsOut = File.Create(tar)
        BZip2.Decompress(fs,fsOut,true)
        printfn "[install] unbz2 done."
        tar

    let untar (sourceFile : string) (destFolder : string) = 
        printfn "[install] untar %s to %s" sourceFile destFolder
        use s = File.OpenRead(sourceFile)
        use tar = TarArchive.CreateInputTarArchive(s)
        tar.ExtractContents(destFolder)
        printfn "[install] untar done."
        tar.Close()

    let copyDir (sourcePath : string) (destPath : string) =
        for dir in Directory.GetDirectories(sourcePath, "*", SearchOption.AllDirectories) do
            let newDir = dir.Replace(sourcePath,destPath)    
            printfn "[install] creating dir: %s" newDir
            Directory.CreateDirectory(newDir) |> ignore

        for newPath in Directory.GetFiles(sourcePath, "*.*", SearchOption.AllDirectories) do
            File.Copy(newPath, newPath.Replace(sourcePath,destPath), true)

    let downloadCefTo (url : string) (unpackDir : string) = 
        let tarbz = Path.GetFileName(url)
        let tar = Path.GetFileNameWithoutExtension tarbz
        let plainDirName = Path.GetFileNameWithoutExtension tar
        let tempDir = Path.GetTempPath()

        let downloadFile = Path.Combine [| tempDir; tarbz |]
        if File.Exists downloadFile then ()
        else
            use wc = new WebClient()
            printfn "[install] downloading: %s to %s" url downloadFile
            wc.DownloadFile(url,downloadFile)
        
        let targetDir = Path.Combine [| tempDir; plainDirName |]
        let tarFile = unbz2 downloadFile
        untar tarFile tempDir
        let release   = Path.Combine [| targetDir;  "Release" |]
        let resources = Path.Combine [| targetDir;  "Resources" |]
        printfn "[install] copy %s to %s" release unpackDir
        copyDir release unpackDir
        copyDir resources unpackDir

        //File.Delete(downloadFile)
        Directory.Delete(targetDir,true)
        printfn "[install] downloadCefTo %s to %s done." url downloadFile

    let unpackDependencies (id,version) (deps : seq<KeyValuePair<string*int,string>>) (workingDir : string) =
        let install () =
            let currentArch = getCurrentArch ()
            match deps |> Seq.tryFind (fun (KeyValue(arch, url)) -> arch = currentArch) with
                | Some (KeyValue((os,arch),url)) -> 
                    printfn "[install] Install target: %A, which will be fetched from: %s" (os,arch) url
                    downloadCefTo url workingDir
                    File.WriteAllText(id,version)
                | None -> 
                    failwithf "no native dependency for current platform: %A, candidates are: %A" currentArch (deps |> Seq.toList |> List.map (fun (KeyValue(k,v)) -> k,v))
        
        let c = Console.ForegroundColor
        if File.Exists id then
            let installedVersion = File.ReadAllText id
            if installedVersion = version then 
                printfn "[install] cef (%s) found beside current executable. skipping installation." version
            else
                Console.ForegroundColor <- ConsoleColor.Red
                printfn "[install] Cef version %s was found beside your executable but this application demands for %s" installedVersion version
                printfn "[install] I will automatically fetch the correct version and install it besides your application..." 
                install ()
        else 
            Console.ForegroundColor <- ConsoleColor.Green
            printfn "[install] No cef build was found besides your application... "
            printfn "[install] I will automatically fetch the correct version(%s) and install it besides your application..." version
            install ()
        Console.ForegroundColor <- c

    let unpackCef () =
        unpackDependencies ("cef", "3.2883.1539") Xilium.CefGlue.NativeDependencies.NativeDependencyPaths System.Environment.CurrentDirectory
