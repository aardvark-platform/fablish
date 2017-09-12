#load @"paket-files/build/aardvark-platform/aardvark.fake/DefaultSetup.fsx"

open Fake
open System
open System.IO
open System.Diagnostics
open Aardvark.Fake


do Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

DefaultSetup.install ["src/Fablish.sln"]

Target "Tests" (fun () ->
    Fake.NUnitSequential.NUnit (fun p -> { p with ToolPath = @"packages\NUnit.Runners\tools"
                                                  ToolName = "nunit-console.exe" }) [@"bin\Release\Aardvark.Base.Incremental.Tests.exe"]
)

Target "Statistics" (fun () ->
    let fsFiles = !!"src/**/*.fs"  

    let mutable stats = Map.empty
    for f in fsFiles do
        tracefn "file: %A" f
        ()



)

let libs = [
    "FSharp.Compiler.Service.dll"
    //"FSharp.Compiler.Service.MSBuild.v12.dll"
    "FSharp.Core.dll"
    "System.Collections.Immutable.dll"
    "System.Reflection.Metadata.dll"
    "System.Runtime.dll"
]

Target "Merge" (fun () -> 
    ILMergeHelper.ILMerge (fun p -> 
        { p with TargetKind = TargetKind.Exe
                 SearchDirectories = [@"src/fablish-hmr/bin/Debug/";@"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5";@"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\Facades"]
                 TargetPlatform = @"v4,C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5"
                 ToolPath = @"C:\Program Files (x86)\Microsoft\ILMerge\ILMerge.exe"
                 Libraries = libs })  "merged.exe" @"src/fablish-hmr/bin/Debug/fablish-hmr.exe"
)




entry()
