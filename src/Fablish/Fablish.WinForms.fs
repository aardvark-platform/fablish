namespace Fablish

open System
open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.Html
open Suave

module WinForms =

    open System.Windows.Forms
    open System.Threading

    let runControl (port : string) (app : App<'model,'msg, DomNode<'msg>>) =
        let url,t,ct = Fablish.serveLocally port app

        let browser = new WebBrowser()
        browser.Url <- Uri(url)
        browser.Dock <- DockStyle.Fill
        browser

module Chromium =

    open System.Windows.Forms
    open System.Threading

    open Xilium.CefGlue.Wrapper
    open Xilium.CefGlue

    type MyCefApp() =
        inherit CefApp()


    let mutable private initialized = false
    let l = obj()
    let init argv =
        lock l (fun _ -> 
            if not initialized then
                initialized <- true

                CefRuntime.Load()

                let settings = CefSettings()
                settings.MultiThreadedMessageLoop <- CefRuntime.Platform = CefRuntimePlatform.Windows;
                settings.SingleProcess <- false;
                settings.LogSeverity <- CefLogSeverity.Default;
                settings.LogFile <- "cef.log";
                settings.ResourcesDirPath <- System.IO.Path.GetDirectoryName(Uri(System.Reflection.Assembly.GetEntryAssembly().CodeBase).LocalPath);
                settings.RemoteDebuggingPort <- 1337;
                settings.NoSandbox <- true;
                let args = 
                    if CefRuntime.Platform = CefRuntimePlatform.Windows then argv
                    else Array.append [|"-"|] argv

                let mainArgs = CefMainArgs(argv)
                let app = MyCefApp()
                let code = CefRuntime.ExecuteProcess(mainArgs,app,IntPtr.Zero)
                if code <> -1 then System.Environment.Exit code

                CefRuntime.Initialize(mainArgs,settings,app,IntPtr.Zero)

                Application.ApplicationExit.Add(fun _ -> 
                    CefRuntime.Shutdown()
                )
                AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> 
                    CefRuntime.Shutdown()
                )
        )
    
    let runControl (port : string) (app : App<'model,'msg, DomNode<'msg>>) =
       // if not initialized then failwith "run Chromium.init argv before running a control."

        let url,t,ct,app = Fablish2.serveLocally port app

        let cleanup _ =
            printfn "[fablish] closing -> shutting down."; 
            ct.Cancel()
            printfn "[fablish] closed server."

        printfn "[fablish] serving fablish ui on: %s" url
        let browser = new Xilium.CefGlue.WindowsForms.CefWebBrowser(StartUrl=url)
        browser.Disposed.Add cleanup
        browser.Dock <- DockStyle.Fill
        browser

        