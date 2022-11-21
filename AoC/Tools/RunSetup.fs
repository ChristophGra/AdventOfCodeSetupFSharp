module AoC.Tools.RunSetup
open System.IO
open AoC.Tools
open AoC.Tools.HelperMethods

open System

let cookie =
    GetCookie()
    
    
Directory.SetCurrentDirectory((*SET YOUR DESIRED PATH HERE*))
CompleteSetup.setupFolders cookie
TaskFetcher.FetchAllTasks cookie
InputFetcher.FetchAllInputs cookie
CleanUp.rewriteCSSRef ()
    
    
Console.WriteLine "setup"