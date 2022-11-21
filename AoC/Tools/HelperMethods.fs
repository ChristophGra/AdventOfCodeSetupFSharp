module AoC.Tools.HelperMethods

open System
open System.IO

let MaxYear () =
    DateTime.Now.AddYears((if DateTime.Now.Month < 12 then (-1) else (0))).Year
    
let GetCookie () =
    File.ReadAllText """../../../Tools/cookie.txt"""
    
    