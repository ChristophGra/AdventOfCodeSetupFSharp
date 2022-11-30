module AoC.Tools.CleanUp
open System
open System.IO
open System.Text.RegularExpressions

let rewriteCSSRef () =
    for year in 2015..DateTime.Now.Year do
        for day in 1..25 do
            if File.Exists $"""./AoC{year}\day{day}\Task.html""" then
                File.ReadAllText $"""./AoC{year}\day{day}\Task.html"""
                |> fun x -> Regex.Replace(x,"""/static/style.css\?(\d+)""","""../../Tools/static/style$1.css""")
                |> fun x -> File.WriteAllText($"""./AoC{year}\day{day}\Task.html""",x)