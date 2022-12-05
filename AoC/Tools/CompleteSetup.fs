module AoC.Tools.CompleteSetup
open System
open System.IO
open System.Net
open System.Net.Http
open System.Net.Http.Headers
open System.Text
open System.Text.RegularExpressions
open System.Threading

        
let Request cookie (url:string) filePath (rewriter:string -> string) =
    let request = new HttpClient()
    request.BaseAddress <- Uri(url)
    request.DefaultRequestHeaders.TryAddWithoutValidation("User-Agent","""https://github.com/ChristophGra/AdventOfCodeSetupFSharp""") |> ignore
    request.DefaultRequestHeaders.TryAddWithoutValidation("cookie","session=" + cookie) |> ignore
    let response = request.Send(new HttpRequestMessage())
    if response.StatusCode = HttpStatusCode.OK then
        let buffer = Array.init 1024 (fun _ -> 0uy)
        let reader = response.Content.ReadAsStream()
        let mutable count = 0
        let builder = new StringBuilder()
        reader.Position <- 0
        while (reader.Position < reader.Length) do
            count <- reader.Read buffer
            builder.Append (Encoding.UTF8.GetString(buffer,0,count))
            |> ignore
        let text = builder.ToString()
        File.WriteAllText(filePath, text.Trim())
        reader.Close()
        false
    else
        true

let rewriteStylesheetToLocal cookie input =
    let stylesheetNum = (Regex.Match (input, @"/static/style.css\?(\d+)")).Groups.[1]
    if not (Directory.Exists "AoC/Tools") then
        Directory.CreateDirectory "AoC/Tools" |> ignore
    if not (Directory.Exists "AoC/Tools/static") then
        Directory.CreateDirectory "AoC/Tools/static" |> ignore
    if not (Directory.Exists "AoC/Tools/static/style") then
        Directory.CreateDirectory "AoC/Tools/static/style" |> ignore
    if not (File.Exists $"""AoC/Tools/static/style/style{stylesheetNum}.css""") then
        if Request cookie $"https://adventofcode.com/static/style.css?{stylesheetNum}" $"""Aoc/Tools/static/style/style{stylesheetNum}.css""" id then
            exit(1) |> ignore
        else
            Console.WriteLine($"Request for stylesheet {stylesheetNum} sent")
    Regex.Replace(input,"""/static/style.css\?(\d+)""","""../../Tools/static/style/style$1.css""")

let setupFolders cookie =
    let sleepTime = 60 //60 seconds wait time so that you are a good citizen
    for year in 2015..HelperMethods.MaxYear() do
        if not (Directory.Exists $"""AoC/AoC{year}""") then
            Directory.CreateDirectory($"""AoC/AoC{year}""") |> ignore
        let req = Request cookie
        for day in 1..25 do
            let dayString = day.ToString("00")
            Console.WriteLine $"year {year} day {dayString}"
            if not (Directory.Exists $"""AoC/AoC{year}/day{dayString}""") then
                Directory.CreateDirectory($"""AoC/AoC{year}/day{dayString}""") |> ignore
            if not (File.Exists $"""AoC/AoC{year}/day{dayString}/part1.fsx""") then
                File.WriteAllText($"""AoC/AoC{year}/day{dayString}/part1.fsx""",$"""open System.IO
let input =
  File.ReadAllText @"\AoC\AoC{year}\day{dayString}\Input.txt" """)
            if not (File.Exists $"""AoC/AoC{year}/day{dayString}/part2.fsx""") then
                File.WriteAllText($"""AoC/AoC{year}/day{dayString}/part2.fsx""",$"""open System.IO
let input =
  File.ReadAllText @"\AoC\AoC{year}\day{dayString}\Input.txt" """)
            if not (File.Exists $"""AoC/AoC{year}/day{dayString}/Task.html""" ) then
                if req $"https://adventofcode.com/{year}/day/{day}" $"""AoC/AoC{year}/day{dayString}/Task.html""" (rewriteStylesheetToLocal cookie) then
                    exit(1) |> ignore
                else
                    Console.WriteLine($"Request for year {year} and day {day} for Task sent")
                    Thread.Sleep(sleepTime * 1000 / 2)
            if not (File.Exists $"""./AoC/AoC{year}/day{dayString}/Input.txt""") then
                if req $"https://adventofcode.com/{year}/day/{day}/input" $"""AoC/AoC{year}/day{dayString}/Input.txt""" id then
                    exit(1) |> ignore
                else
                    Console.WriteLine($"Request for year {year} and day {day} for input sent")
                    Thread.Sleep(sleepTime * 1000 / 2)
            
