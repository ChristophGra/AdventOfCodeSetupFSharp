module AoC.Tools.CompleteSetup
open System
open System.IO
open System.Net
open System.Text.RegularExpressions
open System.Threading


let Request cookie (url:string) filePath (rewriter:string -> string) =
    let request = HttpWebRequest.Create(url)
    request.Headers.Add("cookie:" + cookie)
    let response = (request.GetResponse() :?> HttpWebResponse)
    if response.StatusCode = HttpStatusCode.OK then
        let reader = new StreamReader(response.GetResponseStream())
        let text = reader.ReadToEnd() |> rewriter
        File.WriteAllText(filePath, text)
        reader.Close()
        false
    else
        true

let rewriteStylesheetToLocal cookie input =
    let stylesheetNum = (Regex.Match (input, @"/static/style.css\?(\d+)")).Groups.[1]
    if not (Directory.Exists "Tools") then
        Directory.CreateDirectory "Tools" |> ignore
    if not (Directory.Exists "Tools/static") then
        Directory.CreateDirectory "Tools/static" |> ignore
    if not (File.Exists $"""Tools/static/style{stylesheetNum}.css""") then
        if Request cookie $"https://adventofcode.com/static/style.css?{stylesheetNum}" $"""Tools/static/style{stylesheetNum}.css""" id then
            exit(1) |> ignore
    Regex.Replace(input,"""/static/style.css\?(\d+)""","""../../Tools/static/style$1.css""")

let setupFolders cookie =
    let sleepTime = 60 //60 seconds wait time so that you are a good citizen
    for year in 2015..HelperMethods.MaxYear() do
        if not (Directory.Exists $"""AoC{year}""") then
            Directory.CreateDirectory($"""AoC{year}""") |> ignore
        let req = Request cookie
        for day in 1..25 do
            if not (Directory.Exists $"""AoC{year}/day{day}""") then
                Directory.CreateDirectory($"""AoC{year}/day{day}""") |> ignore
            if not (File.Exists $"""AoC{year}/day{day}/part1""") then
                File.WriteAllText($"""AoC{year}/day{day}/part1""","")
            if not (File.Exists $"""AoC{year}/day{day}/part2""") then
                File.WriteAllText($"""AoC{year}/day{day}/part2""","")
            if not (File.Exists $"""AoC{year}/day{day}/Task.html""" ) then
                if req $"https://adventofcode.com/{year}/day/{day}" $"""AoC{year}/day{day}/Task.html""" (rewriteStylesheetToLocal cookie) then
                    exit(1) |> ignore
            if not (File.Exists $"""./AoC{year}/day{day}/Input.txt""") then
                if cookie <> "" then
                    if req $"https://adventofcode.com/{year}/day/{day}/input" $"""./AoC{year}/day{day}/Input.txt""" id then
                        exit(1) |> ignore
            Thread.Sleep(sleepTime * 1000)
