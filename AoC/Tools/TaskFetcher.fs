module AoC.Tools.TaskFetcher
open System
open System.IO
open System.Net
open System.Text.RegularExpressions
open System.Threading

let FetchAllTasks cookie =
    for year in 2015..DateTime.Now.Year do
        for day in 1..25 do
            let date = DateTime.Parse($"{year}-12-{day}")
            if date <= DateTime.Now.Date.AddDays(1.0)  && not <| (File.Exists $"""./AoC{year}\day{day}\Task.html""" ) then
                Console.WriteLine($"Starting Request for year {year}, day {day}")
                let request = HttpWebRequest.Create($"https://adventofcode.com/{year}/day/{day}")
                request.Headers.Add("cookie:" + cookie)
                let response = (request.GetResponse() :?> HttpWebResponse)
                if response.StatusCode = HttpStatusCode.OK then
                    let reader = new StreamReader(response.GetResponseStream())
                    let text =
                        reader
                            .ReadToEnd()
                            |> fun x -> Regex.Replace(x,"""/static/style.css\?(\d+)""","""../../Tools/static/style$1.css""")
                    File.WriteAllText( $""".\AoC{year}\day{day}\Task.html""", text)
                    reader.Close()
                    Console.WriteLine "Sleeping"
                    Thread.Sleep(60 * 1000)
                else
                    Console.WriteLine $"Some Error occured on year {year} day {day}"
                    exit(1)
                    