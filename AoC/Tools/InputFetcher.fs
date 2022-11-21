module AoC.Tools.InputFetcher
open System
open System.IO
open System.Net
open System.Threading

let FetchAllInputs cookie =
    for year in 2015..DateTime.Now.Year do
        for day in 1..25 do
            let date = DateTime.Parse($"{year}-12-{day}")
            if date <= DateTime.Now.Date.AddDays(1.0)  && not <| (File.Exists $"""./AoC{year}\day{day}\Input.txt""" ) then
                Console.WriteLine($"Starting Request for year {year}, day {day}")
                let request = HttpWebRequest.Create($"https://adventofcode.com/{year}/day/{day}/input")
                request.Headers.Add("cookie:" + cookie)
                let response = (request.GetResponse() :?> HttpWebResponse)
                if response.StatusCode = HttpStatusCode.OK then
                    let reader = new StreamReader(response.GetResponseStream())
                    let text = reader.ReadToEnd()
                    File.WriteAllText( $""".\AoC{year}\day{day}\Input.txt""", text)
                    reader.Close()
                    Console.WriteLine "Sleeping"
                    Thread.Sleep(60 * 1000)
                else
                    Console.WriteLine $"Some Error occured on year {year} day {day}"
                    exit(1)
                    