module AoC.Tools.RefetchTask
open System
open System.IO
open System.Net
open System.Text.RegularExpressions


let fetchTaskOfDay cookie year day =
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
                    