open System.Ipen System.IO
open System
open System.Net
open System.Text.RegularExpressions

let input =
    File.ReadAllText "AoC/AoC2022/day01/Input.txt"
    |> fun x -> x.Replace ("\r\n","\n")
    |> fun x -> x.Split "\n\n"
    |> Array.map (fun x -> x.Split "\n")
    |> Array.map (Array.choose (fun x -> match System.Int32.TryParse x with true, value -> Some value | false, _ -> None))
    
    
let results =
    Array.map (fun x -> Array.sum x) input
    |> Array.sortDescending
    |> Array.take 3
    |> Array.sum


