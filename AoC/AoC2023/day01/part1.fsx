open System
open System.IO
open Microsoft.FSharp.Core
let input =
  File.ReadAllLines @"AoC\AoC2023\day01\Input.txt"
 

let extractNumbers str =
  List.ofSeq str
  |> List.filter (Char.IsNumber)
  |> fun x -> (List.head x |> string |> int), (List.rev x |> List.head |>  string |> int)
  |> fun (a,b) -> a * 10 + b
  
  
let result =
  Array.map extractNumbers input
  |> Array.sum
  
  
result