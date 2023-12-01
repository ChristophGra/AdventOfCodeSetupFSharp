open System
open System.IO
open Microsoft.FSharp.Core
let input =
  File.ReadAllLines @"AoC\AoC2023\day01\Input.txt"
//  """two1nine
//eightwothree
//abcone2threexyz
//xtwone3four
//4nineeightseven2
//zoneight234
//7pqrstsixteen""".ReplaceLineEndings("\n").Split("\n") 

let extractNumbers (str: string) =
  str
    .Replace("one","o1e")
    .Replace("two","t2p")
    .Replace("three","t3e")
    .Replace("four","f4r")
    .Replace("five","f5e")
    .Replace("six","s6x")
    .Replace("seven","s7n")
    .Replace("eight","e8t")
    .Replace("nine","n9e")
  |> List.ofSeq 
  |> List.filter (Char.IsNumber)
  |> fun x -> (List.head x |> string |> int), (List.rev x |> List.head |>  string |> int)
  |> fun (a,b) -> a * 10 + b
  
  
let result =
  Array.map extractNumbers input
  |> Array.sum
  
  
result