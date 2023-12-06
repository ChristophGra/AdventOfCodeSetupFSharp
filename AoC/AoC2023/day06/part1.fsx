open System.IO


let tinput =
    """Time:      7  15   30
Distance:  9  40  200"""
let input =
  File.ReadAllText @"AoC\AoC2023\day06\Input.txt"
  
let processed =
    input.Split("\n")
    |> Array.map (fun (x:string) -> x.Split(" ") |> Array.where ((<>) ""))
    |> Array.map Array.tail
    |> Array.map (Array.map int)
    |> fun x -> Array.zip (x[0]) (x[1])
    
    
let runSims (time:int,goal:int) =
    Seq.initInfinite (fun i -> (time - i) * i > goal)
    |> Seq.skipWhile not
    |> Seq.takeWhile id
    |> Seq.length
Array.map runSims processed
|> Array.reduce (*)