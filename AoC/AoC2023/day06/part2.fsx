open System.IO


let tinput =
    """Time:      7  15   30
Distance:  9  40  200"""
let input =
  File.ReadAllText @"AoC\AoC2023\day06\Input.txt"
  
let processed =
    input.Split("\n")
    |> Array.map (fun (x:string) -> x.Replace(" ","").Split(":") |> Array.where ((<>) ""))
    |> Array.map Array.tail
    |> Array.map (Array.map uint64)
    |> fun x -> Array.zip (x[0]) (x[1])
    
    
let runSims (time:uint64,goal:uint64) =
    Seq.initInfinite (fun i -> (time - uint64 i) * uint64 i > goal)
    |> Seq.skipWhile not
    |> Seq.takeWhile id
    |> Seq.length
    
Array.map runSims processed
|> Array.reduce (*)