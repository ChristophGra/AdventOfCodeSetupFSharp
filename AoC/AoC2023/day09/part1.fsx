open System.IO
let input =
  File.ReadAllText @"AoC\AoC2023\day09\Input.txt"
let tinput =
  """0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"""
let parsed =
  input.Split("\n")
  |> Array.map (fun x -> x.Split(" ") |> Array.map int)
  
let getdifferences arr =
  Array.pairwise arr
  |> Array.map (fun (a,b) ->  b - a)
  
Array.map getdifferences parsed

let dup f = fun x -> x, f x
let notAllZeroes arr =
  match Array.forall ((=) 0 ) (fst arr) with
  | false -> Some arr
  | true -> None
  
let extend (arrs: int array array) =
  Array.map (Array.last) arrs
  |> Array.rev
  |> Array.reduce (+)
  
parsed
|> Array.map (Array.unfold (dup getdifferences >> notAllZeroes))
|> Array.sumBy extend