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

let dup = fun x -> x, x
let dupf = fun f x -> Array.append x (f x)
let notAllZeroes f arr =
  match Array.forall ((=) 0 ) (arr) with
  | false -> Some (f arr)
  | true -> None

let flip f a b = f b a
let extend (arrs: int array array) =
  Array.map (Array.head) arrs
  |> Array.rev
  |> Array.reduce (flip (-))
  
  
parsed
|> Array.map (fun x -> Array.append [|x|]  ((Array.unfold (notAllZeroes (getdifferences >> dup))x)))
|> Array.sumBy extend