open System
open System.IO
let input =
  File.ReadAllLines @"AoC\AoC2022\day08\Input.txt"
  |> Array.mapi (fun idx1 x -> x.ToCharArray() |>  Array.mapi (fun idx2 y -> Int32.Parse($"{y}"),(idx1,idx2)))
  |> array2D
  
   
let getCross (inp: (int * (int * int))[,]) (x,y)=
    let len = inp.Length - 1
    [|
      inp[0..x,y]   |> Array.map fst |> Array.rev |> Array.tail |> List.ofArray
      inp[x..len,y] |> Array.map fst              |> Array.tail |> List.ofArray
      inp[x,0..y]   |> Array.map fst |> Array.rev |> Array.tail |> List.ofArray
      inp[x,y..len] |> Array.map fst              |> Array.tail |> List.ofArray
    |]
    
let calculateVisible (inp: (int * (int * int))[,]) (x,y) =
  let treeHeight = fst inp[x,y]
  let views = getCross inp (x,y)
  let rec folder maxheight trees =
    match trees with
    | h::t ->
      if h >= maxheight then
          1
      else
        1 + folder maxheight t
    | [] -> 0
  let score =
    Array.map (folder treeHeight) views
    |> Array.reduce (*)
  score, (x,y)
  
let scores = 
  Array.collect (fun x -> Array.map (fun y -> calculateVisible input (x,y)) [|0..(Array2D.length1 input) - 1|]) [|0..(Array2D.length2 input) - 1|]
  |> Array.sortByDescending fst
  |> Array.head
  |> fst