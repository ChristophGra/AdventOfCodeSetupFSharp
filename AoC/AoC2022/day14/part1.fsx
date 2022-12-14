open System
open System.IO
let input =
  File.ReadAllLines @"AoC\AoC2022\day14\Input.txt"
  |> Array.map (fun (x:string) -> x.Split(" -> ") |> Array.map (fun (y:string) -> y.Split(",") |> Array.map (int) |> fun x -> x[1], x[0]))
let input2 =
  """498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9""".Split("\n")
  |> Array.map (fun (x:string) -> x.Split(" -> ") |> Array.map (fun (y:string) -> y.Split(",") |> Array.map (int) |> fun x -> x[1], x[0]))
let minx =
  Array.collect (Array.map fst) input
  |> Array.min
  
let maxx =
  Array.collect (Array.map fst) input
  |> Array.max
  
let miny =
  Array.collect (Array.map snd) input
  |> Array.min
  
let maxy =
  Array.collect (Array.map snd) input
  |> Array.max
  
let walls =
  Array2D.createBased 0 miny (maxx + 1) (maxy - miny + 1) false
  
  
let setupWalls =
  let setLineSegment ((x1,y1),(x2,y2))=
    let xs =
      if x1 <= x2 then
        [|x1..x2|]
      else
        [|x2..x1|]
    let ys =
      if y1 <= y2 then
        [|y1..y2|]
      else
        [|y2..y1|]
    Array.allPairs xs ys
    |> Array.iter (fun (x,y) ->
      //Console.WriteLine $"{x},{y}"
      walls[x,y] <- true)
  Array.map (Array.pairwise) input
  |> Array.iter (Array.iter setLineSegment)
  

let rec flowSand (currentx,currenty) =
  try
    if not (walls[currentx + 1,currenty] ) then
      flowSand (currentx + 1,currenty)
    else if not (walls[currentx + 1,currenty - 1]) then
      flowSand (currentx + 1,currenty - 1)
    else if not (walls[currentx + 1,currenty + 1]) then
      flowSand (currentx + 1,currenty + 1)
    else
      walls[currentx, currenty] <- true
      true
  with
     _ ->
        Console.WriteLine ($"{currentx},{currenty}")
        false
    
Console.WriteLine $"{Array2D.length1 walls}"
Console.WriteLine $"{Array2D.length2 walls}"
Console.WriteLine $"{Array2D.base1 walls}"
Console.WriteLine $"{Array2D.base2 walls}"
Console.WriteLine $"{Array2D.base1 walls + Array2D.length1 walls}"     
Console.WriteLine $"{Array2D.base2 walls + Array2D.length2 walls}"
let cnt =
  Seq.takeWhile (fun _ -> flowSand (0,500)) (Seq.unfold (fun x -> (x, x + 1) |> Some) 0)
  |> Seq.length

Array2D.iteri (fun _ idxy x ->
  File.AppendAllText (@"AoC\AoC2022\day14\Output.txt",(if x then "X" else " "))
  if idxy = maxy then
    File.AppendAllText (@"AoC\AoC2022\day14\Output.txt","\n")
  ) walls

cnt