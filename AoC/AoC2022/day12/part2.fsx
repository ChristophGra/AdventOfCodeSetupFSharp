open System
open System.Collections.Generic
open System.IO



let input =
  File.ReadAllLines @"AoC\AoC2022\day12\Input.txt"
let getfield text =
  let field = Array2D.create (Array.length text) (Array.head text |> fun (x:string) -> x.ToCharArray() |> Array.length) 0uy
  let mutable startLoc = (0,0)
  Array.iteri
    (fun x (line:string) ->
    Array.iteri
      (fun y chara ->
      if chara = 'E' then
        Console.WriteLine $"S{x}{y}"
        startLoc <- x,y
      
      field[x,y] <- if chara = 'E' then 27uy else if chara = 'S' then 0uy else byte chara - byte 'a' + 1uy)
      (line.ToCharArray()))
      text
  field, startLoc

let heightmap, startLoc =
  getfield input
let visited =
  Array2D.create (Array2D.length1 heightmap) (Array2D.length2 heightmap) Int32.MaxValue
  
visited[fst startLoc, snd startLoc] <- 0

let getNeighbours (visited:int[,]) (heightmap:byte[,]) ((currx,curry): int * int) =
  let currentCount = visited[currx,curry] + 1
  let currentHeight = heightmap[currx,curry] - 1uy
  let maxx = Array2D.length1 visited - 1
  let maxy = Array2D.length2 visited - 1
  let xs = Array.map (fun x -> x,curry) [|currx - 1..currx + 1|] 
  let ys = Array.map (fun y -> currx,y) [|curry - 1..curry + 1|]
  let neighbours =
    Array.append xs ys
    |> Array.distinct
    |> Array.except [|currx,curry|]
    |> Array.where (fun (x,y) -> x >= 0 && x <= maxx)
    |> Array.where (fun (x,y) -> y >= 0 && y <= maxy)
    |> Array.where (fun (x,y) -> heightmap[x,y] >= currentHeight)
    |> Array.where (fun (x,y) -> visited[x,y] > currentCount)
  Array.iter (fun (x,y) -> visited[x,y] <- currentCount) neighbours
  neighbours
  
let rec moves (visited:int[,]) (heightmap:byte[,]) (nextMoves:(int * int) Queue) prev =
  if nextMoves.Count > 0 then 
    let (x,y) = nextMoves.Dequeue()
    if heightmap[x,y] = 1uy then
      visited[x,y] 
    else
      let neighbours = getNeighbours visited heightmap (x,y)
      Array.iter nextMoves.Enqueue neighbours
      moves visited heightmap nextMoves (x,y) 
  else
    -1
let q =
  new Queue<int * int>()
q.Enqueue(startLoc)
moves visited heightmap q startLoc