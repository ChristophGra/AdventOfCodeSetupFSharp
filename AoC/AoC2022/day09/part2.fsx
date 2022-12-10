open System
open System.IO
open System.Text.RegularExpressions
let input =
  File.ReadAllLines @"AoC\AoC2022\day09\Input.txt"
  
let parse lines =
  let reg = Regex("(?<direction>.) (?<steps>\d+)")
  let mapElem str =
    let res = reg.Match str
    if res.Success then
      Some (res.Groups["direction"].Value, int res.Groups["steps"].Value)
    else
      None
  Array.choose mapElem lines
  
let traverseHead inp =
  let doMove arr move =
    let (x,y) = Array.last arr
    match move with
    | "L", moves -> Array.map (fun  idx -> x - idx, y) [|1..moves|] 
    | "R", moves -> Array.map (fun  idx -> x + idx, y) [|1..moves|] 
    | "U", moves -> Array.map (fun  idx -> x, y - idx) [|1..moves|] 
    | "D", moves -> Array.map (fun  idx -> x, y + idx) [|1..moves|]
    | str,_ -> raise (new Exception(str))
  parse inp
  |> Array.scan doMove [|(0,0)|]
  |> Array.collect id
  
let headPositions inp =  
  traverseHead inp
  
let getTailPositions positions =
  let folder (tailx,taily) (newHeadx,newHeady) : (int * int) =
    let newTail =
      match newHeadx - tailx , newHeady - taily  with
      | x,y when abs x + abs y > 1 && (abs x > 1 || abs y > 1) -> (tailx + sign x,taily + sign y)
      | x,y -> (tailx,taily )
    newTail
  Array.scan folder (0,0) positions
  

headPositions input
|> getTailPositions
|> getTailPositions
|> getTailPositions
|> getTailPositions
|> getTailPositions
|> getTailPositions
|> getTailPositions
|> getTailPositions
|> getTailPositions
|> Array.distinct
|> Array.length