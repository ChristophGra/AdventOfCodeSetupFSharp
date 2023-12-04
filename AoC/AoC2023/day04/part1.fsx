open System.IO


let input =
  File.ReadAllLines @"AoC\AoC2023\day04\Input.txt"
  |> Array.map (fun x -> x.Split(":"))
  |> Array.map (fun x -> x[0], x[1].Split("|"))
  |> Array.map (fun x -> fst x, (snd x)[0], (snd x)[1])
   
   
   
let extractNums (str:string) =
  str.Split(" ")
  |> Array.where (fun x -> String.length (x.Trim()) > 0)
  |> Array.map int

input
|> Array.map (fun curr -> 
  let card, winning, having = curr
  extractNums winning, extractNums having)
|> Array.map (fun (win,have) -> Set.intersect (Set.ofSeq win) (Set.ofSeq have))
|> Array.where (Array.ofSeq >> Array.length >> (fun x -> x > 0))
|> Array.map Array.ofSeq
|> Array.map Array.length
|> Array.sumBy (fun x -> 2.0 ** ((x |> float) - 1.0) |> int)
//|> Array.sumBy (Array.ofSeq >> Array.length >> (fun cnt -> 2.0 ** cnt)>> int)
