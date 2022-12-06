open System.IO


let StartOf cnt =
  Array.windowed cnt
  >> Array.mapi (fun idx x->x,idx)
  >> Array.find (fun (chars,idx) -> Array.distinct chars |> Array.length = cnt)
  >> snd
  >> (+) cnt
let input =
  (File.ReadAllText @"AoC\AoC2022\day06\Input.txt").ToCharArray()

let res =
  StartOf 14 input
  