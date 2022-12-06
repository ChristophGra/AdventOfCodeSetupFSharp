open System.IO
let input =
  (File.ReadAllText @"AoC\AoC2022\day04\Input.txt").Replace("\r\n", "\n").Split("\n")
  |> Array.map (fun x -> x.Split(",") |> Array.map (fun y -> y.Split("-") |> (fun z -> Set.ofArray [|int z[0] ..int z[1]|])) |> fun a -> a[0], a[1])
  |> Array.sumBy (fun (first,second) -> if Set.intersect first second |> Set.isEmpty |> not then 1 else 0)
