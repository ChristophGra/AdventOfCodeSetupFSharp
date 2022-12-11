open System
open System.IO
open System.Text.RegularExpressions
type instruction =
  | Noop
  | Addx of int
let input =
  File.ReadAllLines @"AoC\AoC2022\day10\Input.txt"
  |> Array.map (fun x -> Regex.Match(x,"(?<instruction>noop|addx)\s*(?<num>-?\d*)?"))
  |> Array.map (fun x -> if x.Groups["instruction"].Value = "noop" then Noop else Addx (int <| x.Groups["num"].Value))
let rec cycle currValue instructions = seq {
  match instructions with
  | h::t ->
    match h with 
    | Noop ->
      yield currValue
      yield! cycle (currValue) t
    | Addx x ->
      yield currValue
      yield currValue + x
      yield! cycle (currValue + x) t
  | _ -> ignore
}
List.ofArray input
|> cycle 1
|> Seq.append (Seq.singleton 0)
|> Seq.iteri
     (fun idx x ->
        if (idx) % 40 = 0 then
          Console.WriteLine()
        if abs (x - ((idx) % 40)) < 2 then
          Console.Write "█"
        else
          File.AppendAllText(@"AoC\AoC2022\day10\Output.txt", $"{x} @ {idx}\n")
          Console.Write " " )
