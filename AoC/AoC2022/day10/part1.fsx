open System.IO
open System.Text.RegularExpressions

type instruction =
  | Noop
  | Addx of int
let testInput =
  """addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop""".Split("\n")
  |> Array.map (fun x -> Regex.Match(x,"(?<instruction>noop|addx)\s*(?<num>-?\d*)?"))
  |> Array.map (fun x -> if x.Groups["instruction"].Value = "noop" then Noop else Addx (int <| x.Groups["num"].Value))
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
|> Seq.mapi (fun idx x -> x,idx + 2)

|> Seq.where (fun (_,idx) -> (idx + 20) % 40 = 0)

|> Array.ofSeq
|> Seq.map (fun (f,s) -> f * s)

|> Array.ofSeq
|> Seq.reduce (+)


