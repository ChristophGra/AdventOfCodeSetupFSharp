open System.ComponentModel
open System.IO

type Hand = |Rock|Paper|Scissors
type strategy = {
    OpponentHand: Hand
    OwnHand: Hand
}
let scoreHand (matchup: strategy) =
    let RoundScore oppo own =
        match oppo, own with
        | Rock, Paper -> 6
        | Paper, Scissors -> 6
        | Scissors, Rock -> 6
        | Paper, Rock -> 0
        | Scissors, Paper -> 0
        | Rock, Scissors -> 0
        | _, _ -> 3
    let ScoreOwn own =
        match own with
        | Rock -> 1 
        | Paper -> 2
        | Scissors -> 3
    (RoundScore matchup.OpponentHand matchup.OwnHand) + (ScoreOwn matchup.OwnHand)

let parseHand (opponent, own) =
    {OpponentHand = (match opponent with | 'A' -> Rock | 'B' -> Paper | 'C' -> Scissors | _ -> Rock)
     OwnHand = (match own with | 'X' -> Rock | 'Y' -> Paper | 'Z' -> Scissors | _ -> Rock)
     }
let input =
  (File.ReadAllText @"AoC\AoC2022\day02\Input.txt").Replace("\r\n", "\n").Split("\n")
  |> Array.choose (fun x -> x.Split(" ") |> fun y -> (if (y.Length > 1) then Some (y.[0].Chars 0,y.[1].Chars 0) else None))
  |> Array.map (parseHand >> scoreHand)
  |> Array.sum