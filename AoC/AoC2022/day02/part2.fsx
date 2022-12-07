open System.IO

type Hand = |Rock|Paper|Scissors
type Reply = |Win|Draw|Lose
type strategy = {
    OpponentHand: Hand
    Reply: Reply
}
let scoreHand (matchup: strategy) =
    let RoundScore oppo strategy =
        match  strategy with
        | Lose-> 
            0 + 
            match oppo with
            | Rock -> 3
            | Paper -> 1
            | Scissors -> 2 
        | Draw-> 
            3 + 
            match oppo with
            | Rock -> 1
            | Paper -> 2
            | Scissors -> 3 
        | Win -> 
            6 + 
            match oppo with
            | Rock -> 2
            | Paper -> 3
            | Scissors -> 1 
    (RoundScore matchup.OpponentHand matchup.Reply)

let parseHand (opponent, own) =
    {OpponentHand = (match opponent with | 'A' -> Rock | 'B' -> Paper | 'C' -> Scissors | _ -> Rock)
     Reply = (match own with | 'X' -> Lose | 'Y' -> Draw | 'Z' -> Win | _ -> Lose)
     }
let input =
  (File.ReadAllText @"AoC\AoC2022\day02\Input.txt").Replace("\r\n", "\n").Split("\n")
  |> Array.choose (fun x -> x.Split(" ") |> fun y -> (if (y.Length > 1) then Some (y[0].Chars 0,y[1].Chars 0) else None))
  |> Array.map (parseHand >> scoreHand)
  |> Array.sum