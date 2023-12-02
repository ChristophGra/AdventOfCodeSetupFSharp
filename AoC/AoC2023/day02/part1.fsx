open System
open System.IO
open System.Text.RegularExpressions
let tinput =
    """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".ReplaceLineEndings("\n").Split("\n")
let input =
  File.ReadAllLines @"AoC\AoC2023\day02\Input.txt"
  
  
type neededBagContents =
    {
        Red: int
        Blue: int
        Green: int
    }
type Game =
    {
        id: int
        Contents: neededBagContents
    }
    
let parseGame (line:string) =
    let getNeededContents (moves: string array): neededBagContents =
        let start = {Red=0; Blue = 0; Green = 0}
        moves |>
        Array.fold (fun state move ->
            let matches = Regex.Match(move, "(\d+)\s*(\S+)").Groups
            match matches[2].Value with
            | "red" -> {state with Red = state.Red + (matches[1].Value |> int)} 
            | "blue" -> {state with Blue = state.Blue + (matches[1].Value |> int)} 
            | "green" -> {state with Green =state.Green + (matches[1].Value |> int)}
            | x -> state)
            start
    let getMaxNeeded =
        Array.reduce (fun f s ->
            {
                Red = if f.Red < s.Red then s.Red else f.Red
                Green = if f.Green < s.Green then s.Green else f.Green
                Blue = if f.Blue < s.Blue then s.Blue else f.Blue
            })
    let data = line.Split(":")
    let game = Regex.Match(data[0], "(\d+)").Value |> int
    let rounds = data[1].Split(";") |> Array.map (fun x -> x.Split(",") |> Array.map (fun y -> y.Trim()))
    let bags = Array.map getNeededContents rounds
    {id = game; Contents = getMaxNeeded bags}
    
    
Array.map parseGame input
|> Array.where (fun x -> x.Contents.Blue <= 14 && x.Contents.Red <= 12 && x.Contents.Green <= 13)
|> Array.map (fun x -> x.id)
|> Array.sum