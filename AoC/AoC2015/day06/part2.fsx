open System
open System.IO
open System.Text.RegularExpressions

type Command = |On|Off|Toggle

let input = File.ReadAllLines "AoC\AoC2015\day06\Input.txt"
let testInput = ("""turn on 0,0 through 999,999
toggle 0,0 through 999,0
turn off 499,499 through 500,500""".Split("\n"))
let getCommand (str:string) =
    let matches = Regex("(toggle|turn on|turn off)\s(\d+),(\d+) through (\d+),(\d+)").Match(str)
    let command =
        match matches.Groups[1].Value with
        | "turn on" -> Command.On
        | "turn off" -> Command.Off
        | "toggle" -> Command.Toggle
        | _ -> raise (new InvalidDataException())
    command,(int matches.Groups[2].Value,int matches.Groups[3].Value),(int matches.Groups[4].Value,int matches.Groups[5].Value)
    
let parsedInput =
    Array.map getCommand input
    
let executeCommand (field: int [,])  (comm:Command,(startx:int,starty:int),(endx:int,endy:int)): unit =
    for x in startx..endx do
        for y in starty..endy do
            match comm with
            | On -> field[x,y] <-  field[x,y] + 1
            | Off -> field[x,y] <- if field[x,y] = 0 then 0 else field[x,y] - 1
            | Toggle ->
                field[x,y] <- field[x,y] + 2


let executeAllCommands commands =
    let initer = seq {
        for x in 1..1000 do
            seq {
            for y in 1..1000 do
                yield 0
            }
        }
    let field = array2D initer
    Array.iter (executeCommand field) commands
    field
    
let exec =
    executeAllCommands parsedInput
    |> fun arr -> Array.fold (fun cnt2 x -> Array.fold (fun cnt y -> cnt + arr.[x,y]) cnt2 [|0..999|]) 0 [|0..999|] 