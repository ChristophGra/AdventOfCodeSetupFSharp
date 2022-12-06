open System
open System.IO


let testInput ="""    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"""
let stacks (): char list array =
    [|list.Empty;list.Empty;list.Empty;list.Empty;list.Empty;list.Empty;list.Empty;list.Empty;list.Empty|]


let updateStack (stack:char list array) (letter, location) =
    if letter <> ' ' then 
        stack[location] <- letter :: stack[location]
    stack

    
let parseStacks (stacksString:string) =
    stacksString.Split("\n")
    |> Array.map (fun y -> Array.mapi (fun i x -> if ((i - 1) % 4) = 0 then Some (x,(i - 1) / 4)  else None) (y.ToCharArray()) |> Array.choose id)
    |> Array.rev
    |> Array.tail
    |> Array.fold (fun (state: char list array) (x:(char * int)[] ) -> Array.fold updateStack state x) (stacks())
let parseMoves (moves:string) =
    moves.Split("\n")
    |> Array.map (fun x -> x.Split(" from ") |> fun y ->  (y[1].Split(" to ") |> fun z -> int (y[0].Replace("move ","")),int z[0] - 1, int z[1] - 1))
    
 
let rec doOneMove (stack: char list array) (count, from, ``to``) =
    stack[``to``] <- List.append (List.take (Math.Min (count, (List.length stack[from]))) stack[from] |> List.rev) stack[``to``]
    stack[from ] <- List.skip (Math.Min (count, (List.length stack[from]))) stack[from]
    
let initialStacks,moves =
  (File.ReadAllText @"AoC\AoC2022\day05\Input.txt").Replace("\r\n","\n").Split("\n\n")
  |> fun x -> parseStacks x[0], parseMoves x[1]
  
    
let finaStack =
    Array.iter (fun x -> doOneMove initialStacks x)  moves
    initialStacks
    |> Array.map List.tryHead
    |> Array.choose id
    |> String
    
    