open System
open System.IO

let stacks (): char list array =
    [|list.Empty;list.Empty;list.Empty;list.Empty;list.Empty;list.Empty;list.Empty;list.Empty;list.Empty|]


let updateStack (stack:char list array) (letter, location) =
    if letter <> ' ' then 
        stack[location] <- letter :: stack[location]

    
let parseStacks (stacksString:string) =
    let makeCharArrayWithEmbeddedIndices (str:string) =
        let extractLetter index character =
            if ((index - 1) % 4) = 0 then
                Some (character,(index - 1) / 4)
            else None
        Array.mapi extractLetter (str.ToCharArray())
        |> Array.choose id
        
    let stack = stacks()
    stacksString.Split("\n")
    |> Array.map makeCharArrayWithEmbeddedIndices
    |> Array.rev
    |> Array.tail
    |> Array.iter (fun  (x:(char * int)[] ) -> Array.iter (updateStack stack) x )
    stack
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
    
    