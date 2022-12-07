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
    let extractInstructions (str:string) =
        let move,from = str.Split(" from ") |> fun from -> from[0],from[1]
        let slotFrom,slotTo = from.Split(" to ") |> fun slots -> int slots[0], int slots[1]
        let amountToMove = int (move.Replace("move ",""))
        amountToMove,slotFrom - 1, slotTo - 1
    moves.Split("\n")
    |> Array.map extractInstructions
    

let rec doOneMove (stack: char list array) (count, from, ``to``) =
    stack[``to``] <- List.append  (List.take (Math.Min(count, List.length stack[from])) stack[from]) stack[``to``]
    stack[from ] <-  List.skip (Math.Min(count, List.length stack[from])) stack[from] 
    
    
let initialStacks,moves =
  (File.ReadAllText @"AoC\AoC2022\day05\Input.txt").Replace("\r\n","\n").Split("\n\n")
  |> fun x -> parseStacks x[0], parseMoves x[1]
  
let charToS (c:char) =
    $"{c}"

    
let finaStack =
    Array.iter (fun x -> doOneMove initialStacks x) moves
    initialStacks
    |> Array.map List.tryHead
    |> Array.choose id
    |> String
    
    