open System.IO



let input = File.ReadAllLines "AoC\AoC2015\day08\Input.txt"

let rec parseLine literalCount memoryCount (str: char list) =
    match str with
    | '\\'::t -> parseLine (literalCount + 2) (memoryCount + 1) t
    | '"'::t -> parseLine (literalCount + 2) (memoryCount + 1) t 
    | h::t -> parseLine (literalCount + 1) (memoryCount + 1) t
    | [] -> literalCount,memoryCount
    
let getCount: string -> int*int =
    List.ofSeq >> parseLine 0 0
    
Array.map getCount input
|> Array.map (fun x -> (fst x + 2) - snd x)
|> Array.sum