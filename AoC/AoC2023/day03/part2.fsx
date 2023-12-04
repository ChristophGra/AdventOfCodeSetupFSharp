open System
open System.IO

let input =
  File.ReadAllLines @"AoC\AoC2023\day03\Input.txt"
  |> Array.map (Array.ofSeq)
  
let extractPotentialGearLocations (data:char array array) =
    seq {
        for i in 0..data.Length - 1 do
            for j in 0..data[0].Length - 1 do
                if data[i][j] = '*' then
                    yield i,j
    }
let extractNum (data: char array array) (location: (int * int)) =
    let mutable num = 0
    let row = fst location
    let mutable col = snd location
    while ( col > 0 && Char.IsDigit (data[row][col - 1])) do
        col <- col - 1
    while ( col < data[0].Length && Char.IsDigit (data[row][col])) do
        num <- num * 10 + int (data[row][col]) - 48 
        col <- col + 1
    num
let getSurroundingNums (data: char array array) (location: int * int) =
    let mutable isDigit = false
    let leftBound = if snd location - 1 < 0 then 0 else snd location - 1
    let rightBound = if snd location + 1 > data[0].Length - 1 then data[0].Length - 1 else snd location + 1
    let topBound = if fst location - 1 < 0 then 0 else fst location - 1
    let bottomBound = if fst location + 1 > data.Length - 1 then data.Length - 1 else fst location + 1
    
    seq {
        for i in topBound..bottomBound do
            for j in leftBound .. rightBound do
                if not isDigit && Char.IsDigit(data[i][j]) then
                    yield extractNum data (i,j)
                    isDigit <- true
                if not (Char.IsDigit(data[i][j])) then
                    isDigit <- false
            isDigit <- false
    }
        
    
extractPotentialGearLocations input
|> Seq.map (getSurroundingNums input)
|> Seq.where (Seq.length >> (=) 2)
|> Seq.sumBy (Seq.reduce (*))
