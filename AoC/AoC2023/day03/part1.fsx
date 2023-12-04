open System
open System.IO

let tInput =
    """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..""".ReplaceLineEndings("\n").Split("\n") |>Array.map Array.ofSeq
let input =
  File.ReadAllLines @"AoC\AoC2023\day03\Input.txt"
  |> Array.map (Array.ofSeq)
  
  
  
  
let extractRanges (data:char array array) =
    seq {
        let mutable parsingNum = false
        let mutable currStart = (0,0)
        for i in 0..data.Length - 1 do
            for j in 0..data[0].Length - 1 do
                if not parsingNum && Char.IsNumber(data[i][j]) then
                    parsingNum <- true
                    currStart <- (i,j)
                if parsingNum && (not <| Char.IsNumber(data[i][j])) then
                    parsingNum <- false
                    yield currStart, (i,j - 1)
            if parsingNum then
                parsingNum <- false
                yield currStart, (i, data[0].Length - 1)
    }
    
    
let getSurroundingSymbols (data: char array array) (range: (int * int)*(int * int)) =
    let start,``end`` = range
    let leftBound = if snd start - 1 < 0 then 0 else snd start - 1
    let rightBound = if snd ``end`` + 1 > data[0].Length - 1 then data[0].Length - 1 else snd ``end`` + 1
    let topBound = if fst start - 1 < 0 then 0 else fst start - 1
    let bottomBound = if fst ``end`` + 1 > data.Length - 1 then data.Length - 1 else fst ``end`` + 1
    seq {
        for i in topBound .. bottomBound do
            for j in leftBound .. rightBound  do
                if (not (Char.IsNumber (data[i][j])) && not (data[i][j] = '.')) then
                    yield (data[i][j])
    }
    
let extractNum (data: char array array) (range: (int * int)*(int * int)) =
    let mutable num = 0
    for i in (fst >> fst) range .. (snd >> fst) range do
        for j in (fst >> snd) range .. (snd >> snd) range do
            num <- num * 10 + (int (data[i][j]) - 48)
    num
    
extractRanges input
|> Seq.where ((getSurroundingSymbols input)>> Seq.isEmpty >> not)
|> Seq.map (extractNum input)
|> Seq.sum
//|> Seq.iter Console.WriteLine
