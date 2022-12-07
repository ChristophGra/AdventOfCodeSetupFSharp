open System.IO

let input =
    (File.ReadAllText "AoC\AoC2015\day01\Input.txt").ToCharArray()

let floor =
    Array.fold (fun curr x -> match x with '(' -> curr + 1 | ')' -> curr - 1) 0 input
    