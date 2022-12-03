open System.IO

let input =
    (File.ReadAllText "AoCAoC2015day01Input.txt").ToCharArray()

let floor =
    Array.fold (fun curr x -> match x with '(' -> curr + 1 | ')' -> curr - 1) 0 input
    