open System.IO

let input =
    (File.ReadAllText "AoC\AoC2015\day01\Input.txt").ToCharArray()

let floor =
    Array.scan (fun (floor,count) x -> match x with '(' -> floor + 1, count + 1 | ')' -> floor - 1, count + 1) (0,0) input
    |> Array.find (fst >> (>) 0)
    