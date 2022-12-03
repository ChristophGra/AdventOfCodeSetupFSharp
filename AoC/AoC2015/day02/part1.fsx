open System.IO
let calculateSize (sides: int []) =
    let s = sides.[0]
    let m = sides.[1]
    let l = sides.[2]
    s * m * 3 + s * l * 2 + m * l * 2
let input =
    File.ReadAllText "C:\Users\Chris\Documents\AoC\AoC\AoC2015\day02\Input.txt"
    |> fun x -> x.Replace("\r\n", "\n").Split "\n"
    |> Array.map (fun (x:string) -> x.Split("x") |> Array.choose (fun y -> match System.Int32.TryParse y with true, i -> Some i | false, _ -> None) |> Array.sort)
    |> Array.where (fun x -> Array.length x = 3)
    |> Array.map calculateSize
    |> Array.sum