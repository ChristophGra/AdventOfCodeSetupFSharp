open System
open System.IO
let input =
  File.ReadAllLines @"AoC\AoC2022\day08\Input.txt"
  |> Array.mapi (fun idx1 x -> x.ToCharArray() |>  Array.mapi (fun idx2 y -> Int32.Parse($"{y}"),(idx1,idx2)))
  
  
let sides =
   [|
(*north*) input
(*east*)  input |> Array.rev  |> Array.transpose 
(*south*) input               |> Array.transpose  
(*west*)  input               |> Array.transpose |> Array.rev |> Array.transpose 
    |]
   |> Array.map (Array.map List.ofArray)
    
let max = (Array.head sides).Length
let field =
    Seq.unfold (fun idx -> if idx < max then Some (Array.zeroCreate<bool> max,idx + 1) else None) 0
    |> Array.ofSeq
let printField (f:bool[][]) =
    for a in 0..f.Length - 1 do
        for b in 0..(Array.head f).Length - 1 do
            Console.Write (if f[a][b] then "1" else "0")
        Console.WriteLine ()
let rec setVisible (field: bool[][]) prevHeight (line:(int * (int * int)) list)  =
    
    match line with
    | h::t -> 
        let (height,(x,y)) = h
        if prevHeight < height then
            field[x][y] <- true
            setVisible field height t
        else
            setVisible field prevHeight t
    | [] -> ()
    
    
let initialSetVisible =
    setVisible field -1
    
Array.iter (Array.iter initialSetVisible) sides

Array.collect id field |> Array.sumBy (fun x -> if x then 1 else 0)
