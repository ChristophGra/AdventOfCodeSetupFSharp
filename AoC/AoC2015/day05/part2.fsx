open System.IO
let input =
    (File.ReadAllText "AoC\AoC2015\day05\Input.txt").Trim().Replace("\r\n","\n").Split("\n")
       
let hasTwoPairs (str:string) =
    str.ToCharArray()
    |> Array.windowed 2
    |> Array.mapi (fun idx x -> x,idx)
    |> Array.groupBy fst
    |> Array.map (fun (_,v) -> Array.map snd v |> Array.pairwise)
    |> Array.where (fun x -> Array.length x > 0)
    |> Array.where (fun x -> Array.where (fun (a,b) -> b - a <> 1) x  |> Array.length > 0|| (Array.length x > 1))
    |> Array.collect id
    |> Array.length
    |> (fun x -> x > 0)
    
let hasLetterInBetween (str:string) =
    str.ToCharArray()
    |> Array.windowed 3
    |> Array.where (fun x -> x[0] = x[2] )
    |> Array.collect id
    |> Array.length
    |> fun x -> x > 0
    
let isNice str =
    hasTwoPairs str && hasLetterInBetween str
    
let nice, naughty = 
    Array.partition isNice input
    
Array.length nice

