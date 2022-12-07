open System.IO



let input =
    (File.ReadAllText "AoC\AoC2015\day05\Input.txt").Trim().Replace("\r\n","\n").Split("\n")
    
    
    
let containsThreeVowels (str:string) =
    str.ToCharArray()
    |> Array.where "aeiou".Contains
    |> Array.length
    |> (<=) 3
let hasRepeating (str:string) =
    str.ToCharArray()
    |> Array.windowed 2
    |> Array.exists (fun x -> x[0] = x[1])
    
let doesntContain (str:string) =
    [|"ab";"cd";"pq";"xy"|]
    |> Array.forall (fun x -> not <| str.Contains(x))
let isNice str =
    containsThreeVowels str && hasRepeating str && doesntContain str
    
let nice, naughty = 
    Array.partition isNice input
    
Array.length nice