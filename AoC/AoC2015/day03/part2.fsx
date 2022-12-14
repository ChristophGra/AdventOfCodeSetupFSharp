open System.IO

let mapDirection char =
    match char with
    | '>' -> (1,0)
    | '<' -> (-1,0)
    | 'v' -> (0,1)
    | '^' -> (0,-1)
    | _ -> (0,0)
    
let updateDict (dict: Set<int * int>) newx newy =
    match dict.Contains(newx,newy) with
    | true ->
        dict
    | false ->
        dict.Add(newx,newy)
        
let initialState  =
    Set.empty  |> fun x -> updateDict x 0 0 , (0,0), (0,0)
    
let updatePosition (dict,pos1,pos2: int * int) move =
    let newpos = fst pos1 + fst move, snd pos1 + snd move
    updateDict dict (fst newpos) (snd newpos), pos2, newpos
let input =
    File.ReadAllText @"AoC\AoC2015\day03\Input.txt"
    |> fun x -> x.ToCharArray()
    |> Array.map mapDirection
    |> Array.fold updatePosition initialState
    |> fun (x:Set<int * int>,_, _) -> x.Count 