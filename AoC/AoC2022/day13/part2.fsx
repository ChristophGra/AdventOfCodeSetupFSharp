open System
open System.IO



let testInput =
    """[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"""

type arr =
    | Value of int
    | Arr of arr List
    | Empty
    
let input =
  (File.ReadAllText @"AoC\AoC2022\day13\Input.txt")
  
let parsedInput  =
    let rec parseInput inp: arr list *( char list)=
        match inp with
        | '['::t ->
            let valu, remainder = parseInput t
            let valu2, r2 = parseInput remainder
            List.append [Arr valu] valu2,r2 
        | d::_ when Char.IsDigit d ->
            let num = (List.takeWhile Char.IsDigit inp) |> List.toArray |> String
            let rem = (List.skipWhile Char.IsDigit inp)
            let valu, remainder = parseInput rem
            List.append [ (Value (int num))] valu, remainder
        | ']'::t ->
            [], t
        | ','::t ->
            let valu, remainder = parseInput t
            valu, remainder
        | [] ->
            [],[]
        | _ -> raise (new ArgumentException (inp |> List.toArray |> String))
    
    let parsePair (x:string) =
        x.Split("\n")
        |> Array.map (fun y -> y.ToCharArray() |> List.ofArray)
        |> Array.where (fun x -> List.length x > 0)
        |> Array.map parseInput
        
    parsePair input
    |>Array.map fst
    |> Array.map (List.head)
    
type OrderType = |Ok|Wrong|Equal
let rec compareElems (arr1:arr)(arr2:arr) =
    //Console.WriteLine (arr1.ToString() + "::" + arr2.ToString())
    let rec CompareArray arrs1 arrs2 =
        //Console.WriteLine (arrs1.ToString() + "::" + arrs2.ToString())
        match arrs1, arrs2 with
        | [],[] -> Equal
        | h1::t1,h2::t2 ->
            match compareElems h1 h2 with
            | Equal -> CompareArray t1 t2
            | Ok -> Ok
            | Wrong -> Wrong
        | [],h::t -> Ok
        | h::t,[] -> Wrong
    match arr1, arr2 with
    | Value a, Value b -> if a < b then Ok else if a = b then Equal else Wrong
    | Arr a, Arr b -> CompareArray a b
    | Arr a, Value b -> CompareArray a [Value b]
    | Value a, Arr b -> CompareArray [Value a] b
    | a,b -> raise (new ArgumentException ())
    
let divider1 =Arr [Arr [Value 2]]
let divider2 = Arr [Arr [Value 6]]
let sorted =
    parsedInput
    |> Array.append [|divider1;divider2|]
    |> Array.sortWith (fun a b -> compareElems a b|> fun x -> match x with | Ok -> -1 | _ ->  1)

let index1 =
    Array.indexed sorted
    |> Array.find (fun x -> match compareElems (snd x) divider1 with Equal -> true | _ -> false)
    |> fst
    |> (+) 1
let index2 =
    Array.indexed sorted
    |> Array.find (fun x -> match compareElems (snd x) divider2 with Equal -> true | _ -> false)
    |> fst
    |> (+) 1
    
index1 * index2