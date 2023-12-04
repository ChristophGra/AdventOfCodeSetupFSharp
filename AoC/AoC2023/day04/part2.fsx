open System
open System.IO



  
let input =
  File.ReadAllLines @"AoC\AoC2023\day04\Input.txt"
  |> Array.map (fun x -> x.Split(":"))
  |> Array.map (fun x -> Array.last (x[0].Split(" ")), x[1].Split("|"))
  |> Array.map (fun x -> fst x |> int, (snd x)[0], (snd x)[1])
   

let rec getCards ((totalcards: int), (remainingCards: int list)) (currentCard: Set<int>) =
  let currentCardCount = List.head remainingCards
  let remainingCardCounts = List.tail remainingCards
  let newTotal = totalcards + currentCardCount
  let additionalCards = List.init (Set.count currentCard) (fun _ -> currentCardCount)
  let currentwinninglen = List.length remainingCardCounts
  let additionalLen = List.length additionalCards
  let newAdditional = if currentwinninglen <= additionalLen then additionalCards else List.append additionalCards (List.init (currentwinninglen - additionalLen) (fun _ -> 0))
  let finalList =
    List.zip remainingCardCounts newAdditional
    |> List.map (fun (a,b) -> a + b)
  newTotal, finalList 
   
   
let extractNums (str:string) =
  str.Split(" ")
  |> Array.where (fun x -> String.length (x.Trim()) > 0)
  |> Array.map int

input
|> Array.map (fun curr -> 
  let card, winning, having = curr
  (extractNums winning, extractNums having))
|> Array.map (fun (win,have) -> Set.intersect (Set.ofSeq win) (Set.ofSeq have))
|> Array.fold getCards (0,List.init (Array.length input) (fun _ -> 1))
