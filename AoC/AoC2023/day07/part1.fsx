open System
open System.IO


let tinput =
  """32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483""".Split("\n")
  
let input =
  File.ReadAllLines @"AoC\AoC2023\day07\Input.txt"
  
  
type hand =
  | FiveOfAKind of char
  | FourOfAKind of char * char
  | FullHouse of char * char
  | ThreeOfAKind of char * char * char
  | TwoPair of char * char * char
  | OnePair of char * char * char * char
  | HighCard of char * char * char * char * char
  
type Bet = {
  Hand: hand
  Amount: int
}
  
let compareCardValues a b =
  if Char.IsDigit a && Char.IsDigit b then
    (int a) - (int b) 
  else if Char.IsDigit a && not (Char.IsDigit b) then
    -1
   else if not (Char.IsDigit a) && Char.IsDigit b then
    1
  else
    match a,b with
    | 'A','A' -> 0
    | 'A', _ -> 1
    | _ , 'A' -> -1
    | 'K','K' -> 0
    | 'K', _ -> 1
    | _,'K' -> -1
    | 'Q','Q' -> 0
    | 'Q', _ -> 1
    |  _,'Q' -> -1
    | 'J','J' -> 0
    | 'J', _ -> 1
    | _,'J' -> -1
    | 'T','T' -> 0
    | _ -> failwith $"{a} {b}"
    
    
let inv f a b =
  f a b |> (*) -1
let constructHand (hand:string) =
  let cards =
    Seq.sortWith (inv compareCardValues) hand
    |> Seq.groupBy id 
    |> Seq.map snd
    |> Seq.sortByDescending Seq.length
    |> Seq.map (fun x -> Seq.head x, Seq.length x)
  match Seq.length cards with
  | 1 ->
      FiveOfAKind ((Seq.head >> fst) cards )
  | 2 ->
      match (Seq.head >> snd) cards with
      | 3 ->
        FullHouse ((Seq.head >> fst) cards,  (Seq.tail >> Seq.head >> fst) cards)
      | 4 ->
        FourOfAKind (( Seq.head >> fst) cards, (Seq.tail >> Seq.head >> fst) cards)
      | _ -> failwith $"{cards}"
  | 3 ->
    match (Seq.head >> snd) cards with
    | 3 ->
      ThreeOfAKind (( Seq.head >> fst) cards, (Seq.tail >> Seq.head >> fst) cards, (Seq.tail >> Seq.tail >> Seq.head >> fst) cards)
    | 2 ->
      TwoPair (( Seq.head >> fst) cards, (Seq.tail >> Seq.head >> fst) cards, (Seq.tail >> Seq.tail >> Seq.head >> fst) cards)
    | _ -> failwith $"{cards}"
  | 4 ->
    OnePair (( Seq.head >> fst) cards, (Seq.tail >> Seq.head >> fst) cards, (Seq.tail >> Seq.tail >> Seq.head >> fst) cards, (Seq.tail >> Seq.tail >> Seq.tail >> Seq.head >> fst) cards)
  | 5 ->
    HighCard (( Seq.head >> fst) cards, (Seq.tail >> Seq.head >> fst) cards, (Seq.tail >> Seq.tail >> Seq.head >> fst) cards, (Seq.tail >> Seq.tail >> Seq.tail >> Seq.head >> fst) cards, (Seq.tail >> Seq.tail >> Seq.tail >> Seq.tail >> Seq.head >> fst) cards)
  | _ -> failwith $"{cards}"
    
    
    
let parser =
  Array.map (fun (x:string) -> x.Split(" "))
  >> Array.map (fun x -> x[0],(x[1] |> int))
  
let stripBet f a b =
  f (fst a) (fst b)
  
  
  
  
let compareHands (hand1: Bet) (hand2: Bet) =
  match hand1.Hand, hand2.Hand with
  | FiveOfAKind a, FiveOfAKind b -> compareCardValues a b
  | FiveOfAKind _, _ -> 1
  |  _, FiveOfAKind _ -> -1
  | FourOfAKind (a1, a2), FourOfAKind (b1,b2) ->
    let res1 = compareCardValues a1 b1
    if res1 <> 0 then
      res1
    else
      compareCardValues a2 b2
  | FourOfAKind _, _ -> 1
  | _, FourOfAKind _ -> -1
  | FullHouse (a1, a2), FullHouse (b1,b2) ->
    let res1 = compareCardValues a1 b1
    if res1 <> 0 then
      res1
    else
      compareCardValues a2 b2
  | FullHouse _, _ -> 1
  | _, FullHouse _ -> -1
  | ThreeOfAKind(a1, a2, a3), ThreeOfAKind(b1, b2, b3) ->
    let res1 = compareCardValues a1 b1
    if res1 <> 0 then
      res1
    else 
      let res2 = compareCardValues a2 b2
      if res2 <> 0 then
        res2
      else
        compareCardValues a3 b3
  | ThreeOfAKind _, _ -> 1
  |  _,ThreeOfAKind _ -> -1
  | TwoPair (a1, a2, a3), TwoPair(b1, b2, b3) ->
    let res1 = compareCardValues a1 b1
    if res1 <> 0 then
      res1
    else 
      let res2 = compareCardValues a2 b2
      if res2 <> 0 then
        res2
      else
        compareCardValues a3 b3
  | TwoPair _, _ -> 1
  |  _,TwoPair _ -> -1
  | OnePair(a1, a2, a3, a4), OnePair(b1, b2, b3, b4) ->
    let res1 = compareCardValues a1 b1
    if res1 <> 0 then
      res1
    else 
      let res2 = compareCardValues a2 b2
      if res2 <> 0 then
        res2
      else
        let res3 = compareCardValues a3 b3
        if res3 <> 0 then
          res3
        else
          compareCardValues a4 b4
  | OnePair _, _ -> 1
  |  _, OnePair _ -> -1
  | HighCard(a1, a2, a3, a4, a5),HighCard(b1, b2, b3, b4, b5) ->
    let res1 = compareCardValues a1 b1
    if res1 <> 0 then
      res1
    else 
      let res2 = compareCardValues a2 b2
      if res2 <> 0 then
        res2
      else
        let res3 = compareCardValues a3 b3
        if res3 <> 0 then
          res3
        else
          let res4 = compareCardValues a4 b4
          if res4 <> 0 then
            res4
          else
            compareCardValues a5 b5
  
let data =
  parser tinput
  |> Array.map (fun x -> {Hand= fst x |> constructHand; Amount = snd x})
  |> Array.sortWith compareHands
  |> Array.map (fun x -> x.Hand)
  |> Array.iter Console.WriteLine
  //|> Array.mapi (fun idx x -> (idx + 1) * x.Amount)
  //|> Array.sum