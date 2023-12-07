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
  | FiveOfAKind
  | FourOfAKind
  | FullHouse
  | ThreeOfAKind
  | TwoPair
  | OnePair
  | HighCard
  
type Bet = {
  Hand: hand
  OriginalHand: string
  Amount: int
}
  
let compareCardValues a b =
  if a = 'J' && b = 'J' then
    0
  else if a = 'J' && b <> 'J' then
    -1
  else if a <> 'J' && b = 'J' then
    1
  else  if Char.IsDigit a && Char.IsDigit b then
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
      FiveOfAKind
  | 2 ->
      match (Seq.head >> snd) cards with
      | 3 ->
        match (Seq.head >> fst) cards, (Seq.tail >> Seq.head >> fst) cards with
        | 'J',_ -> FiveOfAKind 
        | _, 'J' -> FiveOfAKind
        | _ -> FullHouse 
      | 4 ->
        match (Seq.head >> fst) cards, (Seq.tail >> Seq.head >> fst) cards with
        | 'J',_ -> FiveOfAKind 
        | _, 'J' -> FiveOfAKind
        | _ -> FourOfAKind 
      | _ -> failwith $"{cards}"
  | 3 ->
    match (Seq.head >> snd) cards with
    | 3 ->
      match (Seq.head >> fst) cards, (Seq.tail >> Seq.head >> fst) cards, (Seq.tail >> Seq.tail >> Seq.head >> fst) cards with
      | 'J',_,_ -> FourOfAKind
      | _,'J',_ -> FourOfAKind
      | _,_,'J' -> FourOfAKind
      | _ -> ThreeOfAKind 
    | 2 ->
      match (Seq.head >> fst) cards, (Seq.tail >> Seq.head >> fst) cards, (Seq.tail >> Seq.tail >> Seq.head >> fst) cards with
      | 'J',_,_ -> FourOfAKind
      | _,'J',_ -> FourOfAKind
      | _,_,'J' -> FullHouse
      | _ -> TwoPair 
    | _ -> failwith $"{cards}"
  | 4 ->
    match (Seq.head >> fst) cards, (Seq.tail >> Seq.head >> fst) cards, (Seq.tail >> Seq.tail >> Seq.head >> fst) cards , (Seq.tail >> Seq.tail >> Seq.tail >> Seq.head >> fst) cards with
    | 'J',_,_,_ -> ThreeOfAKind
    | _,'J',_,_ -> ThreeOfAKind
    | _,_,'J',_ -> ThreeOfAKind
    | _,_,_,'J' -> ThreeOfAKind
    | _ -> OnePair 
  | 5 ->
    match (Seq.head >> fst) cards, (Seq.tail >> Seq.head >> fst) cards, (Seq.tail >> Seq.tail >> Seq.head >> fst) cards , (Seq.tail >> Seq.tail >> Seq.tail >> Seq.head >> fst) cards, (Seq.tail >> Seq.tail >> Seq.tail >> Seq.tail >> Seq.head >> fst) cards with
    | 'J',_,_,_,_ -> OnePair
    | _,'J',_,_,_ -> OnePair
    | _,_,'J',_,_ -> OnePair
    | _,_,_,'J',_ -> OnePair
    | _,_,_,_,'J' -> OnePair
    | _ -> HighCard
  | _ -> failwith $"{cards}"
    
    
    
let parser =
  Array.map (fun (x:string) -> x.Split(" "))
  >> Array.map (fun x -> x[0],(x[1] |> int))
  
let stripBet f a b =
  f (fst a) (fst b)
  
  
  
  
let compareHands (hand1: Bet) (hand2: Bet) =
  match hand1.Hand = hand2.Hand, hand1.Hand, hand2.Hand with
  | true, _,_ -> Seq.compareWith compareCardValues hand1.OriginalHand hand2.OriginalHand
  | _,FiveOfAKind, _ -> 1
  | _, _, FiveOfAKind -> -1
  | _,FourOfAKind, _ -> 1
  | _,_, FourOfAKind -> -1
  | _,FullHouse, _ -> 1
  | _,_, FullHouse -> -1
  | _,ThreeOfAKind, _ -> 1
  | _, _,ThreeOfAKind -> -1
  | _,TwoPair, _ -> 1
  | _, _,TwoPair -> -1
  | _,OnePair, _ -> 1
  | _, _, OnePair -> -1
  | _ -> failwith $"{hand1} {hand2}"
  
let data =
  parser input
  |> Array.map (fun x -> {Hand= fst x |> constructHand; Amount = snd x; OriginalHand = fst x })
  |> Array.sortWith compareHands
  //|> Array.iteri (fun idx x -> Console.WriteLine $"{idx}: {x}")
  |> Array.mapi (fun idx x -> (idx + 1) * x.Amount)
  |> Array.sum