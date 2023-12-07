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
  AmountBet: int
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
    | 'J', 'J' -> 0
    | 'J',_ -> 1
    | _, 'J' -> -1
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
  | 1 -> FiveOfAKind 
  | 2 ->
      match (Seq.head >> snd) cards with
      | 3 -> FullHouse 
      | 4 -> FourOfAKind 
      | _ -> failwith $"{cards}"
  | 3 ->
    match (Seq.head >> snd) cards with
    | 3 -> ThreeOfAKind 
    | 2 -> TwoPair 
    | _ -> failwith $"{cards}"
  | 4 -> OnePair 
  | 5 -> HighCard
  | _ -> failwith $"{cards}"
    
    
    
let parser =
  Array.map (fun (x:string) -> x.Split(" "))
  >> Array.map (fun x -> x[0],(x[1] |> int))
  
  
let compareHands (hand1: Bet) (hand2: Bet) =
  match hand1.Hand = hand2.Hand, hand1.Hand, hand2.Hand with
  | true, _, _ -> Seq.compareWith compareCardValues hand1.OriginalHand hand2.OriginalHand
  | _, FiveOfAKind _, _ -> 1
  | _,  _, FiveOfAKind _ -> -1
  | _, FourOfAKind _, _ -> 1
  | _, _, FourOfAKind _ -> -1
  | _, FullHouse _, _ -> 1
  | _, _, FullHouse _ -> -1
  | _, ThreeOfAKind _, _ -> 1
  | _,  _,ThreeOfAKind _ -> -1
  | _, TwoPair _, _ -> 1
  | _,  _,TwoPair _ -> -1
  | _, OnePair _, _ -> 1
  | _,  _, OnePair _ -> -1
  | _ -> 0
  
let data =
  parser input
  |> Array.map (fun x -> {Hand= fst x |> constructHand; AmountBet = snd x; OriginalHand = fst x })
  |> Array.sortWith compareHands
  //|> Array.iteri (fun idx x -> Console.WriteLine $"{idx}: {x}")
  |> Array.mapi (fun idx x -> (idx + 1) * x.AmountBet)
  |> Array.sum
  