open System
open System.IO
open System.Text.RegularExpressions
let input =
  (File.ReadAllText @"AoC\AoC2022\day11\Input.txt").Split("\n\n")
  
let testInput =
  """Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1""".Split("\n\n")
    
type monkey = {
  Id: int
  items: int[]
  Operation: int -> int
  Test: int
  trueMonkeyId: int
  falseMonkeyId: int
  inspections: int
}
let parseInput (str:string []) =
  let reg = Regex("""Monkey (?<monkeyID>\d+):\s*
\s*Starting items:(?<numbers>(,?\s*\d+)*)\s*
\s*Operation: new = (?<operation>.*)
\s*Test: divisible by (?<test>\d*)
\s*If true: throw to monkey (?<trueMonkey>\d*)
\s*If false: throw to monkey (?<falseMonkey>\d*)""")
  let matches = Array.map reg.Match str
  let parseOperation str: int -> int =
    let ops = (Regex("(?<first>old|\d+) (?<op>\*|\+|) (?<second>old|\d+)").Match(str).Groups)
    Console.WriteLine $"""{ops["first"].Value},{ops["second"].Value}, {ops["op"].Value}"""
    match ops["first"].Value,ops["second"].Value, ops["op"].Value with
    | "old", "old", "+" ->  fun x -> x + x
    | f,"old", "+" -> fun x -> (int f) + x
    | "old", s, "+" -> fun x -> (int s) + x
    | "old", "old", "*" ->  fun x -> x * x
    | f,"old", "*" -> fun x -> (int f) * x
    | "old", s, "*" -> fun x -> (int s) * x
    | _ -> raise (new Exception())
  let extractVals (matc:Match):monkey =
    let vals = matc.Groups
    if matc.Success then
       {
         items = vals["numbers"].Value.Split(",") |> Array.map int
         Id = int vals["monkeyID"].Value
         Operation = parseOperation vals["operation"].Value
         Test = int vals["test"].Value
         falseMonkeyId = int vals["falseMonkey"].Value
         trueMonkeyId = int vals["trueMonkey"].Value
         inspections = 0
       }
    else
      Console.WriteLine matc.Value
      raise (new Exception "parse failed")
  Array.map extractVals matches
  |> Array.sortBy (fun x -> x.Id)


let ms = parseInput input

let doRound (monkeys:monkey[]) =
  let moveItem from ``to`` worryLevel =
    monkeys[``to``] <- {monkeys[``to``] with items = Array.append monkeys[``to``].items (Array.singleton worryLevel)}
    monkeys[from] <- {monkeys[from] with items = Array.tail monkeys[from].items; inspections = monkeys[from].inspections + 1}
  let doMonkey monkey =
    let items = monkey.items
    let getWorryLevel x =
      monkey.Operation x
      |> fun y -> y / 3
    let target =
      fun x -> (if x % monkey.Test = 0 then monkey.trueMonkeyId else monkey.falseMonkeyId) , x
    Array.iter (fun item -> getWorryLevel item |> target |> fun x -> moveItem monkey.Id (fst x) (snd x)) items
  Array.iter doMonkey monkeys
  monkeys
  
  
Array.fold (fun _ _ -> doRound ms) ms [|1..20|]
|> Array.map (fun x -> x.inspections)
|> Array.sortDescending
|> Array.take 2
|> Array.reduce (*)