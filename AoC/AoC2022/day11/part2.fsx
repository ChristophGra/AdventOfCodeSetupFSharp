open System
open System.IO
open System.Text.RegularExpressions
open Microsoft.FSharp.Core
let input =
  (File.ReadAllText @"AoC\AoC2022\day11\Input.txt").Split("\n\n")
  
  
let testInput =
  """Monkey 0:
  Starting items: 1
  Operation: new = old * old
  Test: divisible by 2
    If true: throw to monkey 1
    If false: throw to monkey 2

Monkey 1:
  Starting items: 2, 3, 5
  Operation: new = old * 2
  Test: divisible by 3
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 99, 69
  Operation: new = old + old
  Test: divisible by 5
    If true: throw to monkey 0
    If false: throw to monkey 1""".Split("\n\n")
    
type monkey = {
  Id: int
  items: uint64[]
  Operation: uint64 -> uint64
  Test: uint64
  trueMonkeyId: int
  falseMonkeyId: int
  inspections: uint64
}
let parseInput (str:string []) =
  let reg = Regex("""Monkey (?<monkeyID>\d+):\s*
\s*Starting items:(?<numbers>(,?\s*\d+)*)\s*
\s*Operation: new = (?<operation>.*)
\s*Test: divisible by (?<test>\d*)
\s*If true: throw to monkey (?<trueMonkey>\d*)
\s*If false: throw to monkey (?<falseMonkey>\d*)""")
  let matches = Array.map reg.Match str
  let parseOperation str: uint64 -> uint64 =
    let ops = (Regex("(?<first>old|\d+) (?<op>\*|\+|) (?<second>old|\d+)").Match(str).Groups)
    Console.WriteLine $"""{ops["first"].Value},{ops["second"].Value}, {ops["op"].Value}"""
    match ops["first"].Value,ops["second"].Value, ops["op"].Value with
    | "old", "old", "+" ->  fun x -> x + x
    | f,"old", "+" -> fun x -> (uint64 f) + x
    | "old", s, "+" -> fun x -> (uint64 s) + x
    | "old", "old", "*" ->  fun x -> x * x
    | f,"old", "*" -> fun x -> (uint64 f) *  x
    | "old", s, "*" -> fun x -> (uint64 s) * x
    | _ -> raise (new Exception())
  let extractVals (matc:Match):monkey =
    let vals = matc.Groups
    if matc.Success then
       {
         items = vals["numbers"].Value.Split(",") |> Array.map uint64
         Id = int vals["monkeyID"].Value
         Operation = parseOperation vals["operation"].Value
         Test = uint64 vals["test"].Value
         falseMonkeyId = int vals["falseMonkey"].Value
         trueMonkeyId = int vals["trueMonkey"].Value
         inspections =  uint64 0
       }
    else
      Console.WriteLine matc.Value
      raise (new Exception "parse failed")
  Array.map extractVals matches
  |> Array.sortBy (fun x -> x.Id)




let doRound (monkeys:monkey[]) =
  let Modulo =
    Array.map (fun x -> x.Test) monkeys
    |> Array.reduce (*)
    |> uint64
  let moveItem from ``to`` worryLevel =
    monkeys[``to``] <- {monkeys[``to``] with items = Array.append monkeys[``to``].items (Array.singleton worryLevel)}
    monkeys[from] <- {monkeys[from] with items = Array.tail monkeys[from].items; inspections = monkeys[from].inspections + (uint64 1)}
  let doMonkey monkey =
    let items = monkey.items
    let getWorryLevel x =
      monkey.Operation x
    let target x =
      (if x % monkey.Test = (uint64 0) then monkey.trueMonkeyId else monkey.falseMonkeyId) , x % Modulo
    Array.iter (fun item -> getWorryLevel item |> target |> fun x -> moveItem monkey.Id (fst x) (snd x)) items
  Array.iter doMonkey monkeys
  monkeys
  
let ms = parseInput input  
Array.fold (fun _ _ -> doRound ms) ms [|1..10000|]
|>  Array.map (fun x -> x.inspections)
|> Array.sortDescending
|> Array.take 2
|> Array.reduce (*)