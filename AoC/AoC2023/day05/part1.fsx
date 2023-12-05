open System
open System.IO

type range = {
  destRangeStart: uint64
  sourceRangeStart: uint64
  rangeLength: uint64
}
type parsed = {
  fromTo: range array
  }
let tinput = """seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"""
let input =
  (File.ReadAllText @"AoC\AoC2023\day05\Input.txt")
    
    
let parsed =
  input.Split("\n\n")
    |> Array.map (fun x -> x.Split(":")[1])
    |> fun x ->
      let seeds = (x[0]).Trim().Split(" ") |> Array.map uint64
      let maps = Array.tail x
      let (m:parsed []) =
        Array.map (fun (y:string) -> y.Split("\n")) maps
        |> Array.map (Array.map (fun (y:string) -> y.Trim().Split(" ")))
        |> Array.map (Array.where (Array.length >> (=) 3))
        |> Array.map (Array.map (Array.map uint64))
        |> Array.map (fun y ->
          let ranges = (Array.map (fun (z:uint64 array) -> {destRangeStart = z[0]; sourceRangeStart = z[1]; rangeLength = z[2]})) y
          {fromTo = ranges})
      seeds, m
      
let mapOnce seed (ranges:range array)  =
  let applicable = Array.filter (fun (x:range) -> x.sourceRangeStart <= seed && x.sourceRangeStart + x.rangeLength - 1UL >= seed) ranges
  
  match applicable.Length with
  | 1 -> seed + applicable[0].destRangeStart - applicable[0].sourceRangeStart
  
  | _ ->
    //Console.WriteLine applicable.Length
    //Array.iter (fun x -> Console.WriteLine $"{x}") applicable
    seed  


let solveOneSeed (data: parsed[]) (seed: uint64) =
  Array.map (fun x -> x.fromTo) data
  |> Array.fold mapOnce seed
  
  
let solveAllSeeds (data:parsed[]) (seeds:uint64[]) =
  Array.map (solveOneSeed data) seeds
solveAllSeeds (snd parsed) (fst parsed)
|> Array.min