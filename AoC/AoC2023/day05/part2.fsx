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
      
let mapOnce currentValue (ranges:range array)  =
  let applicable = Array.filter (fun (x:range) -> x.destRangeStart <= fst currentValue && x.destRangeStart + x.rangeLength - 1UL  >= fst currentValue) ranges
  //Console.WriteLine applicable.Length
  //Console.WriteLine currentValue 
  //Array.iter (fun x -> Console.WriteLine $"{x}") applicable
  match applicable.Length with
  | 1 -> fst currentValue + applicable[0].sourceRangeStart - applicable[0].destRangeStart,snd currentValue
  
  | _ ->
    
    fst currentValue,snd currentValue


let solveOneSeed (data: parsed[]) (source: uint64) =
  Array.map (fun x -> x.fromTo) data
  |> Array.fold mapOnce (source,source)
  
  
let solveAllSeeds (data:parsed[]) (seeds:(uint64 * uint64)[]) =
  Seq.map (solveOneSeed data) (Seq.initInfinite uint64)
  |> Seq.where (fun (value,_) -> Array.exists (fun x -> fst x <= value && fst x + snd x - 1UL >= value) seeds)
  |> Seq.head
                                                                                  
  
solveAllSeeds ((snd >> Array.rev) parsed) (Array.chunkBySize 2 (fst parsed) |> Array.map (fun x -> x[0],x[1]))
|> (snd >> Console.WriteLine)

//solveOneSeed (snd parsed) 17UL