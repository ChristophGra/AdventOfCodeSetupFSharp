open System.IO


let input =
  (File.ReadAllText @"AoC\AoC2022\day03\Input.txt").Replace("\r\n","\n").Split("\n")
  |> Array.map (fun x -> Array.splitInto 2 (x.ToCharArray()) |> Array.map Set.ofArray)
  |> Array.map Set.intersectMany
  |> Array.map Set.maxElement//Evil hackery to retrieve the only element
  |> Array.map (fun x -> match System.Char.IsLower x with true -> int x - int 'a' + int 1 | false -> int x - int 'A' + int 27)
  |> Array.sum 
  
  