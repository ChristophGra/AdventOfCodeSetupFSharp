open System.IO
open System.Security.Cryptography
open System.Text



let input = (File.ReadAllText "AoC\AoC2015\day04\Input.txt").Trim()


let result =
    let md = MD5.Create()
    let rec iterator i =
        seq {
            yield md.ComputeHash(Encoding.ASCII.GetBytes(input + $"{i}")), i
            yield! iterator (i + 1)
        }
    iterator 0
result
|> Seq.map (fun (hash, index) -> Array.fold (fun curr (x:byte) -> curr + x.ToString("x2")) "" hash, index)
|> Seq.find (fun x -> (fst x).StartsWith("000000"))
    
    
