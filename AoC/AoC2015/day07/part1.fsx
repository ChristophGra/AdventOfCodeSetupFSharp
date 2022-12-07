open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Text.RegularExpressions
open Microsoft.FSharp.Core
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames
type binaryOperation = {
    first: string
    second: string
    output: string
}
type unaryOperation = {
    input: string
    output: string
}
type inputOperation = {
    input: uint16
    output: string
}

type logicOperation =
    | AndOperation of binaryOperation
    | OrOperation of binaryOperation
    | LshiftOperation of binaryOperation
    | RshiftOperation of binaryOperation
    | NotOperation of unaryOperation
    | Pipe of unaryOperation
    | InputOperation of inputOperation

let input =
    File.ReadAllLines "AoC\AoC2015\day07\Input.txt"
    |> Array.where (fun x -> x.Length > 0)
let testInput =
    """123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i""".Split("\n")

let parseLine line:logicOperation  =
    let binaryOp = Regex("(?<first>\S+) (?<operator>AND|OR|[LR]SHIFT) (?<second>\S+) -> (?<output>\S+)").Match(line)
    let unaryOp = Regex("(?<type>NOT|) ?(?<input>\S+) -> (?<output>\S+)").Match(line)
    let numericInput = Regex("(?<input>\d+) -> (?<output>\S+)").Match(line)
    if binaryOp.Success then
        let groups = binaryOp.Groups
        match groups.["operator"].Value with
        | "AND" -> AndOperation { first = groups["first"].Value; second = groups.["second"].Value; output = groups["output"].Value}
        | "OR"  -> OrOperation { first = groups["first"].Value; second = groups.["second"].Value; output = groups["output"].Value}
        | "LSHIFT" -> LshiftOperation { first = groups["first"].Value; second = groups.["second"].Value; output = groups["output"].Value}
        | "RSHIFT" -> RshiftOperation { first = groups["first"].Value; second = groups.["second"].Value; output = groups["output"].Value}
        | _ ->
            Console.WriteLine line
            raise (new Exception())
    else if numericInput.Success then
        let groups = unaryOp.Groups
        InputOperation {input = UInt16.Parse groups["input"].Value; output = groups["output"].Value}
    else if unaryOp.Success then
        let groups = unaryOp.Groups
        //Console.WriteLine line
        //Console.WriteLine $"""input {groups["input"].Value}, output = {groups["output"].Value}, type = {groups["type"].Value}"""
        match groups["type"].Value with
        | "NOT" -> NotOperation { input = groups["input"].Value; output = groups["output"].Value}
        | "" -> Pipe { input = groups["input"].Value; output = groups["output"].Value}
        | _ ->
            Console.WriteLine line
            raise (new Exception())
    else
        Console.WriteLine line
        raise (new Exception())
        
        
let getOutputName (op:logicOperation) =
    match op with
    | Pipe o -> o.output 
    | NotOperation o -> o.output 
    | InputOperation o -> o.output 
    | AndOperation o -> o.output 
    | OrOperation o -> o.output 
    | LshiftOperation o -> o.output 
    | RshiftOperation o -> o.output 
let parsed =
    let grouper logicOp =
        match logicOp with 
        | InputOperation _ -> 0
        | _ -> 1
    
    Array.map parseLine input
    |> Array.fold (fun (dict:ImmutableDictionary<string,logicOperation>) (x:logicOperation) ->  dict.Add((getOutputName x),x)) (ImmutableDictionary<string,logicOperation>.Empty)
    

let isOutputNamed name (op:logicOperation)  =
    (getOutputName op) = name
let replaceElem (dict:ImmutableDictionary<'a,'b>) key newval =
    let dictWithRemoved = dict.Remove(key)
    dictWithRemoved.Add(key,newval)
    
    
    
let rec fillInData (circuits:ImmutableDictionary<string,logicOperation>) (target:string): uint16 * ImmutableDictionary<string,logicOperation> =
    //Console.WriteLine $"parsing {target}"
    
    match UInt16.TryParse target with
    | true, num -> num, circuits
    | false, _ ->
        let currCircuit =
            match circuits.TryGetValue target with
            | true,x -> x
            | false,_ -> raise (new ArgumentException($"searchTarget: {target}"))
            
        match currCircuit with
        | InputOperation o -> o.input, circuits
        | Pipe o -> fillInData circuits o.input
        | AndOperation o ->
            let first,dict = (fillInData circuits o.first)
            let second, newdict = (fillInData dict o.second)
            let result = first &&& second
            let resultDict = replaceElem newdict target (InputOperation {input = result; output = target})
            result, resultDict
        | OrOperation o ->
            let first,dict = (fillInData circuits o.first)
            let second, newdict = (fillInData dict o.second)
            let result = first ||| second
            let resultDict = replaceElem newdict target (InputOperation {input = result; output = target})
            result, resultDict
        | LshiftOperation o ->
            let first, newdict = (fillInData circuits o.first)
            let result = first <<< int o.second
            let resultDict = replaceElem newdict target (InputOperation {input = result; output = target})
            result, resultDict
        | RshiftOperation o ->
            let first, newdict = (fillInData circuits o.first)
            let result = first >>> int o.second
            let resultDict = replaceElem newdict target (InputOperation {input = result; output = target})
            result, resultDict
        | NotOperation o ->
            let first, newdict = (fillInData circuits o.input)
            let result = ~~~first 
            let resultDict = replaceElem newdict target (InputOperation {input = result; output = target})
            result, resultDict
                
       
   
let res = fillInData parsed "a"