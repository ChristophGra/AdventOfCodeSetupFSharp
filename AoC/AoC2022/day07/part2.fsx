open System.IO
open System.Text.RegularExpressions

type file = {
    Name: string
    Size: int
}

type folder = {
    CurrentFolder: string
    FolderSize: int
    SubFolders: folder list
    Files: file list
}

let rec parse currentFolder  (lines: string list)=
    let discardRow tail = parse currentFolder tail
    match lines with
    | current::tail when current.StartsWith("$") ->
        if current.StartsWith("$ cd") then
            if current = "$ cd .." then
               currentFolder, tail
            else
                let folderName = current.Substring(5)
                let subFolder =
                    match List.tryFind (fun x -> x.CurrentFolder = folderName) currentFolder.SubFolders with
                    | Some sub -> sub
                    | None -> {SubFolders = [];FolderSize =0; CurrentFolder = folderName; Files=[]}
                    
                let subF,restLines = parse subFolder tail
                let newSubFolders = subF::List.where (fun x -> x.CurrentFolder <> folderName) currentFolder.SubFolders
                let newFolderSize = List.sumBy (fun x -> x.FolderSize) newSubFolders + List.sumBy (fun x -> x.Size) currentFolder.Files
                
                parse {currentFolder with SubFolders = newSubFolders; FolderSize = newFolderSize} restLines
                
        else if current.StartsWith("$ ls") then
            discardRow tail
        else
            discardRow tail
    | current::tail when current.StartsWith("dir") ->
        discardRow tail
    | current::tail ->
        let reg = Regex("(\d+) (.*)")
        let matches = reg.Match(current)
        let newFiles = {Size = matches.Groups[1].Value |> int; Name = matches.Groups[2].Value}::currentFolder.Files
        let newFolderSize = List.sumBy (fun x -> x.FolderSize) currentFolder.SubFolders + List.sumBy (fun x -> x.Size) newFiles
        parse {currentFolder with Files = newFiles; FolderSize = newFolderSize} tail
    | [] ->
        currentFolder, []
    

let input =
  (File.ReadAllLines @"AoC\AoC2022\day07\Input.txt")
  
  
let rec getFolders currentFolder = seq {
    yield currentFolder
    yield! Seq.collect getFolders currentFolder.SubFolders
    
}
let parsed =
    input
    |> List.ofArray
    |> List.tail
    |> parse {Files = []; CurrentFolder= "/";FolderSize = 0; SubFolders = []}
    |> fun (x,_) -> x
    
let freeSpace = 70000000 - parsed.FolderSize
let neededSpace = 30000000 - freeSpace

let FolderToDelete =
    getFolders parsed
    |> Seq.sortBy (fun x -> x.FolderSize)
    |> Seq.where (fun x -> x.FolderSize > neededSpace)
    |> Seq.head
let FolderSizeToDelete =
    FolderToDelete.FolderSize