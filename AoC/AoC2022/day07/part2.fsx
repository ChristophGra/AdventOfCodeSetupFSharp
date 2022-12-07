open System
open System.IO
open System.Text.RegularExpressions

type file = {
    Name: string
    Size: int
}

type folder = {
    SubFolders: folder list
    CurrentFolder: string
    Files: file list
    FolderSize: int
}

let rec parse Folder  (lines: string list)=
    match lines with
    | current::tail when current.StartsWith("$") ->
        if current.StartsWith("$ cd") then
            if current = "$ cd .." then
               Folder, tail
            else
                let folderName = current.Substring(5)
                let subFolder =
                    match List.tryFind (fun x -> x.CurrentFolder = folderName) Folder.SubFolders with
                    | Some sub -> sub
                    | None -> {SubFolders = [];FolderSize =0; CurrentFolder = folderName; Files=[]}
                let subF,restLines = parse subFolder tail
                let newSubFolders = subF::List.where (fun x -> x.CurrentFolder <> folderName) Folder.SubFolders
                let newFolderSize = List.sumBy (fun x -> x.FolderSize) newSubFolders + List.sumBy (fun x -> x.Size) Folder.Files
                parse {Folder with SubFolders = newSubFolders; FolderSize = newFolderSize} restLines
                
        else if current.StartsWith("$ ls") then
            parse Folder tail
        else
            parse Folder tail
    | current::tail when current.StartsWith("dir") ->
       parse (* {Folder with
                   SubFolders =
                       match List.tryFind (fun x -> x.CurrentFolder = current.Split(" ").[1]) Folder.SubFolders with
                       | Some _ -> Folder.SubFolders
                       | None ->  {SubFolders = []
                                   CurrentFolder = current.Split(" ").[1]
                                   Files=[]}::Folder.SubFolders
                       } *) Folder tail
    | current::tail ->
        let reg = new Regex("(\d+) (.*)")
        let matches = reg.Match(current)
        let newFiles = {Size = matches.Groups.[1].Value |> int; Name = matches.Groups.[2].Value}::Folder.Files
        let newFolderSize = List.sumBy (fun x -> x.FolderSize) Folder.SubFolders + List.sumBy (fun x -> x.Size) newFiles
        parse {Folder with Files = newFiles; FolderSize = newFolderSize} tail
    | [] ->
        Folder, []
    
let testInput =
    """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k""".Replace("\r\n","\n").Split("\n")        

let input =
  (File.ReadAllLines @"AoC\AoC2022\day07\Input.txt")
  
  
let rec getFolders Folder = seq {
    yield Folder
    yield! Seq.collect getFolders Folder.SubFolders
    
}
let parsed =
    input
    |> List.ofArray
    |> parse {Files = []; CurrentFolder= "";FolderSize = 0; SubFolders = []}
    |> fun (x,_) -> x
    
let freeSpace = 70000000 - parsed.FolderSize
let neededSpace = 30000000 - freeSpace

let FolderToDelete =
    getFolders parsed
    |> Seq.sortBy (fun x -> x.FolderSize)
    |> Seq.where (fun x -> x.FolderSize > neededSpace)
    |> Seq.head