open System.IO
module supplyStackShifting =

    type crateStack = Map<int, list<char>>
    type moveInstruction = {toMoveCount:int;columnSourceIndex:int;columnTargetIndex:int}
    type createPosition={crateIndex:int;crateName:char}

    // let loadStackFromTop (loadElements: int * string) =
    // let loadStackFromTop (loadElements: seq<int * string>) =
    let loadStackFromTop (loadElements:seq<createPosition>) =
        loadElements |>
            // Seq.fold (fun (newStack:Map<int,list<string>>) (createIndex, createName)->
            Seq.fold (fun (newStack) createPosition->
                let found=Map.tryFind createPosition.crateIndex newStack
                let crateColumn =
                    match found with
                    | Some x -> createPosition.crateName :: x
                    | None -> [createPosition.crateName]
                Map.add createPosition.crateIndex crateColumn newStack
            ) Map.empty<int,list<char>>
    let moveFromStack (stack:crateStack) (instructions:moveInstruction) =
        printfn $"{stack}"
        let targetColumn = Map.find instructions.columnTargetIndex stack
        let sourceColumn = Map.find instructions.columnSourceIndex stack
        let (bottom,top) = sourceColumn |> List.splitAt (sourceColumn.Length - instructions.toMoveCount)
        printfn $"split: bottom {bottom} top {top} targetColumn {targetColumn}"
        let toMoveOnFilo = top |>  Seq.rev |> List.ofSeq
        let withNewTarget = Map.add instructions.columnTargetIndex (targetColumn @ toMoveOnFilo) stack
        printfn $"new: {withNewTarget}"

        Map.add instructions.columnSourceIndex bottom withNewTarget
    
    let parseCrateStack (row:string) =
        row 
            |> Seq.mapi (fun i x -> (i, System.Char.IsLetter x, x))
            |> Seq.filter (fun (i, isLetter, x) -> isLetter)
            |> Seq.map (fun (i, isLetter, x) -> {crateIndex=((i-1)/4)+1;crateName=x})
    let instructionRowRegex = @"^move (\d+) from (\d+) to (\d+)";
    let parseInstruction (row:string) =
        let result=System.Text.RegularExpressions.Regex.Match(row,instructionRowRegex)
        {toMoveCount=int result.Groups[1].Value;columnSourceIndex=int result.Groups[2].Value;columnTargetIndex=int result.Groups[3].Value}    
    let parseMoveCreateFile filename =
        File.ReadLines filename 
            |> Seq.fold (fun (groups:seq<seq<string>>,currentGroup:seq<string>) row -> 
                printfn $"{row}"
                if row.Length = 0 then 
                    (Seq.append groups [currentGroup],[]) 
                else  
                    (groups, Seq.append currentGroup [row])) (Seq.empty,Seq.empty)
supplyStackShifting.parseMoveCreateFile "day5.example.txt"
supplyStackShifting.parseCrateStack "    [D]"
supplyStackShifting.parseInstruction "move 5 from 9 to 8"

["a";"c";"";"b";"e"]|> Seq.fold (fun (groups:seq<seq<string>>,currentGroup:seq<string>) row -> 
            if row.Length = 0 then 
                (Seq.append groups [currentGroup],[]) 
            else  
                (groups, Seq.append currentGroup [row])) (Seq.empty,Seq.empty)

//     [D]    
// [N] [C]    
// [Z] [M] [P]
//  1   2   3 
let rows=[
"    [V] [G]             [H]        "
"[Z] [H] [Z]         [T] [S]        "
"[P] [D] [F]         [B] [V] [Q]    "
"[B] [M] [V] [N]     [F] [D] [N]    "
"[Q] [Q] [D] [F]     [Z] [Z] [P] [M]"
"[M] [Z] [R] [D] [Q] [V] [T] [F] [R]"
"[D] [L] [H] [G] [F] [Q] [M] [G] [W]"
"[N] [C] [Q] [H] [N] [D] [Q] [M] [B]"
" 1   2   3   4   5   6   7   8   9 "
]
// let rows=[
//     "    [D]    "
//     "[N] [C]    "
//     "[Z] [M] [P]"
//     " 1   2   3 "
// ]
let instructions = rows |> Seq.map supplyStackShifting.parseCrateStack
let stack=supplyStackShifting.loadStackFromTop [
    {crateIndex=2;crateName='D'}
    {crateIndex=1;crateName='N'}
    {crateIndex=2;crateName='C'}
    {crateIndex=1;crateName='Z'}
    {crateIndex=2;crateName='M'}
    {crateIndex=3;crateName='P'}
    ]

// move 1 from 2 to 1
// move 3 from 1 to 3
// move 2 from 2 to 1
// move 1 from 1 to 2
let instructions:list<supplyStackShifting.moveInstruction> = [
    {toMoveCount=1;columnSourceIndex=2;columnTargetIndex=1}
    {toMoveCount=3;columnSourceIndex=1;columnTargetIndex=3}
    {toMoveCount=2;columnSourceIndex=2;columnTargetIndex=1}
    {toMoveCount=1;columnSourceIndex=1;columnTargetIndex=2}
]
instructions
    |> Seq.fold
        (fun stack instruction -> supplyStackShifting.moveFromStack stack instruction) 
        stack
supplyStackShifting.moveFromStack stack {columnSourceIndex=5;toMoveCount=2;columnTargetIndex=1}