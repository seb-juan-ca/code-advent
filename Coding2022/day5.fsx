open System.IO
module supplyStackShifting =

    type crateStack = Map<int, list<char>>
    type moveInstruction = {toMoveCount:int;columnSourceIndex:int;columnTargetIndex:int}
    type cratePosition={crateIndex:int;crateName:char}

    // let loadStackFromTop (loadElements: int * string) =
    // let loadStackFromTop (loadElements: seq<int * string>) =
    let loadStackFromTop (loadElements:seq<cratePosition>) =
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
        let toMoveOnFilo = top //|>  Seq.rev |> List.ofSeq
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
    
    let parseMoveCrate (rows:seq<string>) =
        rows |>
            Seq.fold 
                (fun 
                    (cratePositions,moveInstructions,isInstruction)
                    (row)->
                    printfn $"{row}"
                    if row.Length=0 then
                        (cratePositions,moveInstructions,true)
                    elif isInstruction then
                        let newInstruction=parseInstruction row
                        (cratePositions, Seq.append moveInstructions [ newInstruction], isInstruction)
                    elif row.Length>0 && not (row.Trim().StartsWith "1") then
                        let newCratePositions=parseCrateStack row
                        (Seq.append cratePositions newCratePositions, moveInstructions, isInstruction)
                    else
                        (cratePositions,moveInstructions,isInstruction)                       
                    )
                (Seq.empty<cratePosition>, Seq.empty<moveInstruction>, false)

    let moveStackWithInstructions (stack:crateStack) (instructions:seq<moveInstruction>) =
        instructions
            |> Seq.fold
                (fun stack instruction -> moveFromStack stack instruction) 
                stack
    let moveStack (rows:seq<string>) =
        rows 
            |> parseMoveCrate 
            |> (fun (cratePositions, instructions, _) ->
                let originalStack=loadStackFromTop cratePositions
                moveStackWithInstructions originalStack instructions)
    let getTop (stack:crateStack) =
        stack.Keys
            |> Seq.sort 
            |> Seq.map (fun key -> Seq.last stack.[key])
            |> System.String.Concat
    let parseMoveCreateFile filename =
        File.ReadLines filename |> moveStack
supplyStackShifting.parseMoveCreateFile "day5.example.txt" |> supplyStackShifting.getTop
let movedStack=supplyStackShifting.parseMoveCreateFile "day5.txt"
supplyStackShifting.getTop movedStack