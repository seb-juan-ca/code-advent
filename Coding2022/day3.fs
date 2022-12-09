open System.IO

module backpacking =
    let splitIn2 row =
        Seq.splitInto 2 row
    let removeDuplicatesOnSeq seq=
        Seq.distinct seq
    let cleanSplitted splittedSeq=
        splittedSeq|>Seq.map (fun x-> removeDuplicatesOnSeq x)
    let joinSeq seqOfSeq =
        Seq.collect (fun x->x) seqOfSeq
    let findDuplicated seq=
        seq|>Seq.groupBy (fun x->x)
           |>Seq.filter(fun (key,items)->Seq.length items>1)
           |>Seq.head
           |>fun (key, items)-> key
    let getRowItemType row=
        row 
            |> splitIn2 
            |> cleanSplitted
            |> joinSeq
            |> findDuplicated

    let lowerCaseItems=seq {0..25} |> Seq.map (fun x-> char (x+97))
    let upperCaseItems=seq {0..25} |> Seq.map (fun x-> char (x+65))
    let letterPriorityOnIndex=Seq.append lowerCaseItems upperCaseItems|>Array.ofSeq
    let getPriorityForLetter letter=
        Array.findIndex (fun x->x=letter) letterPriorityOnIndex
        |>(fun x->x+1)

    let getPriorityForRow row=
        row 
            |> getRowItemType
            |> getPriorityForLetter

    let sumPriorities rows =
        rows
            |> Seq.map (fun row-> getPriorityForRow row)
            |> Seq.sum
    
    let cleanDuplicateWithinRows rows=
        cleanSplitted rows
    
    let findBatch seq =
        seq 
            |> Seq.groupBy (fun x->x)
            |> Seq.filter (fun (key,items)->Seq.length items=3)
            |> Seq.head
            |> fun (key, items)->key
    
    let getBatch rows =
        rows
            |> cleanDuplicateWithinRows
            |> joinSeq
            |> findBatch
    
    let sumBatches rows =
        rows
            |> Seq.chunkBySize 3
            |> Seq.map (fun groupRows->getBatch groupRows)
            |> Seq.map (fun batch -> getPriorityForLetter batch)
            |> Seq.sum
    
    let sumPrioritiesFromFile filename =
        File.ReadLines filename |> sumPriorities
    
    let sumBatchesFromFile filename =
        File.ReadLines filename
            |> Seq.filter (fun x->x.Length>0)
            |> sumBatches

