open System.IO
module caloriesParser=
    let getCurrentMax (maxes:seq<int>, groupMax:int) (el:string) =
        let isEmpty = el.Length=0
        match isEmpty with
        | true -> (Seq.append maxes [groupMax], 0)
        | false -> (maxes, groupMax + (int el))

    let getMax sequence=
        let (accMax,lastGroupMax)=Seq.fold getCurrentMax ([],0) sequence
        accMax
            |> Seq.sortDescending
            |> Seq.distinct
            |> Seq.truncate 3
    
    let getMaxFromFile filename=
        File.ReadLines filename |> getMax

    let getMax3FromFile filename
        File.ReadLines filename |> getMax |> Seq.sum;


