open System.IO
module caloriesParser=
    let getCurrentMax (max:int, groupMax:int) (el:string) =
        let isEmpty = el.Length=0
        let currentMax=if max<groupMax then groupMax else max
        match isEmpty with
        | true -> (currentMax, 0)
        | false -> (max, groupMax + (int el))

    let getMax sequence=
        let (accMax,lastGroupMax)=Seq.fold getCurrentMax (0,0) sequence
        if accMax<lastGroupMax then
            lastGroupMax
        else
            accMax
    
    let getMaxFromFile filename=
        File.ReadLines filename |> getMax


