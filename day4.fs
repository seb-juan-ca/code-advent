open System.IO
module cleanCampSections =
    let isFullyWithIn (firstStart,firstEnd) (secondStart,secondEnd) =
        if firstStart<=secondStart && firstEnd>=secondEnd then true
        elif secondStart<=firstStart && secondEnd>=firstEnd then true
        else false

    let parseInterval (value:string) =
        let parsed = value.Split [|'-'|]
        ((int)parsed[0],(int)parsed[1])

    let parseRowToTupes (row:string) =
        let parsed = row.Split [|','|]
        (parseInterval parsed[0],parseInterval parsed[1])
    
    let hasParsedRowFullOverlap i row =
        let (start,``end``) = parseRowToTupes row
        isFullyWithIn start ``end``
    
    let getFullOverlapCount (rows:seq<string>) =
        rows 
            |> Seq.filter (fun x->x.Length>0)
            |> Seq.mapi hasParsedRowFullOverlap
            |> Seq.filter (fun x->x)
            |> Seq.length

    let getFullOverlapCountFile filename =
        File.ReadLines filename |> getFullOverlapCount

cleanCampSections.getFullOverlapCountFile "day4.example.txt"
cleanCampSections.getFullOverlapCountFile "day4.txt"
