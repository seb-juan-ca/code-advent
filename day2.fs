open System.IO
module rockPaperSizzors =
    type ScoringPoints={ lose: int; draw:int;win:int}
    type itemPoints={ rock: int; paper:int;sizzors:int}
    // find all by index ;)
    type Items=
        |rock=0
        |paper=1
        |sizzors=2

    let createMatchScoring scoringPoints =
        let scoreMap= 
            //     rock=0              paper=1             sizzors=2
   (*my*)   [| 
   (*rock=0*)   [| scoringPoints.draw; scoringPoints.lose; scoringPoints.win |];
   (*papper=1*) [| scoringPoints.win;  scoringPoints.draw; scoringPoints.lose |];
   (*sizzors=2*)[| scoringPoints.lose; scoringPoints.win;  scoringPoints.draw |];
            |]
        scoreMap
    let createItemScoring itemPoints =
        [| itemPoints.rock; itemPoints.paper; itemPoints.sizzors|]
    
    let getOtherInput input =
        match input with   
            | 'A' -> Items.rock
            | 'B' -> Items.paper
            | 'C' -> Items.sizzors
    let getMyInput input =
        match input with    
            | 'X' -> Items.rock
            | 'Y' -> Items.paper
            | 'Z' -> Items.sizzors

    let matchPointsConfig=createMatchScoring {lose=0;draw=3;win=6}
    let itemPointsConfig=createItemScoring {rock=1;paper=2;sizzors=3}
    let getMyItemPoints choice =
        let input=choice |> getMyInput 
        itemPointsConfig[(int)input]
    let getMatchPoints myChoice otherChoice =
        let myItem=getMyInput myChoice
        let otherItem=getOtherInput otherChoice
        scoreMap[(int)myItem][(int)otherItem]
    let getTotalPoints myChoice otherChoice =
        getMyItemPoints myChoice + getMatchPoints myChoice otherChoice
    let calculateGameSum gameChoices=
        gameChoices 
            |> Seq.map (fun (otherChoice, myChoice)-> getTotalPoints myChoice otherChoice)
            |> Seq.sum
    let parseRow (row:string) = (row[0],row[2])
    let calculateGamesSum (sequence:seq<string>) =
        sequence |> Seq.filter (fun x->x.Length > 0) |>  Seq.map parseRow |> calculateGameSum
    
    let getPointsFromFile filename =
        File.ReadLines filename |> calculateGamesSum
        
