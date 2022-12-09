open System.IO
module rockPaperSizzors =
    type ScoringPoints={ lose: int; draw:int;win:int}
    type itemPoints={ rock: int; paper:int;sizzors:int}
    // find all by index ;)
    type Items=
        |rock=0
        |paper=1
        |sizzors=2
    type GameOutcome=
        |win=0
        |draw=1
        |lose=2

    let oponentToGameoutComeItemSelection =
        //      win           draw          lose
        [|
(*rock=0*)    [|Items.paper;  Items.rock;   Items.sizzors; |];
(*papper=1*)  [|Items.sizzors;Items.paper;  Items.rock;    |];
(*sizzors=2*) [|Items.rock;   Items.sizzors;Items.paper;   |];
        |]

    let createMatchScoring scoringPoints =
        let scoreMap= 
            //     rock=0              paper=1             sizzors=2
           [| 
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
    let getMyInputHint input =
        match input with
            | 'X' -> GameOutcome.lose
            | 'Y' -> GameOutcome.draw
            | 'Z' -> GameOutcome.win

    let matchPointsConfig=createMatchScoring {lose=0;draw=3;win=6}
    let itemPointsConfig=createItemScoring {rock=1;paper=2;sizzors=3}
    let getItemPoints item=
        itemPointsConfig[(int)item]

    let getMyItemPoints choice =
        let input=choice |> getMyInput 
        getItemPoints input

    let getMatchPointsFromItem (myItem:Items ) (otherItem:Items)=
        matchPointsConfig[(int)myItem][(int)otherItem]

    let getMatchPoints myChoice otherChoice =
        let myItem=getMyInput myChoice
        let otherItem=getOtherInput otherChoice
        getMatchPointsFromItem myItem otherItem

    let getTotalPoints myChoice otherChoice =
        getMyItemPoints myChoice + getMatchPoints myChoice otherChoice
    let calculateGameSum gameChoices=
        gameChoices 
            |> Seq.map (fun (otherChoice, myChoice)-> getTotalPoints myChoice otherChoice)
            |> Seq.sum
    let parseRow (row:string) = (row[0],row[2])
    let calculateGamesSum (sequence:seq<string>) =
        sequence |> Seq.filter (fun x->x.Length > 0) |>  Seq.map parseRow |> calculateGameSum

    let getMyChoiceFromOutcome (otherChoice:Items) (outcome:GameOutcome)=
        oponentToGameoutComeItemSelection[(int)otherChoice][(int)outcome]

    let getTotalPointsFromOutcome otherChoice outcomeHint=
        let myInputHint=getMyInputHint outcomeHint
        let otherItem=getOtherInput otherChoice
        let myItem=getMyChoiceFromOutcome otherItem myInputHint
        getMatchPointsFromItem myItem otherItem + getItemPoints myItem
    let calculateGameSumFromOutcome gameCheat=
        gameCheat 
            |> Seq.map (fun (otherChoice, outcomeHint)-> getTotalPointsFromOutcome otherChoice outcomeHint)
            |> Seq.sum
    let calculateGamesSumFromCheatOutcome (sequence:seq<string>) =
        sequence |> Seq.filter (fun x->x.Length > 0) |>  Seq.map parseRow |> calculateGameSumFromOutcome

    let getPointsFromFile filename =
        File.ReadLines filename |> calculateGamesSum

    let getPointsFromCheatFile filename =
        File.ReadLines filename |> calculateGamesSumFromCheatOutcome
        
