// For more information see https://aka.ms/fsharp-console-apps

let sumX (rows:seq<string>) =
    rows |>
        Seq.fold
            (fun acc v -> v+"hola")
            "--"
let z=sumX ["a"]
printfn "%A" z
