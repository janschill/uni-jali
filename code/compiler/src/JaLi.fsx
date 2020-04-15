exception SyntaxError of int * int

[<EntryPoint>]
let main argv =
    printfn "Running jali"
    match argv with
    | [| "-i" |] -> printfn "%s" argv.[0]
    | _ -> printfn "Invalid command"
    0
