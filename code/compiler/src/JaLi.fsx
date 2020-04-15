exception SyntaxError of int * int

[<EntryPoint>]
let main (paramList: string []): int =
    try
        match paramList with
        | [| "-i"; file |] ->
            printfn "\n\nIIIIIII: %s\n" "0"
        | [| "-r"; file |] ->
            printfn "\n\nRRRRRRR: %s\n" "0"
        | _ ->
            printfn "\n\n----------: %s\n" "0"
        0
    with SyntaxError(line, col) ->
        System.Environment.Exit 1
        1
