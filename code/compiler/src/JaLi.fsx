open System
open System.Text
open AbstractSyntax
open FSharp.Text.Lexing

exception SyntaxError of int * int

let parseString (s: string): AbstractSyntax.Expr =
    Parser.Main Lexer.Token <| LexBuffer<_>.FromBytes(Encoding.UTF8.GetBytes s)

let interpret (fileName: string): Unit =
    let program = System.IO.File.ReadAllText fileName
    let result = Interpreter.eval (parseString program) []
    printfn "\n\nResult for %s is: %s" fileName (AbstractSyntax.printValue result)


[<EntryPoint>]
let main argv =
    printfn "Running jali …"
    match argv with
    | [| "-i"; file |] -> interpret argv.[1]
    | _ -> printfn "Invalid command"
    0
