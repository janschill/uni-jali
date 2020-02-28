module Program

open System
open FSharp.Text.Lexing

open AbstractSyntax;
// open Interpreter
open Lexer;
open Parser;


let fromString (str: string): expr =
    let lexbuf = LexBuffer<char>.FromString(str)
    try
        Parser.Main Lexer.Token lexbuf
    with 
        | exn -> failwithf "%s\n" 
                  (exn.Message)


[<EntryPoint>]
let main argv =
    fromString "5 + 5"
    0