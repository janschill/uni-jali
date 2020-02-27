module Program

open System

open AbstractSyntax
open Interpreter
open Lexer
open Parser
open Microsoft.FSharp.Text.Lexing

[<EntryPoint>]
let main argv =
    printf fromString "5 + 5"
    0

let fromString (str: string): expr =
    lexbuffer = Lexing.LexBuffer<char>.FromString(str)
    try
        Parser.Main Lexer.Token lexbuffer
    with exn -> printf "Error"
