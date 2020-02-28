module Program

open System
open FSharp.Text.Lexing

open AbstractSyntax
// open Interpreter
open Lexer
open Parser


let fromString (str: string): expr =
    let lexbuf = LexBuffer<char>.FromString(str)
    try
        Parser.Main Lexer.Token lexbuf
    with exn ->
        failwithf "%s\n" (exn.Message)


[<EntryPoint>]
let main argv =
    (*

    let input = "5 * 5";;
    let lexbuf = LexBuffer<char>.FromString(input);;
    let ts = Lexer.Token lexbuf;;
    let ts1 = Lexer.Token lexbuf;;
    let ts2 = Lexer.Token lexbuf;;
    let ts3 = Lexer.Token lexbuf;;

    *)
    0
