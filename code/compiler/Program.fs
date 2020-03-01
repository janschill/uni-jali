module Program

open System
open FSharp.Text.Lexing

open AbstractSyntax
open Transpiler
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
    open Program;;
    open FSharp.Text.Lexing;;
let str = 
"x = 1 
z = 2
        y = 1";;
   let lexbuf = LexBuffer<char>.FromString(str);;
   Lexer.Token lexbuf;;
*)
    // let dream = ""
    // printf "%s" (eval (fromString (str)))
    printf ""
    0
(*
    func addTwo p =
        p + 2
        p
    addTwo 5
*)

