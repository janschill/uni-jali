module Program

open System
open FSharp.Text.Lexing

open AbstractSyntax
open Transpiler
open Lexer
open Parser

let fromString (str: string): program =
    let lexbuf = LexBuffer<char>.FromString(str)
    try
        Parser.Main Lexer.Token lexbuf
    with exn ->
        failwithf "%s\n" (exn.Message)

let src = """
 type DisjointSum =
        Ctor1
      | Ctor2 Integerx
"""

let test = """x = 2
y = 3
func f x = x+y

func f x =
    x = 2
    x
end 

type DisjointSum = Ctor1 | Ctor2 Integer"""

[<EntryPoint>]
let main argv =
    let pgm = fromString "0"
    printf "%A" pgm
    let x = 42
    x
