module Program

open System
open FSharp.Text.Lexing

open AbstractSyntax
open Transpiler
open Lexer
open Parser
open Interpreter

exception SyntaxError of int * int
    with override this.Message = sprintf "Syntax error at line %d, column %d." this.Data0 this.Data1

let fromString (str: string): expr =
    let lexbuf = LexBuffer<char>.FromString(str)
    try
        Parser.Main Lexer.Token lexbuf
    with 
      | Failure ("parse error")     -> raise <| SyntaxError ((lexbuf.EndPos.Line+1) , lexbuf.EndPos.Column)
      | _                           -> reraise ()

let evalString (str: string): value =
    eval (fromString str) []

let testEval = """
3 + 4
7 == 9
"""

let src = """
 type DisjointSum =
        Ctor1
      | Ctor2 Integerx
"""

let test = """
x = 2
y = 3
func f x = x+y

func f x =
    x = 2
    x
end 

type DisjointSum = Ctor1 | Ctor2 Integer
"""

[<EntryPoint>]
let main argv =
    let pgm = fromString "0"
    printf "%A" pgm
    let x = 42
    x
