module Program

open System
open FSharp.Text.Lexing

open AbstractSyntax
open Transpiler
open Lexer
open Parser
open Interpreter

exception SyntaxError of int * int with
    override this.Message = sprintf "Syntax error at line %d, column %d." this.Data0 this.Data1

let fromString (str: string): expr =
    let lexbuf = LexBuffer<char>.FromString(str)
    try
        Parser.Main Lexer.Token lexbuf
    with
    | Failure("parse error") -> raise <| SyntaxError((lexbuf.EndPos.Line + 1), lexbuf.EndPos.Column)
    | _ -> reraise()

let evalString (str: string): value = eval (fromString str) []

let testEval = """
3 + 4
7 == 9
"""

let simplefunction = """
func f x = x + y
"""
let simplefunction2 = """
func f x =
    x = 2
    x
end
"""
let complexfunction = """
func f x y z =
  k = x + y * z
  k
end
"""

let adta = """type DisjointSum = Ctor1 Integer | Ctor2 String String"""
let adtb = """type DisjointSum =
  Ctor1 Integer | Ctor2 String String
"""
let adtc = """type DisjointSum =
    Ctor1 Integer
  | Ctor2 String String
"""
let adtd = """type DisjointSum =
   Ctor1 Integer |
   Ctor2 String String
"""
let adte = """type DisjointSum =
   | Ctor1 Integer
   | Ctor2 String String
"""
let pattern = """
func f x y = match x b c with
   | Ctor1 => 42
   | Ctor2 => 43
   | Ctor3 => 44
   | _     => 45
end
"""
let ifstmt = """
if 3>4
then 3
else 4
"""

[<EntryPoint>]
let main argv =
    let pgm = fromString "0"
    printf "%A" pgm
    let x = 42
    x
