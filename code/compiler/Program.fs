[<AutoOpen>]
module Program

open System
open FSharp.Text.Lexing

open AbstractSyntax
open Transpiler
open Lexer
open Parser
open Interpreter
open TypeInference
open Compiler

exception SyntaxError of int * int with
    override this.Message = sprintf "Syntax error at line %d, column %d." this.Data0 this.Data1

let fromString (str: string): Expr =
    let lexbuf = LexBuffer<char>.FromString(str)
    try
        Parser.Main Lexer.Token lexbuf
    with
    | Failure("parse error") -> raise <| SyntaxError((lexbuf.EndPos.Line + 1), lexbuf.EndPos.Column)
    | _ -> reraise()

let evalString (str: string): Value = eval (fromString str) []
let reduceString (str: string): Expr = reduce (fromString str) []
let inferString (str: string): string = inferType (fromString str)

let test = """
type myType = Ctor1 | Ctor2 String Integer;
x = Ctor1;
match x with
| Ctor1 -> 1
| Ctor2 s i -> 2
| Ctor2 _ _ -> 3
"""

let program = System.IO.File.ReadAllText "./examples/program.javi"
let html = System.IO.File.ReadAllText "./examples/html.javi"

// let abpro = fromString program
// let trpro = transpile abpro

[<EntryPoint>]
let main argv =
    let pgm = fromString "0"
    printf "%A" pgm
    let x = 42
    x
