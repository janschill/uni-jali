[<AutoOpen>]
module Program

open System
open FSharp.Text.Lexing

open AbstractSyntax
open Transpiler
open Lexer
open Parser
open Interpreter
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
let transpileString (str: string): String = transpile (fromString str)

let program = System.IO.File.ReadAllText "./examples/program.javi"
let html = System.IO.File.ReadAllText "./examples/html.javi"
let button = System.IO.File.ReadAllText "./examples/button.javi"
let specializer = System.IO.File.ReadAllText "./src/core/specializer.jali"

// let abpro = fromString program
// let trpro = transpile abpro

[<EntryPoint>]
let main argv =
    let pgm = fromString "0"
    printf "%A" pgm
    let x = 42
    x
