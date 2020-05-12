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
    | Failure ("parse error") ->
        raise
        <| SyntaxError((lexbuf.EndPos.Line + 1), lexbuf.EndPos.Column)
    | _ -> reraise ()

let evalString (str: string): Value = eval (fromString str) []
let reduceString (str: string): Expr = reduce (fromString str) []
let transpileString (str: string): String = transpile (fromString str)

let load = System.IO.File.ReadAllText

// let program = load "./examples/program.javi"
// let html = load "./examples/html.javi"
// let button = load "./examples/button.javi"

let difference = "../core/src/difference.jali"
let diff = load difference


// let abpro = fromString program
// let trpro = transpile abpro

[<EntryPoint>]
let main argv =
    let pgm = fromString "0"
    printf "%A" pgm
    let x = 42
    x
