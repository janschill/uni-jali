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

let inferType = TypeInference.inferType
let inferTypeFromString = fromString >> inferType

let testEval = """
3 + 4
"""

let simplefunction = """
y = 1;

func f x =
  x + y
end
f
"""

let simplefunction2 = """
func f x =
    x = 2;
    x
end
f
"""
let complexfunction = """
func f x y z =
  k = x + y * z;
  k
end
f
"""
let ifstmt = """
if 3 > 4
then 3
else 4
"""

let easypattern = """
func f x =
match x with
   | Ctor1 -> 42
   | Ctor2 -> 45
end
0
"""
let pattern = """
func f x y =
match x with
   | 1 -> 42
   | 2 -> 43
   | 3 -> 44
   | 4 -> 45

end
f (3) (4)
"""

let wcpattern = """
func f x y =
match x with
   | 1 -> 42
   | 2 -> 43
   | 3 -> 44
   | _ -> 45

end
f (5) (0)
"""

let complexpattern = """
func f x y =
match (x, y) with
   | (1, (2, _)) -> 40
   | (1, (2, 5)) -> 41
   | (1, (3, 1)) -> 42
   | (1, (3, _)) -> 43
   | (1, (3, 7)) -> 44
end
f (1) ((3, 5))
"""

let booleanpattern = """
func f x y =
match (x, y) with
   | (true, (true, _)) -> 40
   | (true, (true, true)) -> 41
   | (true, (true, false)) -> 42
   | (false, (false, _)) -> 43
   | (false, (true, _)) -> 44
end
f (false) ((true, 5))
"""

let apply = """
func f x y z =
  k = x + y * z;
  k
end

f 3 2 1
"""

let tuple = """
x = (1,2);
x
"""

let adt = """type DisjointSum =
    Ctor1 Integer
  | Ctor2 String String;
  Ctor2 "a" "b"
"""

let adtpattern = """
type DisjointSum =
    Ctor1 Integer
  | Ctor2 String String;
"""

let adtvalue = """
x = Ctor1 0 1;
x
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
