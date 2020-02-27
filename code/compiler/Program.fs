open System

open AbstractSyntax
open Interpreter

[<EntryPoint>]
let main argv =
    eval(Prim("+", ConstantInteger 5, ConstantInteger 10))
    0
