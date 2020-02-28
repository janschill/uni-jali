module Transpiler

open AbstractSyntax

let rec eval (e: expr): string =
    match e with
    | ConstantInteger i -> string i
    | Prim(operation, expression1, expression2) ->
        let value1 = eval expression1
        let value2 = eval expression2
        match (operation) with
        | ("+") -> sprintf "%s + %s" value1 value2
        | ("-") -> sprintf "%s - %s" value1 value2
        | ("*") -> sprintf "%s * %s" value1 value2
        | ("/") -> sprintf "%s / %s" value1 value2
