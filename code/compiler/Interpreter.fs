module Interpreter

open AbstractSyntax

let rec eval (e: expr): int =
    match e with
    | ConstantInteger i -> i
    | Prim(operation, expression1, expression2) ->
        let value1 = eval expression1
        let value2 = eval expression2
        match (operation) with
        | ("+") -> value1 + value2
