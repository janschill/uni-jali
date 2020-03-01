module Transpiler

open AbstractSyntax

let rec eval (e: expr): string =
    match e with
    | ConstantInteger i -> string i
    | Let(name, expression) -> sprintf "const %s = %s;" name (eval expression)
    | Prim(operation, expression1, expression2) ->
        let value1 = eval expression1
        let value2 = eval expression2
        match (operation) with
        | ("+") -> sprintf "%s + %s" value1 value2
        | ("-") -> sprintf "%s - %s" value1 value2
        | ("*") -> sprintf "%s * %s" value1 value2
        | ("/") -> sprintf "%s / %s" value1 value2
        | ("%") -> sprintf "%s %s %s" value1 "%" value2
(*
const x = 1;
let y = 1;

function name(parameter) {
    return parameter;
}

{
    key: "value"
}

if (true) {

} else if (false) {

} else {

}

*)

