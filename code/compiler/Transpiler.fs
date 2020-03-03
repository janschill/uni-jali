module Transpiler

open AbstractSyntax

let rec eval (e: expr): string =
    match e with
    | ConstantInteger i -> sprintf "%s" (string i)
    | Variable v -> string v
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
    | Function(name, parameter, expressions) ->
        let rec evalExpressions es = 
            match es with 
            | e::exs -> eval e :: evalExpressions exs
            | [] -> []
        let exps = evalExpressions expressions
        sprintf "function %s(%s) {\n" name parameter |> ignore
        List.map (fun e -> sprintf "%s;\n" e) exps |> ignore
        sprintf "return %s;\n}" "something??"
