module Transpiler

open AbstractSyntax

let rec transpile (e: expr): string =
    match e with
    | Program p ->
        let rec evalExpressions es =
            match es with
            | [] -> ""
            | e :: exs -> sprintf "%s%s" (transpile e) (evalExpressions exs)
        evalExpressions p
    | ConstantInteger i -> sprintf "%s" (string i)
    | ConstantBoolean b -> sprintf "%s" (string b)
    | Variable v -> string v
    | Let(name, expression) -> sprintf "const %s = %s" name (transpile expression)
    | Prim(operation, expression1, expression2) ->
        let value1 = transpile expression1
        let value2 = transpile expression2
        sprintf "(%s %s %s)" value1 operation value2
    | Function(name, (parameters: string list), expressions) ->
        let parametersTranspiled = List.fold (fun acc el -> (acc + el + ", ")) "" parameters
        let parametersTranspiledCut = (sprintf "%s" (parametersTranspiled.Remove(parametersTranspiled.Length - 2)))

        let rec evalExpressions es =
            match es with
            | [] -> []
            | [ e ] -> [ sprintf "return %s" (transpile e) ]
            | e :: exs -> transpile e :: evalExpressions exs

        let exps = evalExpressions expressions

        let js =
            sprintf "function %s(%s) {\n" name parametersTranspiledCut

        List.fold (fun s e ->
            s + sprintf "%s;\n" e) js exps
    | Tuple(expression1, expression2) ->
        let value1 = transpile expression1
        let value2 = transpile expression2
        sprintf "[%s, %s]" value1 value2
    | ADT(adtName, (constructors: adtconstructor list)) ->
        let setterGenerator count = "this.p" + string (count) + " = p" + string (count) + ";\n"
        let parameterGenerator count = "p" + string (count) + ", "
        let generateVariables types generatorFunc =
            fst (List.fold (fun (acc: string, it: int) el -> (acc + generatorFunc (it), it + 1)) ("", 1) types)

        let printClass ((className, types): adtconstructor): string =
            let classDef =
                sprintf "class %s extends %s {\n" className adtName

            let parameters = generateVariables types parameterGenerator
            let parametersCut = sprintf "%s" (parameters.Remove(parameters.Length - 2))

            let constructorDef =
                sprintf "constructor(%s) {\n" parametersCut

            let setters = generateVariables types setterGenerator

            let constructorBody =
                sprintf "super();\n%s" setters

            sprintf "%s%s%s}\n}\n" classDef constructorDef constructorBody

        let rec evalConstructors cs: string list =
            match cs with
            | [] -> []
            | x :: xs -> printClass x :: evalConstructors xs

        let adtClass =
            sprintf "class %s {}\n" adtName

        let adt =
            List.fold (fun s e ->
                s + sprintf "%s\n" e) adtClass (evalConstructors constructors)

        adt
