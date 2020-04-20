module Transpiler

open AbstractSyntax

let rec foldWithCommas (values) = List.fold (fun acc el -> (acc + el + ", ")) "" values

let rec transpile (e: Expr): string =
    match e with
    | Constant c ->
        let rec transpileConstant ct =
            match ct with
            | IntegerValue i -> string i
            | BooleanValue b -> string b
            | CharValue cv -> string cv
            | StringValue sv -> sv
            | TupleValue(value1, value2) ->
                sprintf "(%s, %s)" (transpileConstant value1) (transpileConstant value2)
            | ListValue(values) ->
                sprintf "[%s]" (List.fold (fun acc el -> (acc + (transpileConstant el) + ", ")) "" values)
            | ADTValue(name, superType, values) ->
                sprintf "new %s(%s);" name
                    ((List.fold (fun acc el -> (acc + (transpileConstant el) + ", ")) "" values))
            | Closure(name, parameters, body, env) -> failwith "Closure not yet implemented"
            | ADTClosure((cname, ctypes), superType, env) -> failwith "ADTClosure not yet implemented"
        transpileConstant c
    | Variable v -> string v
    | Tuple(e1, e2) -> sprintf "(%s, %s)" (transpile e1) (transpile e2)
    | List(list) -> sprintf "[%s]" (List.fold (fun acc el -> (acc + (transpile el) + ", ")) "" list)
    | And(e1, e2) -> sprintf "%s && %s" (transpile e1) (transpile e2)
    | Or(e1, e2) -> sprintf "%s || %s" (transpile e1) (transpile e2)
    | Prim(operation, expression1, expression2) ->
        let value1 = transpile expression1
        let value2 = transpile expression2
        sprintf "(%s %s %s)" value1 operation value2
    | Let(name, expression1, expression2) ->
        sprintf "const %s = %s;\n%s" name (transpile expression1) (transpile expression2)
    | If(expression1, expression2, expression3) ->
        let value1 = transpile expression1
        let value2 = transpile expression2
        let value3 = transpile expression3

        let row1 =
            sprintf "if (%s) {\n" value1
        let row2 =
            sprintf "%s\n" value2

        let row3 = "} else {\n"

        let row4 =
            sprintf "%s\n" value3

        let row5 = "}\n"
        sprintf "%s%s%s%s%s" row1 row2 row3 row4 row5
    | Function(name, (parameters: string list), expression, expressionAfter) ->
        let parametersTranspiled = foldWithCommas parameters
        let parametersTranspiledCut = (sprintf "%s" (parametersTranspiled.Remove(parametersTranspiled.Length - 2)))

        // let rec evalExpressions es =
        //     match es with
        //     | [] -> []
        //     | [ e ] -> [ sprintf "return %s" (transpile e) ]
        //     | e :: exs -> transpile e :: evalExpressions exs

        sprintf "function %s(%s) {\n%s}\n\n%s" name parametersTranspiledCut (transpile expression)
            (transpile expressionAfter)
    | ADT(adtName, (constructors: ADTConstructor list), a) ->
        let setterGenerator count = "this.p" + string (count) + " = p" + string (count) + ";\n"
        let parameterGenerator count = "p" + string (count) + ", "
        let generateVariables types generatorFunc =
            fst (List.fold (fun (acc: string, it: int) el -> (acc + generatorFunc (it), it + 1)) ("", 1) types)

        let printClass ((className, types): ADTConstructor): string =
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
    | Apply(fname, farguments) -> failwith "Apply matching not implemented"
    | Pattern(expressions, tuples) -> failwith "Pattern matching not implemented"
    | _ -> failwith "unknown transpile pattern"
