module Interpreter

(*
    This interpreter takes our code and executes it using F#
*)

open AbstractSyntax

let rec lookup env x =
    match env with
    | [] -> failwith (x + " not found")
    | (y, v) :: r ->
        if x = y then v else lookup r x

let rec eval (e: Expr) (env: Value Env): Value =
    match e with
    | Constant c -> c
    | Variable v -> lookup env v
    | Tuple(expr1, expr2) -> TupleValue(eval expr1 env, eval expr2 env)
    | Prim(operation, expression1, expression2) ->
        let value1 = eval expression1 env
        let value2 = eval expression2 env
        match (value1, value2) with
        | ((IntegerValue(v1)), IntegerValue(v2)) ->
            match (operation) with
            | ("+") -> IntegerValue(v1 + v2)
            | ("-") -> IntegerValue(v1 - v2)
            | ("*") -> IntegerValue(v1 * v2)
            | ("/") -> IntegerValue(v1 / v2)
            | ("%") -> IntegerValue(v1 % v2)
            | (">") -> BooleanValue(v1 > v2)
            | (">=") -> BooleanValue(v1 >= v2)
            | ("<") -> BooleanValue(v1 < v2)
            | ("<=") -> BooleanValue(v1 <= v2)
            | ("==") -> BooleanValue(v1 = v2)
            | ("!=") -> BooleanValue(v1 <> v2)
            | _ -> failwithf "%s is not a valid operation on integers" operation
        | _ -> failwith "Sorry, can only operate on integers"
    | Let(name, expression1, expression2) ->
        let value = eval expression1 env
        let newEnv = (name, value) :: env
        eval expression2 newEnv
    | If(cond, thenExpr, elseExpr) ->
        match eval cond env with
        | BooleanValue true -> eval thenExpr env
        | BooleanValue false -> eval elseExpr env
        | _ -> failwith "Evaluator failed on if-statement: condition must be a boolean value"
    | Function(name, parameters, expression, expression2) ->
        let closure = Closure(name, parameters, expression, env)
        let newEnv = (name, closure) :: env
        eval expression2 newEnv
    | Apply(fname, farguments) ->
        let fclosure = lookup env fname
        match fclosure with
        | Closure(cname, cparameters, cexpression, declarationEnv) ->
            let declarationEnv2 = (cname, fclosure) :: declarationEnv
            let newEnv =
                List.fold (fun newEnv (name, arg) -> (name, eval arg env) :: newEnv) declarationEnv2  // should evaluated args also be added to env, since later args are evaluated with this env? Also, should we test for duplicate names?
                    (List.zip cparameters farguments)
            eval cexpression newEnv
        | _ -> failwith <| sprintf "Evaluator failed on apply: %s is not a function" fname
    | Pattern(matchExpression, matchList) ->
        let rec matchSingle (actual: Value) (pattern: Value) =
            match (actual, pattern) with
            | (a, p) when a = p -> true //Some []
            | (_, CharValue '_') -> true //Some []
            // | (Variable y, v) -> true // Some [(y, v)]
            | (TupleValue(v1, v2), TupleValue(p1, p2)) ->
                let eval1 = matchSingle v1 p1
                let eval2 = matchSingle v2 p2
                (eval1 && eval2)
            | _, _ -> false

        let body =
            List.tryFind (fun ((pattern, exp): Expr * Expr) ->
                // let ePattern = eval pattern env
                matchSingle (eval matchExpression env) (eval pattern env)) matchList
        match body with
        | Some(e) -> eval (snd e) env
        | None -> failwith "Pattern match incomplete"
    | ADT(adtName, (constructors: ADTConstructor list), a) -> failwith "not implemented"
    | _ -> failwith "No match found"
