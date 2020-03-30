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

let rec tryLookup env x =
    match env with
    | [] -> None
    | (y, v) :: r ->
        if x = y then Some(v) else tryLookup r x

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
    | ADT(adtName, (constructors: (string * Type list) list), expression) ->
        let newADT env adtName constructor = ADTClosure(constructor, adtName, env)

        let finalEnv =
            List.fold (fun newEnv constructorDeclaration ->
                let constructorName = fst constructorDeclaration
                let constructor = ADTClosure(constructorDeclaration, adtName, env)
                (constructorName, constructor) :: newEnv) env constructors
        eval expression finalEnv
    | Apply(fname, farguments) ->
        let fclosure = lookup env fname
        match fclosure with
        | Closure(cname, cparameters, cexpression, declarationEnv) ->
            let newEnv =
                List.fold2 (fun dEnv parameterName argument -> (parameterName, eval argument env) :: dEnv)
                    ((cname, fclosure) :: declarationEnv) cparameters farguments // should evaluated args also be added to env, since later args are evaluated with this env? Also, should we test for duplicate names?
            eval cexpression newEnv
        | ADTClosure((constructor: string * Type list), adtName, declarationEnv) ->
            let values = List.map (fun arg -> eval arg env) farguments
            ADTValue(fst constructor, adtName, values) // we chould check whether the arguments have the same length and types as the type list ??
        | _ -> failwith <| sprintf "Evaluator failed on apply: %s is not a function" fname
    | Pattern(matchExpression, (patternList)) ->
        let rec matchSingle (actual: Value) (pattern: Expr) =
            match (actual, pattern) with
            | (_, Constant(CharValue '_')) -> Some []
            | (a, Constant v) when a = v -> Some []
            | (a, Variable x) ->
                let value = tryLookup env x
                if value.IsNone
                then Some [ (x, a) ]
                else matchSingle actual (Constant(Option.get (value)))
            | (ADTValue(constructorName, superName, values), Apply(callName, exprs)) when constructorName = callName ->
                let evaluatedArguments = List.map2 matchSingle values exprs
                if List.forall Option.isSome evaluatedArguments
                then Some(List.collect Option.get evaluatedArguments)
                else None
            | (TupleValue(v1, v2), Tuple(p1, p2)) ->
                match (matchSingle v1 p1, matchSingle v2 p2) with
                | (Some(v1), Some(v2)) -> Some(v1 @ v2)
                | _ -> None
            | _, _ -> None

        let rec find x =
            function
            | (case, expr) :: ps ->
                match matchSingle x case with
                | None -> find x ps
                | Some(bindings) -> Some(expr, bindings)
            | [] -> None

        let body =
            let x = eval matchExpression env
            find x patternList

        match body with
        | Some(expr, bindings) -> env @ bindings |> eval expr
        | None -> failwith "Pattern match incomplete"
    | _ -> failwith "No match found"
