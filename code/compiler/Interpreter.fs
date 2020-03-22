module Interpreter

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
        | Closure(cname, cparameters, cexpression, cenv) ->
            let paramArgu = List.zip cparameters farguments
            let newEnv = List.fold (fun env (name, arg) -> (name, eval arg env) :: cenv) farguments
            let a = (cname, fclosure) :: newEnv
            eval cexpression a

    | ADT(adtName, (constructors: ADTConstructor list), a) -> failwith "not implemented"
