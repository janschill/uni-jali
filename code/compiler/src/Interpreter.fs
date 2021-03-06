module Interpreter

open AbstractSyntax
open Util

let rec eval (e: Expr) (env: Value Env): Value =
    match e with
    | Constant c -> c
    | Variable v -> lookup env v
    | ConcatC (h, t) ->
        let head = eval h env
        match eval t env with
        | ListValue (tail) -> ListValue(head :: tail)
        | _ -> failwith "Evaluator failed on concatenation: tail must be a list"
    | Tuple (expr1, expr2) -> TupleValue(eval expr1 env, eval expr2 env)
    | List (list) -> ListValue(List.map (fun e -> eval e env) list)
    | And (expression1, expression2) ->
        match eval expression1 env with
        | BooleanValue false -> BooleanValue false
        | BooleanValue true -> eval expression2 env
        | _ -> failwith "Can only use boolean values on logical AND"
    | Or (expression1, expression2) ->
        match eval expression1 env with
        | BooleanValue true -> BooleanValue true
        | BooleanValue false -> eval expression2 env
        | _ -> failwith "Can only use boolean values on logical OR"
    | Negate (expression) ->
        match eval expression env with
        | BooleanValue b -> BooleanValue(not b)
        | _ -> failwithf "Negation is only a valid operation on booleans but not a valid operation on %O" expression
    | Prim (operation, expression1, expression2) ->
        let value1 = eval expression1 env
        let value2 = eval expression2 env
        match (value1, value2) with
        | ((IntegerValue (v1)), IntegerValue (v2)) ->
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
        | ((StringValue (v1)), StringValue (v2)) ->
            match (operation) with
            | ("+") -> StringValue(v1 + v2)
            | ("==") -> BooleanValue(v1 = v2)
            | ("!=") -> BooleanValue(v1 <> v2)
            | _ -> failwithf "%s is not a valid operation on integers" operation
        | ((IntegerValue (v1)), StringValue (v2)) ->
            match (operation) with
            | ("+") -> StringValue(string v1 + v2)
            | _ -> failwithf "%s integer with string" operation
        | ((StringValue (v1), IntegerValue (v2))) ->
            match (operation) with
            | ("+") -> StringValue(v1 + string v2)
            | _ -> failwithf "%s integer with string" operation
        | ((a, b)) -> failwithf "Can only operate on integers or strings 1:%O 2:%O" a b
    | Let (name, expression1, expression2) ->
        let value = eval expression1 env
        let newEnv = (name, value) :: env
        eval expression2 newEnv
    | If (cond, thenExpr, elseExpr) ->
        match eval cond env with
        | BooleanValue true -> eval thenExpr env
        | BooleanValue false -> eval elseExpr env
        | _ -> failwith "Evaluator failed on if-statement: condition must be a boolean value"
    | Function (name, parameters, expression, expression2) ->
        let closure =
            Closure(name, parameters, expression, env)

        let newEnv = (name, closure) :: env
        eval expression2 newEnv
    | ADT (adtName, (constructors: (string * Type list) list), expression) ->
        let addADTConstr env' constructorDecl =
            match constructorDecl with
            | (name, []) -> (name, ADTValue(name, adtName, [])) :: env'
            | (name, _) ->
                (name, ADTClosure(constructorDecl, adtName, env))
                :: env'

        let newEnv =
            List.fold (addADTConstr) env constructors

        eval expression newEnv
    | Apply (f, farguments) ->
        let fclosure = eval f env
        match fclosure with
        | Closure (cname, cparameters, cexpression, declarationEnv) ->
            if (farguments.Length > cparameters.Length)
            then failwith "too many args"
            if (farguments.Length < cparameters.Length) then
                let leftoverParams = List.skip farguments.Length cparameters

                let args =
                    List.map (fun a -> Constant(eval a env)) farguments

                let variables = List.map (Variable) leftoverParams

                let body =
                    Apply(Variable cname, (List.append args variables))

                let argString =
                    List.fold (fun s a -> s + printArg a) "" args

                Closure(cname + argString, leftoverParams, body, env)
            else
                let newEnv =
                    List.fold2 (fun dEnv parameterName argument -> (parameterName, eval argument env) :: dEnv)
                        ((cname, fclosure) :: declarationEnv) cparameters farguments // should we test for duplicate names?

                eval cexpression newEnv
        | ADTClosure ((name, argTypes), adtName, declarationEnv) ->
            let values =
                List.map2 (fun argType arg -> eval arg env) argTypes farguments

            ADTValue(name, adtName, values) // we chould check whether the arguments have the same length and types as the type list ??
        | _ ->
            failwith
            <| sprintf "Evaluator failed on apply: %O is not a function" f
    | Pattern (matchExpression, (patternList)) ->
        let matchPattern x (case, expr) =
            tryMatch (tryLookup env) x case
            |> Option.map (fun bs -> (case, expr, bs))

        let evaluatedMatch = eval matchExpression env
        match List.tryPick (matchPattern evaluatedMatch) patternList with
        | Some (case, expr, bindings) -> env @ bindings |> eval expr
        | None -> failwith "Pattern match incomplete"
    | _ -> failwith "No match found"
