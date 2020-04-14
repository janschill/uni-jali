module Helpers

open AbstractSyntax

let rec matchSingleExpr (actual: Expr) (pattern: Expr) =
    match (actual, pattern) with
    | (_, Constant(CharValue '_')) -> Some []
    | (Constant av, Constant pv) when av = pv -> Some []
    | (e, Variable bindName) -> Some [ (bindName, e) ]
    | (Variable x, _) -> Some []
    | (Apply(aConstructorName, aValues), Apply(pConstructorName, pValues)) when aConstructorName = pConstructorName ->
        let evaluatedArguments = List.map2 matchSingleExpr aValues pValues
        if List.forall Option.isSome evaluatedArguments
        then Some(List.collect Option.get evaluatedArguments)
        else None
    | (Tuple(e1, e2), Tuple(p1, p2)) ->
        match (matchSingleExpr e1 p1, matchSingleExpr e2 p2) with
        | (Some(v1), Some(v2)) -> Some(v1 @ v2)
        | _ -> None
    | _, _ -> None

let rec matchSingleVal (actual: Value) (pattern: Expr) =
    match (actual, pattern) with
    | (_, Constant(CharValue '_')) -> Some []
    | (a, Constant v) when a = v -> Some []
    | (a, Variable x) -> Some [ (x, a) ]
    | (ADTValue(constructorName, superName, values), Apply(callName, exprs)) when constructorName = callName ->
        let evaluatedArguments = List.map2 matchSingleVal values exprs
        if List.forall Option.isSome evaluatedArguments
        then Some(List.collect Option.get evaluatedArguments)
        else None
    | (TupleValue(v1, v2), Tuple(p1, p2)) ->
        match (matchSingleVal v1 p1, matchSingleVal v2 p2) with
        | (Some(v1), Some(v2)) -> Some(v1 @ v2)
        | _ -> None
    | _, _ -> None

let rec findPattern (x: Value) patternList =
    match patternList with
    | (case, expr) :: ps ->
        match matchSingleVal x case with
        | None -> findPattern x ps
        | Some(bindings) -> Some(expr, bindings)
    | [] -> None
