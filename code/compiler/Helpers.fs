module Helpers

open AbstractSyntax

(* Reducing *)

let rec matchSingleExpr (actual: Expr) (pattern: Expr) =
    match (actual, pattern) with
    | (_, Constant(CharValue '_')) -> Some []
    | (Constant av, Constant pv) when av = pv -> Some []
    | (e, Variable bindName) -> Some [ (bindName, e) ]
    | (Variable x, _) -> Some []
    | (Apply(constrName, values), Apply(patternConstrName, patternValues)) when constrName = patternConstrName ->
        matchAllExprs values patternValues
    | (Tuple(e1, e2), Tuple(p1, p2)) ->
        match (matchSingleExpr e1 p1, matchSingleExpr e2 p2) with
        | (Some(v1), Some(v2)) -> Some(v1 @ v2)
        | _ -> None
    | (List(exprList1), List(exprList2)) -> matchAllExprs exprList1 exprList2
    | _, _ -> None

and matchAllExprs exprs1 exprs2 =
    let matchings = List.map2 matchSingleExpr exprs1 exprs2
    if List.forall Option.isSome matchings
    then Some(List.collect Option.get matchings)
    else None

(* Interpreting *)

let rec matchSingleVal (actual: Value) (pattern: Expr) =
    match (actual, pattern) with
    | (_, Constant(CharValue '_')) -> Some []
    | (a, Constant v) when a = v -> Some []
    | (a, Variable x) -> Some [ (x, a) ]
    | (ADTValue(constructorName, superName, values), Apply(callName, exprs)) when constructorName = callName ->
        matchAllVals values exprs
    | (TupleValue(v1, v2), Tuple(p1, p2)) ->
        match (matchSingleVal v1 p1, matchSingleVal v2 p2) with
        | (Some(v1), Some(v2)) -> Some(v1 @ v2)
        | _ -> None
    | (ListValue(valList), List(exprList)) -> matchAllVals valList exprList
    | _, _ -> None

and matchAllVals values exprs =
    let matchings = List.map2 matchSingleVal values exprs
    if List.forall Option.isSome matchings
    then Some(List.collect Option.get matchings)
    else None

let rec findPattern (x: Value) patternList =
    match patternList with
    | (case, expr) :: ps ->
        match matchSingleVal x case with
        | None -> findPattern x ps
        | Some(bindings) -> Some(expr, bindings)
    | [] -> None
