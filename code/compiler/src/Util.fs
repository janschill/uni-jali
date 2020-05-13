module Util

open AbstractSyntax

let rec lookup (env: 'v Env) x =
    match env with
    | [] -> failwith (x + " not found")
    | (y, v) :: r -> if x = y then v else lookup r x

let rec tryLookup (env: 'v Env) x =
    match env with
    | [] -> None
    | (y, v) :: r -> if x = y then Some(v) else tryLookup r x

let forAll matcher values exprs =
    let matchings = List.map2 (matcher) values exprs
    if List.forall Option.isSome matchings then Some(List.collect Option.get matchings) else None

let rec tryMatch (lookupValue: string -> option<Value>) (actual: Value) (pattern: Expr) =
    // printf "actual: %O, pattern: %O" actual pattern
    let matchSingle = tryMatch lookupValue

    match (actual, pattern) with
    // printfn "actual: %O\npattern: %O" (printValue actual) (printExpr pattern)
    | (_, Constant (CharValue '_')) -> Some []
    | (a, Constant v) when a = v -> Some []
    | (a, Variable x) ->
        match lookupValue x with
        | Some (ADTValue (a, b, c)) -> matchSingle actual (Constant(ADTValue(a, b, c)))
        | _ -> Some [ (x, a) ]
    | (ADTValue (name, _, values), Apply (Variable (callName), exprs)) when name = callName ->
        forAll matchSingle values exprs
    | (TupleValue (v1, v2), Tuple (p1, p2)) ->
        match (matchSingle v1 p1, matchSingle v2 p2) with
        | (Some (v1), Some (v2)) -> Some(v1 @ v2)
        | _ -> None
    | (ListValue (valList), List (exprList)) when valList.Length = exprList.Length ->
        forAll matchSingle valList exprList
    | (ListValue (h :: t), ConcatC (h', t')) -> forAll matchSingle [ h; (ListValue t) ] [ h'; t' ]
    | _, _ -> None
