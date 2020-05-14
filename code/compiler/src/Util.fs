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
    let tryMatch = tryMatch lookupValue

    match (actual, pattern) with
    | (_, Constant (CharValue '_')) -> Some []
    | (a, Constant v) when a = v -> Some []
    | (a, Variable x) ->
        match lookupValue x with
        | Some (ADTValue (a, b, c)) -> tryMatch actual (Constant(ADTValue(a, b, c)))
        | _ -> Some [ (x, a) ]
    | (ADTValue (name, _, values), Apply (Variable (callName), exprs)) when name = callName ->
        forAll tryMatch values exprs
    | (TupleValue (v1, v2), Tuple (p1, p2)) ->
        match (tryMatch v1 p1, tryMatch v2 p2) with
        | (Some (v1), Some (v2)) -> Some(v1 @ v2)
        | _ -> None
    | (ListValue (valList), List (exprList)) when valList.Length = exprList.Length -> forAll tryMatch valList exprList
    | (ListValue (h :: t), ConcatC (h', t')) -> forAll tryMatch [ h; (ListValue t) ] [ h'; t' ]
    | _, _ -> None
