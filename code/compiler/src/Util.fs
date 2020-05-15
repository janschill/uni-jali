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
    | (ADTValue (name, _, values), Constant(ADTValue (name', _, values'))) when name = name' ->
        forAll tryMatch values (List.map Constant values')
    | (ADTValue (name, _, values), Apply (Variable (callName), exprs)) when name = callName ->
        forAll tryMatch values exprs
    | (TupleValue (v1, v2), Tuple (p1, p2)) ->
        match (tryMatch v1 p1, tryMatch v2 p2) with
        | (Some (v1), Some (v2)) -> Some(v1 @ v2)
        | _ -> None
    | (ListValue (valList), List (exprList)) when valList.Length = exprList.Length -> forAll tryMatch valList exprList
    | (ListValue (h :: t), ConcatC (h', t')) -> forAll tryMatch [ h; (ListValue t) ] [ h'; t' ]
    | _, _ -> None




type Bindings = list<string * Expr>

type ReducedMatch =
    | NoMatch
    | DynamicMatch of Bindings
    | StaticMatch of Bindings

let rec collect = // If actual is dynamic, then this just collects all variables as bindings, in order to reduce the body
    function
    | Variable x -> [ (x, Variable x) ]
    | Tuple (e1, e2) -> collect e1 @ collect e2
    | List exprs -> exprs |> List.collect collect
    | ConcatC (h, t) -> collect h @ collect t
    | Apply (name, values) -> values |> List.collect collect
    | _ -> []

(* tryMatch returns
        None            if there can never be a match
        Some bindings   *)

let rec match1 (actual: Expr, case: Expr) =
    printfn "match1: %A ? %A" actual case
    match (actual, case) with
    | Constant v1, Constant v2 when v1 = v2 -> StaticMatch []
    | _, Constant (CharValue '_') -> StaticMatch []
    | _, Variable x -> StaticMatch [ (x, actual) ]
    | Variable a, _ -> DynamicMatch <| collect case
    | Tuple (e1, e2), Tuple (p1, p2) -> matchMany [ e1; e2 ] [ p1; p2 ]
    | List es, List pats when es.Length = pats.Length -> matchMany es pats
    | List (h :: t), ConcatC (h', t') -> matchMany [ h; (List t) ] [ h'; t' ]
    | Apply (name, exprs), Apply (pname, pats) when List.length exprs = List.length pats ->
        matchMany (name :: exprs) (pname :: pats)
    | Apply (name, exprs), Constant (ADTValue (x, _, vals)) when List.length exprs = List.length vals ->
        matchMany (name :: exprs) (Variable x :: List.map Constant vals)
    | Constant (ADTValue (name, _, [])), Constant (ADTValue (x, _, [])) when x = name -> StaticMatch []
    | Constant (ADTValue (name, _, values)), Apply (Variable name', exprs) ->
        if name = name'
        then matchMany (List.map Constant values) exprs
        else NoMatch
    | _, _ ->
        printfn "No match: %A ~ %A" actual case
        NoMatch


and matchMany exprs pats =
    let matches = List.map match1 <| List.zip exprs pats
    if List.exists ((=) NoMatch) matches then
        NoMatch
    else
        let bindings = matches |> List.collect (function
            | DynamicMatch bindings -> bindings
            | StaticMatch bindings -> bindings
            | NoMatch -> failwithf "Impossible: NoMatch")

        if List.exists (function DynamicMatch _ -> true | _ -> false) matches then
            DynamicMatch bindings
        else
            StaticMatch bindings