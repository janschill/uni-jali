module Compiler

open AbstractSyntax
open Interpreter

let isConstant e =
    match e with
    | Constant _ -> true
    | _ -> false

let allStatic l = List.isEmpty l || List.forall isConstant l

let getValue e =
    match e with
    | Constant v -> v
    | _ -> failwith "not a constant"

let getValues = List.map getValue

let rec reduce (e: Expr) (store: Expr Env): Expr =
    match e with
    | Constant c -> Constant c
    | Variable v -> lookup store v
    | Tuple(expr1, expr2) ->
        let rexpr1 = reduce expr1 store
        let rexpr2 = reduce expr2 store
        match (rexpr1, rexpr2) with
        | (Constant v1, Constant v2) -> Constant(TupleValue(v1, v2))
        | _ -> Tuple(rexpr1, rexpr2)
    | List(list) ->
        let reducedItems = List.map (fun e -> reduce e store) list
        if (allStatic reducedItems)
        then Constant(ListValue(getValues reducedItems))
        else List(reducedItems)
    | Prim(operation, expression1, expression2) ->
        let rexpr1 = reduce expression1 store
        let rexpr2 = reduce expression2 store
        if allStatic [ rexpr1; rexpr2 ]
        then Constant(eval (Prim(operation, rexpr1, rexpr2)) [])
        else Prim(operation, rexpr1, rexpr2)
    | Let(name, expression1, expression2) ->
        let e = reduce expression1 store
        (name, e) :: store |> reduce expression2
    | If(cond, thenExpr, elseExpr) ->
        let rcond = reduce (cond) store
        match rcond with
        | Constant(BooleanValue(b)) ->
            if b then reduce (thenExpr) store else reduce (elseExpr) store
        | _ -> If(rcond, reduce (thenExpr) store, reduce (elseExpr) store)
    // let thenExpr = reduce (thenExpr) store
    // let elseExpr = reduce (elseExpr) store
    // if (allStatic [ rcond; thenExpr; elseExpr ]) then // ifs are static only if all e1, e2, e3 are static ?
    //     match rcond with
    //     | Constant(BooleanValue(b)) ->
    //         if b then thenExpr else elseExpr
    //     | _ -> failwith "Condition not a boolean"
    // else
    //     If(rcond, thenExpr, elseExpr)
    | Function(name, parameters, expression, expression2) ->
        ((name, Constant(Closure(name, parameters, expression, []))) :: store) |> reduce expression2
    | ADT(adtName, (constructors: (string * Type list) list), expression) ->
        let eval constrDecl =
            match constrDecl with
            | (name, []) -> (name, Constant(ADTValue(name, adtName, [])))
            | (name, argTypes) -> (name, Constant(ADTClosure((name, argTypes), adtName, [])))

        let storeWithConstructors = List.fold (fun s c -> eval c :: s) store constructors

        reduce expression storeWithConstructors
    | Apply(fname, farguments) ->
        let func = lookup store fname
        match func with
        | Constant(Closure(name, parameters, rbody, [])) ->
            if (parameters.Length = farguments.Length) then
                // if ) then
                let store = ((name, func) :: store)
                let reducedArgs = List.map (fun a -> reduce a store) farguments
                let storeArguments = reducedArgs |> List.zip parameters
                let bodyStore = List.append storeArguments store
                reduce rbody bodyStore
            // else
            // Apply(fname, farguments)
            else
                failwith "Partial application not implemented"
        | Constant(ADTClosure((name, argTypes), adtName, [])) ->
            let reducedArgs = List.map2 (fun argType arg -> reduce arg store) argTypes farguments
            if allStatic reducedArgs
            then Constant(ADTValue(name, adtName, getValues reducedArgs))
            else Apply(fname, reducedArgs) // TODO: Can I eval here, even though i can't give eval an environment?
        | Constant(c) -> Constant(c)
        | _ -> failwith <| sprintf "Reduce failed on apply: %O is not a function" func
    | Pattern(matchExpression, (patternList)) ->
        let rec matchSingleVal (actual: Value) (pattern: Expr) =
            // printfn "actual: %O\npattern: %O" (printValue actual) (printExpr pattern)
            match (actual, pattern) with
            | (_, Constant(CharValue '_')) -> Some []
            | (a, Constant v) when a = v -> Some []
            | (a, Variable x) ->
                match tryLookup store x with
                | Some(Constant(ADTValue(a, b, c))) -> matchSingleVal actual (Constant(ADTValue(a, b, c)))
                | _ -> Some [ (x, a) ]
            | (ADTValue(name, _, values), Apply(callName, exprs)) when name = callName -> matchAllVals values exprs
            | (TupleValue(v1, v2), Tuple(p1, p2)) ->
                match (matchSingleVal v1 p1, matchSingleVal v2 p2) with
                | (Some(v1), Some(v2)) -> Some(v1 @ v2)
                | _ -> None
            | (ListValue([]), List([])) -> Some []
            | (ListValue(valList), List(exprList)) when valList.Length = exprList.Length ->
                matchAllVals valList exprList
            | (ListValue(valList), ConcatC(Variable h, Variable t)) when valList.Length > 0 ->
                Some
                    [ (h, List.head valList)
                      (t, ListValue(List.tail valList)) ]
            | _, _ -> None

        and matchAllVals values exprs =
            let matchings = List.map2 (matchSingleVal) values exprs
            if List.forall Option.isSome matchings
            then Some(List.collect Option.get matchings)
            else None

        let makeStatic = List.map (fun (name, v) -> (name, Constant v))
        let matchPattern x (case, expr) = matchSingleVal x case |> Option.map (fun bs -> (case, expr, bs))
        let rActual = reduce matchExpression store
        match rActual with
        | Constant v ->
            match List.tryPick (matchPattern v) patternList with
            | Some(case, expr, bindings) -> (makeStatic bindings) @ store |> reduce expr
            | None -> failwith "No pattern matching"
        | _ -> Pattern(matchExpression, patternList)
    // and matchSingleExpr (actual: Expr) (case: Expr) =
    //     match (actual, case) with
    //     | (_, Constant(CharValue '_')) -> Some []
    //     | (Constant av, Constant pv) when av = pv -> Some []
    //     | (Constant c, expr) ->
    //         let vs = matchSingleVal c expr
    //         Option.map (List.map (fun (name, v) -> (name, Constant(v)))) vs
    //     //     Interpreter.matchSingleExpr [] c expr |> Option.map (List.map (fun (name, v) -> (name, Constant(v))))
    //     | (e, Variable x) ->
    //         match tryLookup x store with
    //         | Some(v) -> matchSingleExpr e <| DsToExpr v // TODO test if this is correct
    //         | None -> Some [ (x, e) ]
    //     | (Apply(name, values), Apply(pname, patternValues)) when name = pname ->
    //         matchAllExpr values patternValues
    //     | (Tuple(e1, e2), Tuple(p1, p2)) ->
    //         match (matchSingleExpr e1 p1, matchSingleExpr e2 p2) with
    //         | (Some(v1), Some(v2)) -> Some(v1 @ v2)
    //         | _ -> None
    //     | (List([]), List([])) -> Some []
    //     | (List(exprList1), List(exprList2)) when exprList1.Length = exprList2.Length ->
    //         matchAllExpr exprList1 exprList2
    //     | (List(h :: t), ConcatC(Variable h', Variable t')) ->
    //         Some
    //             [ (h', h)
    //               (t', List(t)) ]

    //     | _, _ -> None


    | _ -> failwith "No match found"


// let reduce (e: Expr) (store: Expr Env): Expr = reduce2 e true store
