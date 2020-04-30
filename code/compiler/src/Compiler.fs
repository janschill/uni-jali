module Compiler

open AbstractSyntax
open Interpreter

exception ReduceError of Expr * string with
    override this.Message =
        sprintf "Reduce error at expression %O \n%s" this.Data0 this.Data1

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

let rec reduce2 (e: Expr) (context: bool) (store: Expr Env): Expr =
    match e with
    | Constant c -> Constant c
    | Variable v -> lookup store v
    | ConcatC(h, t) ->
        let head = reduce2 h context store
        let tail = reduce2 t context store
        match (head, tail) with
        | (Constant v, Constant(ListValue(vs))) -> Constant(ListValue(v :: vs))
        | (_, List _) -> ConcatC(head, tail)
        | _ -> failwith "Reducer failed on concatenation: tail must be a list"
    | Tuple(expr1, expr2) ->
        let rexpr1 = reduce2 expr1 context store
        let rexpr2 = reduce2 expr2 context store
        match (rexpr1, rexpr2) with
        | (Constant v1, Constant v2) -> Constant(TupleValue(v1, v2))
        | _ -> Tuple(rexpr1, rexpr2)
    | List(list) ->
        let reducedItems = List.map (fun e -> reduce2 e context store) list
        if (allStatic reducedItems)
        then Constant(ListValue(getValues reducedItems))
        else List(reducedItems)
    | Prim(operation, expression1, expression2) ->
        let rexpr1 = reduce2 expression1 context store
        let rexpr2 = reduce2 expression2 context store
        if allStatic [ rexpr1; rexpr2 ]
        then Constant(eval (Prim(operation, rexpr1, rexpr2)) [])
        else Prim(operation, rexpr1, rexpr2)
    | Let(name, expression1, expression2) ->
        let e = reduce2 expression1 context store
        (name, e) :: store |> reduce2 expression2 context
    | If(cond, thenExpr, elseExpr) ->
        let rcond = reduce2 (cond) context store
        match rcond with
        | Constant(BooleanValue(b)) ->
            if b then reduce2 (thenExpr) context store else reduce2 (elseExpr) context store
        | _ -> If(rcond, reduce2 (thenExpr) context store, reduce2 (elseExpr) context store)
    // let thenExpr = reduce2 (thenExpr) store
    // let elseExpr = reduce2 (elseExpr) store
    // if (allStatic [ rcond; thenExpr; elseExpr ]) then // ifs are static only if all e1, e2, e3 are static ?
    //     match rcond with
    //     | Constant(BooleanValue(b)) ->
    //         if b then thenExpr else elseExpr
    //     | _ -> failwith "Condition not a boolean"
    // else
    //     If(rcond, thenExpr, elseExpr)
    | Function(name, parameters, expression, expression2) ->
        ((name, Constant(Closure(name, parameters, expression, []))) :: store) |> reduce2 expression2 context
    | ADT(adtName, (constructors: (string * Type list) list), expression) ->
        let eval constrDecl =
            match constrDecl with
            | (name, []) -> (name, Constant(ADTValue(name, adtName, [])))
            | (name, argTypes) -> (name, Constant(ADTClosure((name, argTypes), adtName, [])))

        let storeWithConstructors = List.fold (fun s c -> eval c :: s) store constructors

        reduce2 expression context storeWithConstructors
    | Apply(f, farguments) ->
        let func = reduce2 f context store
        match func with
        | Constant(Closure(name, parameters, rbody, [])) ->
            if (parameters.Length = farguments.Length) then
                if (context) then
                    let store = ((name, func) :: store)
                    let reducedArgs = List.map (fun a -> reduce2 a context store) farguments
                    let storeArguments = reducedArgs |> List.zip parameters
                    let bodyStore = List.append storeArguments store
                    reduce2 rbody context bodyStore
                else
                    Apply(f, farguments)
            else
                raise <| ReduceError(e, "Partial application not implemented")
        | Constant(ADTClosure((name, argTypes), adtName, [])) ->
            let reducedArgs = List.map2 (fun argType arg -> reduce2 arg context store) argTypes farguments
            if allStatic reducedArgs
            then Constant(ADTValue(name, adtName, getValues reducedArgs))
            else Apply(f, reducedArgs) // TODO: Can I eval here, even though i can't give eval an environment?
        | Constant(c) -> Constant(c)
        | _ -> raise <| ReduceError(e, sprintf "Reduce failed on apply: %O is not a function" func)
    | Pattern(matchExpression, (patternList)) ->
        let lookupVal x =
            match tryLookup store x with
            | Some(Constant(v)) -> Some(v)
            | _ -> None

        let makeStatic = List.map (fun (name, v) -> (name, Constant v))
        let matchPattern x (case, expr) = matchSingleVal lookupVal x case |> Option.map (fun bs -> (case, expr, bs))
        let rActual = reduce2 matchExpression context store
        match rActual with
        | Constant v ->
            match List.tryPick (matchPattern v) patternList with
            | Some(case, expr, bindings) -> (makeStatic bindings) @ store |> reduce2 expr context
            | None -> raise <| ReduceError(e, "No pattern matching")
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


    | _ -> raise <| ReduceError(e, "No match found")


let reduce (e: Expr) (store: Expr Env): Expr = reduce2 e true store
