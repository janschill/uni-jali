module Compiler

open AbstractSyntax
open Interpreter

type Ds =
    | Dynamic of Expr
    | Static of Value

let rec lookup (e: string) (staticStore: Ds Env): Ds =
    match staticStore with
    | [] -> failwithf "%s not found" e
    | (key, value) :: rest ->
        if e = key then value else lookup e rest

let rec tryLookup (e: string) (staticStore: Ds Env): Option<Ds> =
    match staticStore with
    | [] -> None
    | (key, value) :: rest ->
        if e = key then Some(value) else tryLookup e rest

let expToDs e =
    match e with
    | Constant(v) -> Static(v)
    | expr -> Dynamic(expr)

let DsToExpr e =
    match e with
    | Static(v) -> Constant(v)
    | Dynamic(expr) -> expr

let isConstant e =
    match e with
    | Constant _ -> true
    | _ -> false

let getValue e =
    match e with
    | Constant v -> v
    | _ -> failwith "not a constant"

let getValues = List.map getValue

let rec reduce (e: Expr) (store: Ds Env): Expr =
    match e with
    | Constant c -> Constant c
    | Variable v -> lookup v store |> DsToExpr
    | Tuple(expr1, expr2) ->
        let rexpr1 = reduce expr1 store
        let rexpr2 = reduce expr2 store
        match (rexpr1, rexpr2) with
        | (Constant v1, Constant v2) -> Constant(TupleValue(v1, v2))
        | _ -> Tuple(rexpr1, rexpr2)
    | List(list) ->
        let reducedItems = List.map (fun e -> reduce e store) list
        if (List.forall isConstant reducedItems)
        then Constant(ListValue(getValues reducedItems))
        else List(reducedItems)
    | Prim(operation, expression1, expression2) ->
        let rexpr1 = reduce expression1 store
        let rexpr2 = reduce expression2 store
        let rprim = Prim(operation, rexpr1, rexpr2)
        match (rexpr1, rexpr2) with
        | (Constant(IntegerValue(v1)), Constant(IntegerValue(v2))) -> Constant(eval rprim [])
        | _ -> rprim
    | Let(name, expression1, expression2) ->
        let e = reduce expression1 store
        (name, (expToDs e)) :: store |> reduce expression2
    | If(cond, thenExpr, elseExpr) ->
        let rcond = reduce (cond) store
        match rcond with
        | Constant(BooleanValue(b)) ->
            if b then reduce thenExpr store else reduce elseExpr store
        | _ -> If(rcond, reduce thenExpr store, reduce elseExpr store)
    | Function(name, parameters, expression, expression2) ->
        let storeWithParams = List.fold (fun s p -> (p, Dynamic(Variable p)) :: s) store parameters
        let rbody = reduce expression storeWithParams
        match rbody with
        | Constant c -> (name, Static(c)) :: store |> reduce expression2
        | _ -> (name, Static(Closure(name, parameters, rbody, []))) :: store |> reduce expression2 // TODO insert closure instead and fix apply appropriatley
    | ADT(adtName, (constructors: (string * Type list) list), expression) ->
        let eval constrDecl =
            match constrDecl with
            | (name, []) -> (name, Static(ADTValue(name, adtName, [])))
            | (name, argTypes) -> (name, Static(ADTClosure((name, argTypes), adtName, [])))

        let storeWithConstructors = List.fold (fun s c -> eval c :: s) store constructors

        reduce expression storeWithConstructors
    | Apply(fname, farguments) ->
        let func = lookup fname store
        match func with
        | Static(ADTClosure((name, argTypes), adtName, [])) ->
            let reducedArgs = List.map2 (fun argType arg -> reduce arg store) argTypes farguments
            // if List.forall isConstant reducedArgs
            // then
            Constant(ADTValue(name, adtName, getValues reducedArgs)) // TODO test if this is correct, without the if-else
        // else Apply(fname, reducedArgs) // TODO: Can I eval here, even though i can't give eval an environment?

        | Static(Closure(name, parameters, rbody, [])) ->
            if (parameters.Length = farguments.Length) then
                let reduceAndAddArg s name arg = (name, reduce arg store |> expToDs) :: s
                let bodyStore = List.fold2 (reduceAndAddArg) store parameters farguments
                reduce rbody bodyStore
            else
                failwith "Partial application not implemented"
        | Static(c) -> Constant(c)
        // | Dynamic(Function(name, parameters, expression, expression2)) ->
        //     // this will never be variables, because they have been replaced by their expressions -> is this really correct?
        //     let reducedArgs = List.map (fun arg -> reduce arg store) farguments

        //     let bodyStore = List.fold2 (fun s name ra -> (name, (expToDs ra)) :: s) store parameters reducedArgs

        //     reduce expression bodyStore

        | _ -> failwith <| sprintf "Reduce failed on apply: %O is not a function" func
    | Pattern(matchExpression, (patternList)) ->
        let collect ms =
            if List.forall Option.isSome ms then Some(List.collect Option.get ms) else None

        let rec matchSingleVal (actual: Value) (pattern: Expr) =
            match (actual, pattern) with
            | (_, Constant(CharValue '_')) -> Some []
            | (a, Constant v) when a = v -> Some []
            | (a, Variable x) ->
                match tryLookup x store with
                | None -> Some [ (x, a) ]
                | Some(v) -> matchSingleVal actual (DsToExpr v)
            | (ADTValue(name, _, values), Apply(callName, exprs)) when name = callName -> matchAllVals values exprs
            | (TupleValue(v1, v2), Tuple(p1, p2)) ->
                match (matchSingleVal v1 p1, matchSingleVal v2 p2) with
                | (Some(v1), Some(v2)) -> Some(v1 @ v2)
                | _ -> None
            | (ListValue(valList), List(exprList)) -> matchAllVals valList exprList
            | _, _ -> None

        and matchAllVals a b = List.map2 matchSingleVal a b |> collect
        and matchAllExpr a b = List.map2 matchSingleExpr a b |> collect

        and matchSingleExpr (actual: Expr) (case: Expr) =
            // printfn "%O <-->\n %O\n" actual case
            match (actual, case) with
            | (_, Constant(CharValue '_')) -> Some []
            | (Constant av, Constant pv) when av = pv -> Some []
            | (Constant c, expr) ->
                let vs = matchSingleVal c expr
                Option.map (List.map (fun (name, v) -> (name, Constant(v)))) vs
            //     Interpreter.matchSingleExpr [] c expr |> Option.map (List.map (fun (name, v) -> (name, Constant(v))))
            | (e, Variable x) ->
                match tryLookup x store with
                | Some(v) -> matchSingleExpr e <| DsToExpr v // TODO test if this is correct
                | None -> Some [ (x, e) ]
            | (Variable x, _) -> Some []
            | (Apply(name, values), Apply(pname, patternValues)) when name = pname ->
                matchAllExpr values patternValues
            | (Tuple(e1, e2), Tuple(p1, p2)) ->
                match (matchSingleExpr e1 p1, matchSingleExpr e2 p2) with
                | (Some(v1), Some(v2)) -> Some(v1 @ v2)
                | _ -> None
            | (List(exprList1), List(exprList2)) -> matchAllExpr exprList1 exprList2
            | _, _ -> None

        let checkPattern actual (case, expr) = matchSingleExpr actual case |> Option.map (fun bs -> (case, expr, bs))

        let reducePattern (case, expr, bindings): Expr * Expr =
            let bToDs (name, expr) = (name, expToDs expr)

            let newStore = List.foldBack (fun b s -> bToDs b :: s) bindings store
            (case, reduce expr newStore)

        let rActual = reduce matchExpression store
        match rActual with
        | Constant _ ->
            let rPattern = List.tryPick (checkPattern rActual) patternList |> Option.map reducePattern
            match rPattern with
            | Some(case, expr) -> expr
            | _ -> failwith "Pattern match incomplete"
        | _ ->
            let bs = List.choose (checkPattern rActual) patternList
            let rPatterns = List.map (reducePattern) bs
            match rPatterns with
            | [] -> failwith "Pattern match incomplete"
            | [ (case, expr) ] -> expr
            | many -> Pattern(rActual, many)

    | _ -> failwith "No match found"
