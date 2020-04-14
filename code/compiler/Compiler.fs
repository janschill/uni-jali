module Compiler

open AbstractSyntax
open Interpreter
open Helpers

type Ds =
    | Dynamic of Expr
    | Static of Value

let rec lookup (e: string) (staticStore: Ds Env): Ds =
    match staticStore with
    | [] -> failwithf "%s not found" e
    | (key, value) :: rest ->
        if e = key then value else lookup e rest


let expToDs e =
    match e with
    | Constant(v) -> Static(v)
    | expr -> Dynamic(expr)

let rec reduce (e: Expr) (store: Ds Env): Expr =
    match e with
    | Constant c -> Constant c
    | Variable v ->
        match lookup v store with
        | Dynamic e -> e // TODO: Do we really want to return e? Or do we wanna return Variable v?
        | Static vv -> Constant(vv)
    | Tuple(expr1, expr2) ->
        let rexpr1 = reduce expr1 store
        let rexpr2 = reduce expr2 store
        match (rexpr1, rexpr2) with
        | (Constant v1, Constant v2) -> Constant(TupleValue(v1, v2))
        | _ -> Tuple(rexpr1, rexpr2)
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
        | _ -> (name, Dynamic(Function(name, parameters, rbody, expression2))) :: store |> reduce expression2
    | ADT(adtName, (constructors: (string * Type list) list), expression) ->
        let eval (name, argTypes) = (name, Static(ADTClosure((name, argTypes), adtName, [])))

        let storeWithConstructors = List.fold (fun s c -> eval c :: s) store constructors

        reduce expression storeWithConstructors // TODO: IS THIS CORRECT OR SHOULD WE RETURN AN ADT???
    | Apply(fname, farguments) ->
        let func = lookup fname store
        match func with
        | Static(ADTClosure((name, argTypes), adtName, [])) ->
            let reducedArgs = List.map2 (fun argType arg -> reduce arg store) argTypes farguments

            let isStatic =
                List.forall (function
                    | Constant c -> true
                    | _ -> false) reducedArgs

            if isStatic then
                let values = List.map (fun (Constant(c)) -> c) reducedArgs
                Constant(ADTValue(name, adtName, values))
            else
                Apply(fname, reducedArgs) // TODO: Can I eval here, even though i can't give eval an environment?

        | Static(c) -> Constant(c)
        | Dynamic(Function(name, parameters, expression, expression2)) ->
            // this will never be variables, because they have been replaced by their expressions -> is this really correct?
            let reducedArgs = List.map (fun arg -> reduce arg store) farguments

            let bodyStore = List.fold2 (fun s name ra -> (name, (expToDs ra)) :: s) store parameters reducedArgs

            reduce expression bodyStore

        | _ -> failwith <| sprintf "Reduce failed on apply: %O is not a function" func
    | Pattern(matchExpression, (patternList)) ->

        let reduceExpr (expr, bindings): Expr = List.foldBack (fun b s -> b :: s) bindings store |> reduce expr

        let findAndReduceManyPatterns (x: Expr) patterns =
            let bindingsToDs bindings = List.map (fun (name, expr) -> (name, expToDs expr)) bindings

            let rec findPatterns x patterns =
                match patterns with
                | (case, expr) :: tail ->
                    match Helpers.matchSingleExpr x case with
                    | None -> findPatterns x tail
                    | Some(bindings) -> (case, reduceExpr (expr, (bindingsToDs bindings))) :: findPatterns x tail
                | [] -> []

            findPatterns x patterns

        let evalAndReduce (v: Value) patterns =
            match Helpers.findPattern v patterns with
            | Some(expr, bindings) ->
                let dsBindings = List.map (fun (name, value) -> (name, Static(value))) bindings
                reduceExpr (expr, dsBindings)
            | _ -> failwith "Pattern match incomplete"

        let rActual = reduce matchExpression store
        match rActual with
        | Constant c -> evalAndReduce c patternList
        | _ ->
            match findAndReduceManyPatterns rActual patternList with
            | [] -> failwith "Pattern match incomplete"
            | [ (case, expr) ] -> expr
            | many -> Pattern(rActual, many)

    | _ -> failwith "No match found"
