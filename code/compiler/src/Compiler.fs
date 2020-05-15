module Compiler

open AbstractSyntax
open Interpreter
open Util

exception ReduceError of Expr * string with
    override this.Message = sprintf "Reduce error at expression %O \n%s" this.Data0 this.Data1


let isConstant e =
    match e with
    | Constant _ -> true
    | _ -> false

let allStatic l =
    List.isEmpty l || List.forall isConstant l

let getValue e =
    match e with
    | Constant v -> v
    | _ -> failwith "not a constant"

let getValues = List.map getValue
let mutable indent = 0

let rec reduce2 (e: Expr) (context: bool) (store: Expr Env): Expr =
    (* This will print out the store and the expression it is about to reduce *)
    //List.fold (fun u (n, e) -> printArg e |> printf "(%s,%s) " n |> ignore) () (("Store", StringLiteral(":")) :: store)
    //printfn "\n"
    indent <- indent + 1
    printfn "**(%d) Reducing %A" indent e
    let fail msg = ReduceError(e, msg) |> raise

    let x =
        let fail msg = ReduceError(e, msg) |> raise
        match e with
        | Constant c -> Constant c
        | Variable x -> lookup store x
        | ConcatC (h, t) ->
            let head = reduce2 h context store
            let tail = reduce2 t context store
            match (head, tail) with
            | (v, List (vs)) -> List(v :: vs)
            | _ -> ConcatC(head, tail)
        | Tuple (expr1, expr2) ->
            let rexpr1 = reduce2 expr1 context store
            let rexpr2 = reduce2 expr2 context store
            Tuple(rexpr1, rexpr2)
        | List (list) ->
            let reducedItems =
                List.map (fun e -> reduce2 e context store) list

            List(reducedItems)
        | Prim (op, expr1, expr2) ->
            let rexpr1 = reduce2 expr1 context store
            let rexpr2 = reduce2 expr2 context store
            match op, rexpr1, rexpr2 with
            | _, Constant _, Constant _ -> Constant(eval (Prim(op, rexpr1, rexpr2)) [])
            | "*", _, Constant (IntegerValue 0) -> Constant <| IntegerValue 0
            | "*", _, Constant (IntegerValue 1) -> rexpr1
            | "+", _, Constant (IntegerValue 0) -> rexpr1
            | "-", _, Constant (IntegerValue 0) -> rexpr1
            | "*", Constant (IntegerValue 0), _ -> Constant <| IntegerValue 0
            | "*", Constant (IntegerValue 1), _ -> rexpr2
            | "+", Constant (IntegerValue 0), _ -> rexpr2
            | "-", Constant (IntegerValue 0), _ -> rexpr2
            | "==", Variable x, Prim ("+", Variable x', _) -> Constant <| BooleanValue false
            | "==", Variable x, Prim ("+", _, Variable x') -> Constant <| BooleanValue false
            | "==", Variable x, Prim ("-", Variable x', _) -> Constant <| BooleanValue false
            | "==", Variable x, Prim ("-", _, Variable x') -> Constant <| BooleanValue false
            | _ -> Prim(op, rexpr1, rexpr2)
        | Let (name, expr1, expr2) ->
            let e = reduce2 expr1 context store
            (name, e) :: store |> reduce2 expr2 context
        | If (cond, thenExpr, elseExpr) ->
            let rcond = reduce2 (cond) context store
            match rcond with
            | Constant (BooleanValue (b)) ->
                if b then reduce2 (thenExpr) context store else reduce2 (elseExpr) context store
            | _ -> If(rcond, reduce2 (thenExpr) false store, reduce2 (elseExpr) false store)
        | Function (name, parameters, body, expression2) ->
            let bodyStore =
                (name, Constant <| Closure(name, parameters, body, [])) // TODO I believe i should add (name,Closure) and not (name,Variable) to store
                :: List.map (fun p -> p, Variable p) parameters
                @ store

            let rbody = reduce2 body context bodyStore

            let cl =
                Constant <| Closure(name, parameters, rbody, [])

            ((name, cl) :: store) // adds reduced closure to original store
            |> reduce2 expression2 context
        | ADT (adtName, (constructors: (string * Type list) list), expression) ->
            let eval constrDecl =
                match constrDecl with
                | name, [] -> (name, Constant <| ADTValue(name, adtName, []))
                | name, argTypes -> (name, Constant(ADTClosure((name, argTypes), adtName, [])))

            let storeWithConstructors =
                List.fold (fun s c -> eval c :: s) store constructors

            reduce2 expression context storeWithConstructors
        | Apply (f, args) ->
            let func = reduce2 f context store

            let reduceArgs =
                List.map (fun a -> reduce2 a context store)

            match func with
            | Constant (Closure (name, pars, body, expression2)) ->
                if (pars.Length <> args.Length) then
                    fail "Partial application not implemented"
                else if (context) then
                    let nameArgPairs = reduceArgs args |> List.zip pars
                    nameArgPairs @ store |> reduce2 body context // TODO Should this return an 'Apply' if body doesn't reduce to a constant?
                else
                    let args = reduceArgs args
                    Apply(func, args)

            | Constant (ADTClosure ((name, argTypes), adtName, [])) ->
                List.zip argTypes args |> ignore // validates length of arguments
                let args = reduceArgs args

                if allStatic args
                then Constant(ADTValue(name, adtName, getValues args))
                else Apply(func, args) // Inserts Constant(Closure_) into Apply

            | Constant c -> Constant c
            | _ ->
                sprintf "Reduced func not a function: %O" func
                |> fail
        | Pattern (matchExpression, patternList) ->
            let rActual = reduce2 matchExpression context store
            match rActual with
            | Constant v ->
                patternList
                |> List.pick (fun (pat, body) ->
                    let m = match1 (rActual, pat)
                    match m with
                    | StaticMatch bindings -> Some(reduce2 body context <| (bindings @ store))
                    | _ -> None)
            | _ ->
                let reducedMatches =
                    patternList
                    |> List.choose (fun (pat, body) ->
                        let m = match1 (rActual, pat)
                        match m with
                        | StaticMatch bindings -> Some((pat, reduce2 body context <| (bindings @ store)), true)
                        | DynamicMatch bindings -> Some((pat, reduce2 body false <| (bindings @ store)), false)
                        | NoMatch -> None)

                match reducedMatches with
                | ((_, body), true) :: _ -> body
                | [] ->
                    raise
                    <| ReduceError(e, "All patterns are known statically to not match")
                | many -> Pattern(rActual, List.map fst many)

        | _ -> raise <| ReduceError(e, "No match found")

    indent <- indent - 1
    x

let reduce (e: Expr) (store: Expr Env): Expr = reduce2 e true store
