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

let rec reduce2 (e: Expr) (context: bool) (store: Expr Env): Expr =
    (* This will print out the store and the expression it is about to reduce *)
    List.fold (fun u (n, e) -> printArg e |> printf "(%s,%s) " n |> ignore) () (("Store", StringLiteral(":")) :: store)
    printfn "\n"
    printfn "Reducing %O\n" e
    let fail msg = ReduceError(e, msg) |> raise
    match e with
    | Constant c -> Constant c
    | Variable x -> lookup store x
    | ConcatC (h, t) ->
        let head = reduce2 h context store
        let tail = reduce2 t context store
        match (head, tail) with
        | (Constant v, Constant (ListValue (vs))) -> Constant(ListValue(v :: vs))
        | _ -> ConcatC(head, tail)
    | Tuple (expr1, expr2) ->
        let rexpr1 = reduce2 expr1 context store
        let rexpr2 = reduce2 expr2 context store
        match (rexpr1, rexpr2) with
        | (Constant v1, Constant v2) -> Constant(TupleValue(v1, v2))
        | _ -> Tuple(rexpr1, rexpr2)
    | List (list) ->
        let reducedItems =
            List.map (fun e -> reduce2 e context store) list

        if (allStatic reducedItems)
        then Constant(ListValue(getValues reducedItems))
        else List(reducedItems)
    | Prim (operation, expression1, expression2) ->
        let rexpr1 = reduce2 expression1 context store
        let rexpr2 = reduce2 expression2 context store
        if allStatic [ rexpr1; rexpr2 ]
        then Constant(eval (Prim(operation, rexpr1, rexpr2)) [])
        else Prim(operation, rexpr1, rexpr2)
    | Let (name, expression1, expression2) ->
        let e = reduce2 expression1 context store
        (name, e) :: store |> reduce2 expression2 context
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
        // patternlist is just a list of tupels: (case -> body)
        let rActual = reduce2 matchExpression context store
        printfn "The actual expression to match is: %O\n" rActual
        match rActual with
        | Constant v ->
            let lookupVal x =
                match tryLookup store x with
                | Some (Constant (v)) -> Some(v)
                | _ -> None

            let matchAndReduce actual (case, expr) =
                tryMatch lookupVal actual case
                |> Option.map (List.map (fun (name, value) -> name, Constant value))
                |> Option.map (fun bindings -> reduce2 expr context <| bindings @ store)

            match List.tryPick (matchAndReduce v) patternList with
            | Some (expr) -> expr
            | None -> raise <| ReduceError(e, "No pattern matching")
        | _ ->
            let rec collect = // If actual is dynamic, then this just collects all variables as bindings, in order to reduce the body
                function
                | Variable x ->
                    match tryLookup store x with
                    | Some (Constant (ADTClosure (name, sname, vals))) -> []
                    | _ -> [ (x, Variable x) ]
                | Tuple (e1, e2) -> collect e1 @ collect e2
                | List exprs -> exprs |> List.collect collect
                | ConcatC (h, t) -> collect h @ collect t
                | Apply (name, values) -> values |> List.collect collect
                | _ -> []

            let rec tryMatch (actual: Expr) (case: Expr) =
                match (actual, case) with
                | _, Constant (CharValue '_') -> Some []
                | Variable x, _ -> Some <| collect case // If actual is dynamic, just collect bindings
                | Constant (ADTValue (name, _, [])), Variable x when name = x -> Some []
                | Constant (ADTClosure ((name, _), _, [])), Variable x when name = x -> Some []
                | _, Variable x ->
                    match tryLookup store x with
                    | Some (Constant (ADTValue (name, sname, vals))) -> None // the pattern variable might be an adtvalue, e.g. Increment, but matching would have happened in the previous statement
                    | _ -> Some [ (x, actual) ]
                // | Apply (expr, args), Apply (pExpr, pargs) -> forAll tryMatch (expr :: args) (pExpr :: pargs)
                // TODO: this should fix e.g. Apply(Constant(Closure(_)) . It checks if everything matches: expr match pExpr, and args match pargs
                // However, it throws the error 'list1 shorter than list2'
                | Constant v, Constant v' when v = v' -> Some []
                | Apply (name, values), Apply (pname, patternValues) when name = pname ->
                    forAll tryMatch values patternValues
                | Tuple (e1, e2), Tuple (p1, p2) ->
                    match tryMatch e1 p1, tryMatch e2 p2 with
                    | Some v1, Some v2 -> Some <| v1 @ v2
                    | _ -> None
                | List exprs1, List exprs2 when exprs1.Length = exprs2.Length -> forAll tryMatch exprs1 exprs2
                | List (h :: t), ConcatC (h', t') -> forAll tryMatch [ h; (List t) ] [ h'; t' ]
                | _, _ -> None

            let matchAndReduce (case, exp) =
                tryMatch rActual case
                |> Option.map (fun bindings -> reduce2 exp false <| (bindings @ store))
                |> Option.map (fun rBody -> (case, rBody))

            match List.choose (matchAndReduce) patternList with
            | [] -> raise <| ReduceError(e, "Empty patternlist")
            | [ (case, body) ] -> body
            | many -> Pattern(rActual, many)

    | _ -> raise <| ReduceError(e, "No match found")


let reduce (e: Expr) (store: Expr Env): Expr = reduce2 e true store
