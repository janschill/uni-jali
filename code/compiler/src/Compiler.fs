module Compiler

open AbstractSyntax
open Interpreter
open Util

exception ReduceError of Expr * string with
    override this.Message = sprintf "Reduce error at expression %O \n%s" this.Data0 this.Data1

type Bindings = list<string * Expr>

type ReducedMatch =
    | NoMatch
    | DynamicMatch of Bindings
    | StaticMatch of Bindings

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
    | Prim (op, expression1, expression2) ->
        let rexpr1 = reduce2 expression1 context store
        let rexpr2 = reduce2 expression2 context store
        match op, rexpr1, rexpr2 with
        | _, Constant _, Constant _ -> Constant(eval (Prim(op, rexpr1, rexpr2)) [])
        | "*", _, Constant (IntegerValue 0) -> Constant <| IntegerValue 0
        | "*", _, Constant (IntegerValue 1) -> rexpr1
        | "+", _, Constant (IntegerValue 0) -> rexpr1
        | "*", Constant (IntegerValue 0), _ -> Constant <| IntegerValue 0
        | "*", Constant (IntegerValue 1), _ -> rexpr2
        | "+", Constant (IntegerValue 0), _ -> rexpr2
        | "==", Variable x, Prim("+", Variable x', _) -> Constant <| BooleanValue false
        | "==", Variable x, Prim("+", _, Variable x') -> Constant <| BooleanValue false
        | _ -> Prim(op, rexpr1, rexpr2)
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

            (* tryMatch returns
                    None            if there can never be a match
                    Some bindings   *)

            let rec match1 (actual: Expr, case: Expr) =
                // printfn "match1: %A ? %A" actual case
                match (actual, case) with
                | Constant v1, Constant v2 when v1 = v2 -> StaticMatch []
                | Constant v1, Constant v2 when v1 <> v2 -> NoMatch
                | _, Constant (CharValue '_') -> StaticMatch []

                | Variable dyn, Variable x ->
                    // We're binding a variable to a dynamic value. This will always succeed.
                    // REPORT: Try removing this bit and see what it does to the output. Discuss in the report.
                    match tryLookup store dyn with
                    | Some (Variable y) when y = dyn -> StaticMatch [x, Variable dyn]
                    | v -> printfn "Unexpected value %A for dynamic %s" v x; NoMatch

                | Variable x, _ -> DynamicMatch <| collect case // If actual is dynamic, just collect bindings
                // REPORT: The key problem seems to be that ADT values has too many different representations: Constant ADTValue,
                //         Constant ADTClosure, Apply Variable. Likely the reducer will fail on similarly missing cases as soon
                //         as we try it on a larger program
                | Constant (ADTValue (name, _, [])), Variable x -> StaticMatch []
                | Constant (ADTValue (name, _, values)), Apply (Variable name', exprs) ->
                    if name = name'
                    then matchMany (List.map Constant values) exprs
                    else NoMatch
                | Constant (ADTClosure ((name, _), _, [])), Variable x when name = x -> StaticMatch []
                | _, Variable x ->
                    match tryLookup store x with
                    | Some (Constant (ADTValue (name, sname, vals))) -> NoMatch // the pattern variable might be an adtvalue, e.g. Increment, but matching would have happened in the previous statement
                    | None -> StaticMatch [ (x, actual) ] // Pattern variable
                    | v ->
                        printfn "Unexpected value for %s -> %A" x v
                        NoMatch
                | Constant v, Constant v' when v = v' -> StaticMatch []
                | Apply (name, exprs), Apply (pname, pats) when List.length exprs = List.length pats ->
                    matchMany (name :: exprs) (pname :: pats)
                | Tuple (e1, e2), Tuple (p1, p2) -> matchMany [ e1; e2 ] [ p1; p2 ]
                // REPORT: Also lists has too many distinct representations.
                | List es, List pats when es.Length = pats.Length -> matchMany es pats
                | List (h :: t), ConcatC (h', t') -> matchMany [ h; (List t) ] [ h'; t' ]
                | Constant (ListValue (v :: vs)), ConcatC (e1, e2) ->
                    matchMany [ Constant v; Constant <| ListValue vs ] [ e1; e2 ]
                | _, _ ->
                    printfn "No match: %A ~ %A" actual case
                    NoMatch

            and matchMany exprs pats =
                let matches = List.map match1 <| List.zip exprs pats
                if List.exists ((=) NoMatch) matches then
                    NoMatch
                else
                    let bindings =
                        matches
                        |> List.collect (function
                            | DynamicMatch bindings -> bindings
                            | StaticMatch bindings -> bindings
                            | NoMatch -> failwithf "Impossible: NoMatch")

                    if List.exists (function
                        | DynamicMatch _ -> true
                        | _ -> false) matches then
                        DynamicMatch bindings
                    else
                        StaticMatch bindings

            let reducedMatches =
                patternList
                |> List.choose (fun (pat, body) ->
                    let m = match1 (rActual, pat)
                    printfn "Reduced case %O -> %O" pat m
                    match m with
                    | StaticMatch bindings -> Some((pat, reduce2 body context <| (bindings @ store)), true)
                    | DynamicMatch bindings -> Some((pat, reduce2 body false <| (bindings @ store)), false)
                    | NoMatch -> None)
            // Second part of tuple signals whether the case is known to fully match (true), or
            // only might match depending on dynamic values (false).

            match reducedMatches with
            | ((_, body), true) :: _ ->
                // The first match is static and will always match, no matter the dynamic values.
                // Replace the entire branch expression with the body
                body
            | [] ->
                raise
                <| ReduceError(e, "All patterns are known statically to not match")
            | many -> Pattern(rActual, List.map fst many)

    | _ -> raise <| ReduceError(e, "No match found")


let reduce (e: Expr) (store: Expr Env): Expr = reduce2 e true store
