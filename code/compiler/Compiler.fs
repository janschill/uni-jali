module Compiler

open AbstractSyntax
open Interpreter

(*
TODO:

(specialized) program point:
  - specialized function name (f, vs)
  - f: original function name
  - vs: static parameters of f
  - conditionals can also be pp

value store: holds all evaluated static expressions

value domain {S,D}:
  - S: ordinary values (static results)
  - D: residual expressions & values (dynamic results)

find division: assign variables either static or dynamic

two functions:
  - eval(exp, vs): evaluates static expression
  - reduce(exp, vs): applies constant folding of static parts
                     in dynamic expression

specialization algorithm:
  - pending: set of functions to bet specialized
  - marked: set with already specialized
  - as long as pending, construct version of f,
    specialized to the values vs of its static params
  - add new functions from f‘s body to pending

Should be parsed as AST:
Subject program:
--------
type Msg = Increment | Decrement
model = 1
func update msg model =
  if msg == Increment
  then model + 1
  else model - 1
end
model = update (Increment) (model)
model = update (Decrement) (model)
--------
Specialized/target program:
--------
type Msg = Increment | Decrement
model = 1
func update_increment model =
  model + 1
end
func update_decrement model =
  model - 1
end
model = update_increment (model)
model = update_decrement (model)
--------

poly [
    {update, update_increment model = model + 1}
    {update, update_decrement model = model - 1}
]

(pp, store)
computational state:
  pp:
    - current point of control
  store:
    - current values of all program variables

specialization time:
  - reduce program with given input (not all is given yet)
  - during specialization time (when not all inputs are given)
    we cannot evaluate all expression, due to missing inputs
  - thus store is incomplete
  - static: can be evaluate during specialization time
  - dynamic: cannot be evaluate - "" -

func f x = x + 1
z = 3;

func myFun x y =
  a = x + f (3);
  a
end

func hello x =
  b = 3 + myFun 4
  b
end

myFun (4) (5);
*)

(*
Chat:
12:27:34 From Søren Debois : Let f x y = x + y
12:27:55 From Søren Debois : Let f x  = x + 2
12:28:10 From Søren Debois : Constant 2
12:28:55 From Søren Debois : Rexpr1 = Variable x
12:29:01 From Søren Debois : Rexpr2 = Constant 2
12:29:15 From Søren Debois : Plus (Rexpr1, Rexpr2)
12:35:16 From Søren Debois : Let x = Cons (static, dynamic) in match x with Cons (s, _) -> s + 1 end
12:35:54 From Søren Debois : rexpr
12:36:04 From Søren Debois : x -> Dynamic (rexpr)
12:40:48 From Søren Debois : Match (Cons (static, dynamic) with Cons (s, …) -> s
12:43:40 From Søren Debois : Match (Cons (static, dynamic) with Cons (s, d) -> s


- are we on the right track?
- are we creating several functions for the same functions? E.g. update should be two functions, one for increment, and one for decrement?
- when do we do that? In the apply or the function definition?
- dynamic values?
- Should we follow Sestoft?:
    - Binding-time analysis by abstract interpretation
        (division - mapping function names to a binding-time environment T, which maps variables xj to their
        bindingtime tj, where tj E {S,D})
    - Annotate the program as dynamic/static calls
        (From division to annotation - creating a two-level syntax)
    - Specializing
        (reducing, unfolding based on certain rules)
    - Pre
- confused about env, value store (static), value domain (static and dynamic), list of function versions

Subject program:
--------
type Msg = Increment | Decrement
model = 1
func update msg model =
  if msg == Increment
  then model + 1
  else model - 1
end
model = update (Increment) (model)
model = update (Decrement) (model)
--------
Specialized/target program:
--------
type Msg = Increment | Decrement
model = 1
func update_increment model =
  model + 1
end
func update_decrement model =
  model - 1
end
model = update_increment (model)
model = update_decrement (model)
--------
*)

type Ds =
    | Dynamic of Expr
    | Static of Value

let rec lookup (e: string) (staticStore: Ds Env): Ds =
    match staticStore with
    | [] -> failwithf "%s not found" e
    | (key, value) :: rest ->
        if e = key then value else lookup e rest

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
        match reduce expression1 store with
        | Constant c -> (name, Static(c)) :: store |> reduce expression2
        | rexpr -> (name, Dynamic(rexpr)) :: store |> reduce expression2
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
        | expr -> (name, Dynamic(Function(name, parameters, rbody, expression2))) :: store |> reduce expression2
    | ADT(adtName, (constructors: (string * Type list) list), expression) ->
        let eval (name, argTypes) = (name, Static(ADTClosure((name, argTypes), adtName, [])))

        let storeWithConstructors = List.fold (fun s c -> eval c :: s) store constructors

        reduce expression storeWithConstructors // TODO: IS THIS CORRECT OR SHOULD WE RETURN AN ADT???
    | Apply(fname, farguments) ->
        let func = lookup fname store
        match func with
        | Static(ADTClosure((name, argTypes), adtName, [])) ->
            let reducedArgs = List.map (fun arg -> reduce arg store) farguments

            let values =
                List.map2 (fun argType reducedArg ->
                    match reducedArg with
                    | Constant(c) -> Static(c)
                    | rexp -> Dynamic(rexp)) argTypes reducedArgs

            let isStatic =
                List.forall (fun a ->
                    match a with
                    | Constant c -> true
                    | _ -> false) reducedArgs

            if isStatic then
                let values = List.map (fun (Constant(c)) -> c) reducedArgs
                Constant(ADTValue(name, adtName, values))
            else
                Apply(fname, reducedArgs) // TODO: Can I eval here, even though i can't give eval an environment?

        | Static((c)) -> Constant(c)
        | Dynamic(Function(name, parameters, expression, expression2)) ->
            // this will never be variables, because they have been replaced by their expressions -> is this really correct?
            let reducedArgs = List.map (fun arg -> reduce arg store) farguments

            let bodyStore =
                List.fold2 (fun s name ra ->
                    match ra with
                    | Constant(c) -> (name, Static(c)) :: s
                    | rexp -> (name, Dynamic(rexp)) :: s) store parameters reducedArgs

            reduce expression bodyStore

        | _ -> failwith <| sprintf "Reduce failed on apply: %O is not a function" func
    | Pattern(matchExpression, (patternList)) ->
        let rec matchSingle (actual: Expr) (pattern: Expr) =
            match (actual, pattern) with
            | (_, Constant(CharValue '_')) -> Some []
            | (Constant av, Constant pv) when av = pv -> Some []
            | (Constant v, Variable bindName) -> Some [ (bindName, Static(v)) ]
            | (e, Variable bindName) -> Some [ (bindName, Dynamic(e)) ]
            | (Variable x, _) -> Some []
            | (Apply(aConstructorName, aValues), Apply(pConstructorName, pValues)) when aConstructorName =
                                                                                            pConstructorName ->
                let evaluatedArguments = List.map2 matchSingle aValues pValues
                if List.forall Option.isSome evaluatedArguments
                then Some(List.collect Option.get evaluatedArguments)
                else None
            | (Tuple(e1, e2), Tuple(p1, p2)) ->
                match (matchSingle e1 p1, matchSingle e2 p2) with
                | (Some(v1), Some(v2)) -> Some(v1 @ v2)
                | _ -> None
            | _, _ -> None

        let reduceExpr (expr, bindings): Expr = List.foldBack (fun b s -> b :: s) bindings store |> reduce expr

        let findAndReduceManyPatterns (x: Expr) patterns =
            let rec findPatterns x patterns =
                match patterns with
                | (case, expr) :: tail ->
                    match matchSingle x case with
                    | None -> findPatterns x tail
                    | Some(bindings) -> (case, reduceExpr (expr, bindings)) :: findPatterns x tail
                | [] -> []

            findPatterns x patterns

        let evalAndReduce (v: Value) patterns =
            match Interpreter.findPattern v patterns with
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
