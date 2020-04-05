module Compiler

open AbstractSyntax

let rec eval (e: Expr) (vs: Value Env): Option<Value> =
    match vs with
    | [] -> None
    | (key, value) :: vsr ->
        if e = key then Some(value) else eval e vsr

let rec reduce (e: Expr) (vs: Value Env): Expr = e

(*
  Iterates through AST and return specialized AST
*)
let rec specalize (ast: Expr): Expr =
    match ast with
    | Function(name, parameters, expression, expression2) -> ast

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

--------
type Msg = Increment | Decrement
func update model msg =
  if msg == Increment
  then model + 1
  else model - 1
end
--------
update_increment model = model + 1
update_decrement model = model - 1

poly [
    {update, update_increment model = model + 1}
    {update, update_decrement model = model - 1}
]

poly [
    {update, (Increment)}
    {update, (Decrement)}
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

*)

let rec reduce (e: Expr) (vs: Value Env): Expr =
    match e with
    | Constant c -> Constant c
    // | Variable v -> lookup env v
    | Tuple(expr1, expr2) -> Tuple(reduce expr1 env, reduce expr2 env)
    | Prim(operation, expression1, expression2) ->
        let expr1 = reduce expression1 env
        let expr2 = reduce expression2 env
        Prim(operation, expr1, expr2)
    | Let(name, expression1, expression2) ->
        let expr = reduce expression1 env
        let newEnv = (name, expr) :: env
        Let(name, expr, reduce expression2 newEnv)
    | If(cond, thenExpr, elseExpr) ->
        let staticCond = eval (cond) vs
        match staticCond with
        | None -> If(reduce (cond), reduce (thenExpr), reduce (elseExpr))
        | Some(v) ->
            if v then thenExpr else elseExpr

    | Function(name, parameters, expression, expression2) ->
        let closure = Closure(name, parameters, expression, env)
        let newEnv = (name, closure) :: env
        eval expression2 newEnv
    | ADT(adtName, (constructors: (string * Type list) list), expression) ->
        let newAdt constructorDeclaration =
            if List.isEmpty (snd constructorDeclaration)
            then ADTValue(fst constructorDeclaration, adtName, [])
            else ADTClosure(constructorDeclaration, adtName, env)

        let finalEnv =
            List.fold (fun newEnv constructorDeclaration ->
                let name = fst constructorDeclaration
                (name, newAdt constructorDeclaration) :: newEnv) env constructors

        eval expression finalEnv
    | Apply(fname, farguments) ->
        let fclosure = lookup env fname
        match fclosure with
        | Closure(cname, cparameters, cexpression, declarationEnv) ->
            let newEnv =
                List.fold2 (fun dEnv parameterName argument -> (parameterName, eval argument env) :: dEnv)
                    ((cname, fclosure) :: declarationEnv) cparameters farguments // should evaluated args also be added to env, since later args are evaluated with this env? Also, should we test for duplicate names?
            eval cexpression newEnv
        | ADTClosure((constructor: string * Type list), adtName, declarationEnv) ->
            let values = List.map (fun arg -> eval arg env) farguments
            ADTValue(fst constructor, adtName, values) // we chould check whether the arguments have the same length and types as the type list ??
        | _ -> failwith <| sprintf "Evaluator failed on apply: %s is not a function" fname
    | Pattern(matchExpression, (patternList)) ->
        let rec matchSingle (actual: Value) (pattern: Expr) =
            match (actual, pattern) with
            | (_, Constant(CharValue '_')) -> Some []
            | (a, Constant v) when a = v -> Some []
            | (a, Variable x) ->
                let value = tryLookup env x
                if value.IsNone
                then Some [ (x, a) ]
                else matchSingle actual (Constant(Option.get (value)))
            | (ADTValue(constructorName, superName, values), Apply(callName, exprs)) when constructorName = callName ->
                let evaluatedArguments = List.map2 matchSingle values exprs
                if List.forall Option.isSome evaluatedArguments
                then Some(List.collect Option.get evaluatedArguments)
                else None
            | (TupleValue(v1, v2), Tuple(p1, p2)) ->
                match (matchSingle v1 p1, matchSingle v2 p2) with
                | (Some(v1), Some(v2)) -> Some(v1 @ v2)
                | _ -> None
            | _, _ -> None

        let rec find x =
            function
            | (case, expr) :: ps ->
                match matchSingle x case with
                | None -> find x ps
                | Some(bindings) -> Some(expr, bindings)
            | [] -> None

        let body =
            let x = eval matchExpression env
            find x patternList

        match body with
        | Some(expr, bindings) -> env @ bindings |> eval expr
        | None -> failwith "Pattern match incomplete"
    | _ -> failwith "No match found"
