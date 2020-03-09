module Interpreter

open AbstractSyntax

type 'v env = (string * 'v) list

let rec lookup env x =
    match env with 
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

(* A runtime value is an integer or a function closure *)
type value = 
  | Integer of int
  | Boolean of bool
  | TupleVal of value * value
  | Closure of string * string * expr * value env       (* (f, x, fBody, fDeclEnv) *)

let rec eval (e: expr) (env : value env) : value =
    match e with
    | Program(x::xs) -> 
        let xval = eval x env
        if xs.IsEmpty then xval else eval (Program(xs)) env     // TODO: this just returns the last value in the program...
    | ConstantInteger i -> Integer i
    | Variable v -> lookup env v
    | Let(name, expression) -> failwith "not implemented"
    | Prim(operation, expression1, expression2) ->
        let value1 = eval expression1 env
        let value2 = eval expression2 env
        match (value1, value2) with 
        | ((Integer(v1)), Integer(v2)) -> 
            match (operation) with
            | ("+") -> Integer(v1 + v2)
            | ("-") -> Integer(v1 - v2)
            | ("*") -> Integer(v1 - v2)
            | ("/") -> Integer(v1 / v2)
            | ("%") -> Integer(v1 % v2)
            | (">") -> Boolean(v1 > v2)
            | (">=") -> Boolean(v1 >= v2)
            | ("<") -> Boolean(v1 < v2)
            | ("<=") -> Boolean(v1 <= v2)
            | ("==") -> Boolean(v1 = v2)
            | ("!=") -> Boolean(v1 <> v2)
            | _ -> failwithf "%s is not a valid operation on integers" operation
        | _ -> failwith "Sorry, can only operate on integers"
    | If(cond, thenExpr, elseExpr) -> 
        match eval cond env with
        | Boolean true -> eval thenExpr env
        | Boolean false -> eval elseExpr env
        | _ -> failwith "Evaluator failed on if-statement: condition must be a boolean value"
    | Function(name, parameter, expressions) -> failwith "not implemented"
    | Tuple(expression1, expression2) ->
        let value1 = eval expression1 env
        let value2 = eval expression2 env
        TupleVal (value1, value2)
    | ADT(adtName, (constructors: adtconstructor list)) -> failwith "not implemented"