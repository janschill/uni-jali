module AbstractSyntax

type Type =
    | Int
    | Float
    | Boolean
    | String
    | Char
    | ListType of Type
    | Typevar of string

type ADTConstructor = string * Type list

type 'v Env = (string * 'v) list

type Value =
    | IntegerValue of int
    | BooleanValue of bool
    | CharValue of char
    | StringValue of string
    | TupleValue of Value * Value
    | ListValue of Value list
    | ADTValue of string * string * Value list
    | Closure of string * string list * Expr * Value Env (* (f, x, fBody, fDeclEnv) *)
    | ADTClosure of ADTConstructor * string * Value Env (* (f, x, fBody, fDeclEnv) *)

and Expr =
    | List of Expr list
    | Constant of Value
    | StringLiteral of string
    | Variable of string
    | Tuple of Expr * Expr
    | Prim of string * Expr * Expr
    | And of Expr * Expr
    | Or of Expr * Expr
    | Let of string * Expr * Expr
    | If of Expr * Expr * Expr
    | Function of string * string list * Expr * Expr (* (f, x, fBody, letBody) *)
    | ADT of string * ADTConstructor list * Expr
    | Apply of string * Expr list
    | Pattern of Expr * (Expr * Expr) list

// and Pattern =
//     | ConstPattern of Value
//     | Binding of string

let rec printValue d =
    match d with
    | IntegerValue i -> sprintf "%i" i
    | BooleanValue b -> sprintf "%b" b
    | CharValue c -> "'" + (string c) + "'"
    | StringValue s -> "\"" + s + "\""
    | TupleValue(v1, v2) -> "(" + (printValue v1) + "," + (printValue v2) + ")"
    | ADTValue(name, supertype, vs) -> name + (List.fold (fun acc e -> (printValue e) + " " + acc) "" vs)
    | _ -> sprintf "%O" d
// | _ -> "Couldn't find proper value"
