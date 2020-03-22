module AbstractSyntax

type Type =
    | Int
    | Float
    | Boolean
    | String
    | Char
    | Typevar of string

type ADTConstructor = string * Type list

type 'v Env = (string * 'v) list

type Value =
    | IntegerValue of int
    | BooleanValue of bool
    | TupleValue of Value list
    | ADTValue of string * Value list
    | Closure of string * string list * Expr * Value Env (* (f, x, fBody, fDeclEnv) *)

and Expr =
    | Constant of Value
    | Variable of string
    | Prim of string * Expr * Expr
    | Let of string * Expr * Expr
    | If of Expr * Expr * Expr
    | Tuple of Expr * Expr
    | Apply of string * Expr list
    | Pattern of Expr * (Expr * Expr) list
    | Function of string * string list * Expr * Expr (* (f, x, fBody, letBody) *)
    | ADT of string * ADTConstructor list * Expr
