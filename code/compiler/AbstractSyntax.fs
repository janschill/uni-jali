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
    | Tuple of Value * Value
    | ADTValue of string * Value
    | Closure of string * string * Expr * Value Env (* (f, x, fBody, fDeclEnv) *)

and Expr =
    | Program of Expr list
    | Constant of Value
    | Variable of string
    | Prim of string * Expr * Expr
    | Let of string * Expr
    | If of Expr * Expr * Expr
    | Pattern of Expr list * (Expr * Expr list) list
    | Function of string * string list * Expr list
    | ADT of string * ADTConstructor list
