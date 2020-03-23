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
    | CharValue of char
    | TupleValue of Value * Value
    | ADTValue of string * string * Value list
    | Closure of string * string list * Expr * Value Env (* (f, x, fBody, fDeclEnv) *)

and Expr =
    | Constant of Value
    | StringLiteral of string
    | Variable of string
    | Tuple of Expr * Expr
    | Prim of string * Expr * Expr
    | Let of string * Expr * Expr
    | If of Expr * Expr * Expr
    | Function of string * string list * Expr * Expr (* (f, x, fBody, letBody) *)
    | Apply of string * Expr list
    | Pattern of Expr * (Expr * Expr) list
    | ADT of string * ADTConstructor list * Expr
