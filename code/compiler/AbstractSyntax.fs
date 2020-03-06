module AbstractSyntax

type types =
    | Int
    | Float
    | Boolean
    | String
    | Char
    | Typevar of string

type adtconstructor = string * types list

type expr =
    | ConstantInteger of int
    | ConstantBoolean of bool
    | Variable of string
    | Prim of string * expr * expr
    | Let of string * expr
    | Function of string * string * expr list
    | Tuple of expr * expr
    | ADT of string * adtconstructor list

type program = Program of expr list