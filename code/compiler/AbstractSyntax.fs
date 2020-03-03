module AbstractSyntax

type expr =
    | ConstantInteger of int
    | ConstantBoolean of bool
    | Prim of string * expr * expr
    | Let of string * expr
    | Function of string * string * expr
