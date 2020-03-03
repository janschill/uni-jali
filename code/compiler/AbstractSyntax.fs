module AbstractSyntax

type expr =
    | ConstantInteger of int
    | ConstantBoolean of bool
    | Variable of string
    | Prim of string * expr * expr
    | Let of string * expr
    | Function of string * string * expr
