module AbstractSyntax

type expr =
    | ConstantInteger of int
    | Prim of string * expr * expr
    | Let of string * expr
