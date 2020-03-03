module AbstractSyntax

type expr =
    | ConstantInteger of int
    | ConstantBoolean of bool
    | Variable of string
    | Prim of string * expr * expr
    | Let of string * expr
<<<<<<< HEAD
    | Function of string * string * expr list
=======
    | Function of string * string * expr
    | Tuple of expr * expr
>>>>>>> bb23c8be66a19e49406234a6973749d0d3a6cc44
