module AbstractSyntax

type Type =
    | Int
    | Float
    | Boolean
    | String
    | Char
    | TupleType of Type * Type
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
    | ConcatC of Expr * Expr
    | List of Expr list
    | Constant of Value
    | StringLiteral of string
    | Variable of string
    | Tuple of Expr * Expr
    | Prim of string * Expr * Expr
    | And of Expr * Expr
    | Or of Expr * Expr
    | Negate of Expr
    | Let of string * Expr * Expr
    | If of Expr * Expr * Expr
    | Function of string * string list * Expr * Expr (* (f, x, fBody, letBody) *)
    | ADT of string * ADTConstructor list * Expr
    | Apply of Expr * Expr list
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
    | ListValue(vs) -> "[" + (List.map printValue vs |> List.reduce (fun a b -> a + ", " + b)) + "]"
    | ADTValue(name, supertype, vs) -> name + (List.fold (fun acc e -> acc + " " + printValue e) "" vs)
    | _ -> sprintf "%O" d
// | Closure(name, pars, body, env) -> name + " : (" + List.reduce (fun a b -> a + ", " + b) pars + ") -> "
// | ADTClosure((name, types), supername, env) -> supername + " : " + name
// | _ -> "Couldn't find proper value"

let rec printExpr (d: Expr): string =
    match d with
    | ConcatC(h, t) -> printExpr h + "::" + printExpr t
    | List(es) -> "[" + (List.map printExpr es |> List.reduce (fun a b -> a + ", " + b)) + "]"
    | Constant v -> printValue v
    | StringLiteral s -> sprintf "%s" s
    | Variable x -> sprintf "var %s" x
    | Tuple(a, b) -> "(" + printExpr a + ", " + printExpr b + ")"
    | Prim(op, a, b) -> "prim: " + printExpr a + op + printExpr b
    | Let(name, a, b) -> sprintf "let %s = %s;" name (printExpr a)
    | If(a, b, c) -> sprintf "if (%s) then %s else %s" (printExpr a) (printExpr b) (printExpr c)
    | Function(name, pars, body, next) -> printValue (Closure(name, pars, body, []))
    | ADT(name, cs, next) -> "ADT: " + name
    | Apply(name, args) ->
        "Apply(" + printExpr name + "(" + (List.map printExpr args |> List.reduce (fun a b -> a + ", " + b)) + ")"
    | Pattern(x, patterns) ->
        "match " + printExpr x + " with\n"
        + (List.reduce (+) <| List.map (fun (c, e) -> "| " + printExpr c + " -> " + printExpr e + "\n") patterns)


let rec printArg (d: Expr): string =
    match d with
    | ConcatC(h, t) -> "Con(" + printExpr h + "::" + printExpr t + ")"
    | List(es) -> "[" + (List.map printExpr es |> List.reduce (fun a b -> a + ", " + b)) + "]"
    | Constant v -> printValue v
    | StringLiteral s -> sprintf "'%s'" s
    | Variable x -> sprintf "Var() %s" x
    | Tuple(a, b) -> "(" + printExpr a + ", " + printExpr b + ")"
    | Prim(op, a, b) -> "Prim( " + printExpr a + op + printExpr b + ")"
    | Let(name, a, b) -> sprintf "Letbind"
    | If(a, b, c) -> sprintf "IfElse"
    | Function(name, pars, body, next) -> sprintf "Func(" + name + ")"
    | ADT(name, cs, next) -> "ADT(" + name + ")"
    | Apply(name, args) -> "Apply(" + printArg name + ")"
    | Pattern(x, patterns) -> sprintf "Pattern"
    | _ -> sprintf "%O" d