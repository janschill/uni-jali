(* Polymorphic type inference for a higher-order functional language    *)
(* The operator (=) only requires that the arguments have the same type *)
(* sestoft@itu.dk 2010-01-07 *)

module TypeInference

(* If this looks depressingly complicated, read chapter 6 of PLCSD *)

open AbstractSyntax

(* Environment operations *)

type 'v env = (string * 'v) list

let rec lookup env x =
    match env with
    | [] -> failwith (x + " not found")
    | (y, v) :: r ->
        if x = y then v else lookup r x

(* Operations on sets of type variables, represented as lists.
   Inefficient but simple.  Basically compares type variables
   on their string names.  Correct so long as all type variable names
   are distinct. *)

let rec mem x vs =
    match vs with
    | [] -> false
    | v :: vr -> x = v || mem x vr

(* union(xs, ys) is the set of all elements in xs or ys, without duplicates *)

let rec union (xs, ys) =
    match xs with
    | [] -> ys
    | x :: xr ->
        if mem x ys then union (xr, ys) else x :: union (xr, ys)

(* unique xs  is the set of members of xs, without duplicates *)

let rec unique xs =
    match xs with
    | [] -> []
    | x :: xr ->
        if mem x xr then unique xr else x :: unique xr

(* A type is int, bool, function, or type variable: *)

type typ =
    | TypI (* integers *)
    | TypFl (* floats  *)
    | TypB (* booleans *)
    | TypC (* chars *)
    | TypS (* strings *)
    | TypT of typ * typ (* tuples *)
    | TypL of typ (* lists *)
    | TypF of typ list * typ (* (argumenttype, resulttype) *)
    | TypV of typevar (* type variable *)

and tyvarkind =
    | NoLink of string (* uninstantiated type var. *)
    | LinkTo of typ (* instantiated to typ *)

and typevar = (tyvarkind * int) ref

(* A type scheme is a list of generalized type variables, and a type: *)

type typescheme = TypeScheme of typevar list * typ (* type variables and type *)

(* *)

let setTvKind tyvar newKind =
    let (kind, lvl) = !tyvar
    tyvar := (newKind, lvl)

let setTvLevel tyvar newLevel =
    let (kind, lvl) = !tyvar
    tyvar := (kind, newLevel)

(* Normalize a type; make type variable point directly to the
   associated type (if any).  This is the `find' operation, with path
   compression, in the union-find algorithm. *)

let rec normType t0 =
    match t0 with
    | TypV tyvar ->
        match !tyvar with
        | (LinkTo t1, _) ->
            let t2 = normType t1
            setTvKind tyvar (LinkTo t2)
            t2
        | _ -> t0
    | _ -> t0

let rec freeTypeVars t: typevar list =
    match normType t with
    | TypT(t1, t2) -> union (freeTypeVars t1, freeTypeVars t2)
    | TypL t -> freeTypeVars t
    | TypV tv -> [ tv ]
    | TypF(ats, rt) -> union (List.collect freeTypeVars ats, freeTypeVars rt)
    | _ -> []

let occurCheck tyvar tyvars =
    if mem tyvar tyvars then failwith "type error: circularity" else ()

let pruneLevel maxLevel tvs =
    let reducelevel tyvar =
        let (_, level) = !tyvar
        setTvLevel tyvar (min level maxLevel)
    List.iter reducelevel tvs

(* Make type variable tyvar equal to type t (by making tyvar link to t),
   but first check that tyvar does not occur in t, and reduce the level
   of all type variables in t to that of tyvar.  This is the `union'
   operation in the union-find algorithm.  *)

let rec linkVarToType tyvar t =
    let (_, level) = !tyvar
    let fvs = freeTypeVars t
    occurCheck tyvar fvs
    pruneLevel level fvs
    setTvKind tyvar (LinkTo t)

let rec typeToString t: string =
    match t with
    | TypI -> "int"
    | TypFl -> "float"
    | TypB -> "bool"
    | TypC -> "char"
    | TypS -> "string"
    | TypL(t) -> "list"
    | TypT(t1, t2) -> "tuple"
    | TypV _ -> failwith "typeToString impossible"
    | TypF(t1, t2) -> "function"

(* Unify two types, equating type variables with types as necessary *)

let rec unify t1 t2: unit =
    let t1' = normType t1
    let t2' = normType t2
    match (t1', t2') with
    | (tt, ttt) when tt = ttt -> ()
    | (TypT(t1, t2), TypT(tt1, tt2)) ->
        unify t1 tt1
        unify t2 tt2
        ()
    | (TypL t, TypL t2) -> unify t t2
    | (TypF(t11, t12), TypF(t21, t22)) ->
        (List.map2 unify t11 t21 |> ignore
         unify t12 t22)
    | (TypV tv1, TypV tv2) ->
        let (_, tv1level) = !tv1
        let (_, tv2level) = !tv2
        if tv1 = tv2 then ()
        else if tv1level < tv2level then linkVarToType tv1 t2'
        else linkVarToType tv2 t1'
    | (TypV tv1, _) -> linkVarToType tv1 t2'
    | (_, TypV tv2) -> linkVarToType tv2 t1'
    | (a, b) -> failwith <| sprintf "type error: %s and %s" (typeToString a) (typeToString b)

(* Generate fresh type variables *)

let tyvarno = ref 0

let newTypeVar level: typevar =
    let rec mkname i res =
        if i < 26
        then char (97 + i) :: res
        else mkname (i / 26 - 1) (char (97 + i % 26) :: res)

    let intToName i = new System.String(Array.ofList ('\'' :: mkname i []))
    tyvarno := !tyvarno + 1
    ref (NoLink(intToName (!tyvarno)), level)

(* Generalize over type variables not free in the context; that is,
   over those whose level is higher than the current level: *)

let rec generalize level (t: typ): typescheme =
    let notfreeincontext tyvar =
        let (_, linkLevel) = !tyvar
        linkLevel > level

    let tvs = List.filter notfreeincontext (freeTypeVars t)
    TypeScheme(unique tvs, t) // The unique call seems unnecessary because freeTypeVars has no duplicates??

(* Copy a type, replacing bound type variables as dictated by tvenv,
   and non-bound ones by a copy of the type linked to *)

let rec copyType subst t: typ =
    match t with
    | TypV tyvar ->
        (* Could this be rewritten so that loop does only the substitution *)
        let rec loop subst1 =
            match subst1 with
            | (tyvar1, type1) :: rest ->
                if tyvar1 = tyvar then type1 else loop rest
            | [] ->
                match !tyvar with
                | (NoLink _, _) -> t
                | (LinkTo t1, _) -> copyType subst t1
        loop subst
    | TypF(t1, t2) -> TypF(List.map (copyType subst) t1, copyType subst t2)
    | TypT(t1, t2) -> TypT(copyType subst t1, copyType subst t2)
    | TypL(t) -> TypL(copyType subst t)
    | TypI -> TypI
    | TypFl -> TypFl
    | TypB -> TypB
    | TypC -> TypC
    | TypS -> TypS

(* Create a type from a type scheme (tvs, t) by instantiating all the
   type scheme's parameters tvs with fresh type variables *)

let specialize level (TypeScheme(tvs, t)): typ =
    let bindfresh tv = (tv, TypV(newTypeVar level))
    match tvs with
    | [] -> t
    | _ ->
        let subst = List.map bindfresh tvs
        copyType subst t

(* Pretty-print type, using names 'a, 'b, ... for type variables *)

let rec showType t: string =
    let rec pr t =
        match normType t with
        | TypI -> "int"
        | TypFl -> "float"
        | TypB -> "bool"
        | TypC -> "char"
        | TypS -> "string"
        | TypT(t1, t2) -> sprintf "%s * %s" (pr t1) (pr t2)
        | TypL t -> sprintf "%s list" (pr t)
        | TypV tyvar ->
            match !tyvar with
            | (NoLink name, _) -> name
            | _ -> failwith "showType impossible"
        | TypF(t1, t2) ->
            let args =
                if List.isEmpty t1
                then "() -> "
                else (List.fold (fun s a -> s + (pr a) + " -> ") "" t1)
            sprintf "(%s%s)" args (pr t2)
    pr t

(* A type environment maps a program variable name to a typescheme *)

type tenv = typescheme env

(* Type inference helper function:
   (typ lvl env e) returns the type of e in env at level lvl *)

let rec typ (lvl: int) (env: tenv) (e: Expr): typ =
    match e with
    | Constant c ->
        match c with
        | IntegerValue _ -> TypI
        | BooleanValue _ -> TypB
        | CharValue _ -> TypC
        | StringValue _ -> TypS
        | _ -> failwith "type inference incomplete :(("
    | Variable x -> specialize lvl (lookup env x)
    | Tuple(e1, e2) ->
        let t1 = typ lvl env e1
        let t2 = typ lvl env e2
        TypT(t1, t2)
    | Prim(ope, e1, e2) ->
        let t1 = typ lvl env e1
        let t2 = typ lvl env e2

        let unify2 (typ, t1, t2) rtyp =
            (unify typ t1
             unify typ t2
             rtyp)
        match ope with
        | "+" -> unify2 (TypI, t1, t2) TypI
        | "-" -> unify2 (TypI, t1, t2) TypI
        | "*" -> unify2 (TypI, t1, t2) TypI
        | ("/") -> unify2 (TypI, t1, t2) TypI
        | ("%") -> unify2 (TypI, t1, t2) TypI
        | (">") -> unify2 (TypI, t1, t2) TypB
        | (">=") -> unify2 (TypI, t1, t2) TypB
        | "<" -> unify2 (TypI, t1, t2) TypB
        | "<=" -> unify2 (TypI, t1, t2) TypB
        | "==" ->
            unify t1 t2
            TypB
        | "!=" ->
            unify t1 t2
            TypB
        // | "&" -> unify2 (TypB, t1) (TypB, t2) TypB
        | _ -> failwith ("unknown primitive " + ope)
    | Let(x, eRhs, letBody) ->
        let lvl1 = lvl + 1
        let resTy = typ lvl1 env eRhs
        let letEnv = (x, generalize lvl resTy) :: env
        typ lvl letEnv letBody
    | If(cond, thenExpr, elseExpr) ->
        let t2 = typ lvl env thenExpr
        let t3 = typ lvl env elseExpr
        unify TypB (typ lvl env cond) //shouldn't we check this first, for effeciency?
        unify t2 t3
        t2
    | Function(f, xs, fBody, letBody) ->
        let lvl1 = lvl + 1
        let fTyp = TypV(newTypeVar lvl1)
        let xTypes = List.map (fun x -> TypV(newTypeVar lvl1)) xs
        let fBodyEnv =
            List.fold2 (fun e x xType -> (x, TypeScheme([], xType)) :: e) ((f, TypeScheme([], fTyp)) :: env) xs xTypes
        let rTyp = typ lvl1 fBodyEnv fBody
        unify fTyp (TypF(xTypes, rTyp))
        let bodyEnv = (f, generalize lvl fTyp) :: env
        typ lvl bodyEnv letBody
    | Apply(eFun, args) ->
        let tf = specialize lvl (lookup env eFun) // Should eFun be an expressipn instead of string??
        let txs = List.map (fun a -> typ lvl env a) args
        let tr = TypV(newTypeVar lvl)
        unify tf (TypF(txs, tr))
        tr
    | ADT(adtName, (constructors: (string * Type list) list), expression) ->
        let superTyp = TypV(ref (NoLink(adtName), lvl))

        let rec translateType (t: Type) =
            match t with
            | Int -> TypI
            | Float -> TypFl
            | Boolean -> TypB
            | String -> TypS
            | Char -> TypC
            | TupleType(type1, type2) ->
                let t1 = translateType type1
                let t2 = translateType type2
                TypT(t1, t2)
            | Typevar(name) -> specialize lvl (lookup env name)
            | ListType(t) -> TypL(translateType t)

        let typeConstructor (name, typeList) =
            let xTypes = List.map (translateType) typeList
            let fTyp = TypF(xTypes, superTyp)
            (name, generalize lvl fTyp)

        let newEnv = List.fold (fun e c -> typeConstructor c :: e) env constructors
        typ lvl newEnv expression
    | Pattern(matchExpression, ((case, expr) :: patterns)) ->
        let lvl1 = lvl + 1
        let xType = typ lvl1 env matchExpression
        let resType = typ lvl1 env expr
        List.iter (fun (case, expr) ->
            unify xType (typ lvl env case)
            unify resType (typ lvl1 env expr)) patterns
        resType



(* Type inference: tyinf e0 returns the type of e0, if any *)

let rec tyinf e0 = typ 0 [] e0

let inferType e =
    (tyvarno := 0
     showType (tyinf e))
