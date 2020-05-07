[<AutoOpen>]
module Janitor

type Attribute = Att of string * string

type Node =
    | Text of string
    | Tag of string * Attribute list * Node list

type Differ =
    | Null
    | Change of Node
    | Path of int * Differ
// | Path of list<(int * Differ)>

let rec diff (view1: Node) (view2: Node): Differ =
    let rec fold (nodes1: list<Node>) (nodes2: list<Node>) (index): Differ =
        match (nodes1, nodes2) with
        | (n1 :: ns1, n2 :: ns2) ->
            match diff n1 n2 with
            | Null -> fold ns1 ns2 <| index + 1
            | change -> Path(index, change)
        | _ -> Null // extend cases: list, []; [], list; both empty

    match (view1, view2) with
    | (Tag(t1, atts1, ns1), Tag(t2, atts2, ns2)) ->
        if t1 = t2 && atts1 = atts2 then fold ns1 ns2 0 else Change(Tag(t2, atts2, ns2))
    | (Text(s1), Text(s2)) ->
        if (s1 = s2) then Null else Change(Text(s2))
    | _ -> failwith "Diff error"


(*
  traverse view and replace nodes with nodes
  from changes by looking at index in changes
*)

let rec patch (view: Node) (changes: Differ): Node =
    match changes with
    | Null -> view
    | Change(n) -> n
    | Path(index, d) ->
        match view with
        | Tag(name, atts, nodes) ->
            let items =
                List.mapi (fun i item ->
                    if i = index then patch item d else item) nodes
            Tag(name, atts, items)
        | _ -> failwith "TODO: real error"

let view model =
    Tag
        ("div", [],
         [ Text("12313")
           Tag
               ("div", [],
                [ Text(model)
                  Text("Hello World") ]) ])

let v1 = view ("1")
let v2 = view ("2")
let changes = diff v1 v2
let patched = patch v1 changes

[<EntryPoint>]
let main argv = 0
