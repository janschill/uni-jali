module Janitor

type Attribute = Att of string * string

type Node =
    | Text of string
    | Tag of string * Attribute list * Node list

type Differ =
    | Change of Node
    | Path of int * Differ

// type Option =
//     | None
//     | Some of Differ

let rec equal n1 n2: bool = (n1 = n2)
// match (n1,n2) with
// | (Tag(t1, atts1, ns1), Tag(t2, atts2, ns2)) ->
//     (t1=t2 && List.fold2 (fun s a1 a2 -> a1 = a2) false atts1 atts2 )

let rec diff view1 view2: Option<Differ> =
    match (view1, view2) with
    | (Tag(t1, atts1, ns1), Tag(t2, atts2, ns2)) ->
        let changes = fold ns1 ns2
        match List.map Option.get changes with
        | [] -> None
        | cs -> Some(cs)
    | (Text(s1), Text(s2)) ->
        if (s1 = s2) then None else Some(Change(view2))

and fold ns1 ns2 = List.map2 (diff) ns1 ns2
// let (i, indices) =
//     List.fold2 (fun (i, is) n1 n2 ->
//         let diff = diff n1 n2
//         // if (diff) then Path(i,Change(node)) else ??????
//         if (diff n1 n2) then (i + 1, i :: is) else (i + 1, is)) (0, []) ns1 ns2
// match indices with
// | [] -> None
// | _ -> Some(indices)

(*
  traverse view and replace nodes with nodes
  from changes by looking at index in changes
*)
let rec patch view changes = 0

let view model =
    Tag
        ("div", [],
         [ Text(model)
           Text("Hello World") ])

(*
v1 = Tag ("div")
v2 = Tag ("div")
change = None

v1 = Tag ("div") ([ Text ("Hello"); Text ("World") ])
v2 = Tag ("div") ([ Text ("What's"); Text ("Up") ])
change = Path(0, Path(1,Change( Text ("What's")) )

v1 = Tag ("div")
v2 = Tag ("button")
change = Some(Path(0, Change (Tag ("button")))

v1 = Tag ("div" [ Tag ("button") ])
v2 = Tag ("div" [ Text ("Hello world")])
change = Some(Path(0, Path(1, Change(Text("Hello world")))))

v1 = Tag ("div2" [ Tag ("button") ])
v2 = Tag ("div" [ Tag ("button") ])
change = Some(Path(0, Change( Tag ("div" [ Tag ("button") ]) )))


*)

[<EntryPoint>]
let main argv =
    let v1 = view ("1")
    let v2 = view2 ("2")
    let changes = diff v1 v2
    let patchedView = patch v1 changes
    0
