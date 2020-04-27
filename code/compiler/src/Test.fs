[<AutoOpen>]
module Test

let minus = """
x = -4;
x
"""

let string = """
s = "HELLO";
s
"""

let boolean = """
c = true;
c
"""

let tuple = """
x = (1,2);
x
"""
// let record = fromString "x = { name: 'Jan', age: '25', type: monkey, favFruit: pineapple } \n";;

let simplePrim = """
3 + 4
"""

// TODO: Reducing this one will fail because it doesn't know y - but it should not fail because of this, right?
let simpleFunction = """
func f x =
  x + y
end

f
"""

let simpleFunction2 = """
func f x =
    x = 2;
    x
end
f
"""

let simpleFunctionApplication = """
func f x =
    x = 2;
    x
end
f (3)
"""

let complexFunction = """
y = 0;

func f x z =
  k = x + y * z;
  k
end

f
"""

let complexFunctionApplication = """
func f x y z =
  k = x + y * z;
  k
end
f 1 2 3
""" // result shuold be 7

let partialFunctionApplication = """
func f x y z =
  k = x + y * z;
  k
end

f 1 2
""" // WHAT NAME DO WE EXPECT THE CLOSURE TO HAVE WHEN RETURNED?

let partialFunctionApplication2 = """
func f x y z =
  k = x + y * z;
  k
end

func part y =
  f 1 2 y
end

part
"""

let ifStatement = """
if 3 > 4
then 3
else 4
"""

let simplePattern = """
x = 1;
match x with
| 1 -> true
| _ -> false
"""

let patternInFunction = """
func f x =
match x with
   | Ctor1 -> 42
   | Ctor2 -> 45
end
f
"""

let patternApplication = """
func f x y =
  match x with
   | 1 -> 42
   | 2 -> 43
   | 3 -> 44
   | _ -> 45

end
f (3) (4)
"""

let complexPatternApplication = """
func f x y =
  match (x, y) with
   | (1, (2, _)) -> 40
   | (1, (2, 5)) -> 41
   | (1, (3, 1)) -> 42
   | (1, (3, _)) -> 43
   | (1, (3, 7)) -> 44
end
f (1) ((3, 5))
""" // should evaluate to 43

let partialComplexPatternApplication = """
func f x y =
  match (x, y) with
   | (1, (2, _)) -> 40
   | (1, (2, 5)) -> 41
   | (2, (3, 1)) -> 42
   | (2, (3, _)) -> 43
   | (2, (3, 7)) -> 44
end

func h y =
  f (1) y
end

h
"""

let booleanPattern = """
func f x y =
match (x, y) with
   | (true, (true, true)) -> 40
   | (true, (true, _)) -> 41
   | (true, (_, 5)) -> 42
   | (false, (false, 4)) -> 43
   | (false, (_, 5)) -> 44
end
f false (true, 5)
""" // should evaluate / reduce to 44

let variablePattern = """
func f x y =
match (x, y) with
   | (true, (true, _)) -> 40
   | (true, (true, true)) -> 41
   | (true, (_, false)) -> 42
   | (false, (false, _)) -> 43
   | (false, (true, a)) -> a
end
f false (true, 5)
""" // should evaluate / reduce to 5

let apply = """
func f x y z =
  k = x + y * z;
  k
end

f 3 2 1
"""

let adt = """type DisjointSum =
    Ctor1 Integer
  | Ctor2 String String;
  Ctor2 "a" "b"
"""

let adtPattern = """
type DisjointSum =
    Ctor1 Integer
  | Ctor2 String String;
"""

let adtValue = """
x = Ctor1 0 1;
x
"""

let list = """
x = [1, 2, 3];
x
"""

let listPattern = """
x = [1, 2, 3];
match x with
| [1,2,3] -> true
| _ -> false
"""

let emptyListPattern = """
x = [];
match x with
| [] -> true
| _ -> false
"""

let listHeadTailPattern = """
x = [1,2,3];
match x with
| h::t -> h
| [] -> 0
| _ -> -1
""" // return 1

let listHeadTailPattern2 = """
x = [1,2,3];
match x with
| h::t -> t
| [] -> 0
| _ -> -1
""" // return [2,3]

let recursiveFunctions = """
func fold i ls =
  if (i == 0)
  then ls
  else fold (i-1) ([i, ls])
end


fold 3 []
"""

let adtPatternMatch = """
type Node = Text | Tag String [Node];

func compare view1 view2 =
  match (view1, view2) with
  | (Tag (a) (b), Tag (c) (d)) -> (b,d)
  | _ -> "fail"
end

view1 = Tag ("div") ([(Text "1")]);
view2 = Tag ("div") ([(Text "2")]);

compare (view1) (view2)
"""

let sp = """
type Node = Text String | Tag String [Node] ;

func fold nodes1 nodes2 index =
  match (nodes1, nodes2) with
  | (n1::r1, n2::r2) ->
    match (n1, n2) with
    | (Tag (name1) (children1), Tag (name2) (children2)) ->
        (fold (children1) (children2) (0))
    | (Text (s1), Text (s2)) ->
        if (s1 == s2) then Null else Change(Text(s2))
  | _ -> Null
end

view1 = Tag ("div") ([Text "HELLO"]);
view2 = Tag ("div") ([Text "World"]);

fold ([view1]) ([view2]) (0)
"""

let testReduceIf = """
func myFun a =
  if a then 5 else 6
end

myFun true
"""

let testCases =
    [ minus
      string
      boolean
      tuple
      simplePrim
      simpleFunction
      simpleFunctionApplication
      complexFunction
      complexFunctionApplication
      //  partialfunctionApplication;
      partialFunctionApplication2
      ifStatement
      simplePattern
      patternInFunction
      patternApplication
      complexPatternApplication
      partialComplexPatternApplication
      booleanPattern
      variablePattern
      apply
      adt
      adtPattern
      adtValue
      list
      listPattern ]
