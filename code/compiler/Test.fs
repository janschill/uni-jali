[<AutoOpen>]
module Test

let minus = """
x = -4;
x
"""

let string = """
"HELLO";
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

let simpleprim = """
3 + 4
"""

// TODO: Reducing this one will fail because it doesn't know y - but it should not fail because of this, right?
let simplefunction = """
func f x =
  x + y
end

f
"""

let simplefunctionApplication = """
func f x =
    x = 2;
    x
end
f (3)
"""

let complexfunction = """
y = 0;

func f x y z =
  k = x + y * z;
  k
end

f
"""

let complexfunctionApplication = """
func f x y z =
  k = x + y * z;
  k
end
f 1 2 3
""" // result shuold be 11

// TODO: The below partial application is currently not working in our language!!:

// let partialfunctionApplication = """
// func f x y =
//   x + y
// end

// f 2
// """

let partialfunctionApplication2 = """
func f x y z =
  k = x + y * z;
  k
end

func part y =
  f 1 2 y
end

part
"""

let ifstmt = """
if 3 > 4
then 3
else 4
"""

let pattern = """
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

let complexpatternApplication = """
func f x y =
  match (x, y) with
   | (1, (2, _)) -> 40
   | (1, (2, 5)) -> 41
   | (1, (3, 1)) -> 42
   | (1, (3, _)) -> 43
   | (1, (3, 7)) -> 44
end
f (1) ((3, 5))
"""

let partialComplexpatternApplication = """
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

let booleanpatternApplication = """
func f x y =
match (x, y) with
   | (true, (true, _)) -> 40
   | (true, (true, true)) -> 41
   | (true, (true, false)) -> 42
   | (false, (false, _)) -> 43
   | (false, (true, _)) -> 44
end
f false (true, 5)
"""

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

let adtpattern = """
type DisjointSum =
    Ctor1 Integer
  | Ctor2 String String;
"""

let adtvalue = """
x = Ctor1 0 1;
x
"""

let testcases =
    [ minus
      string
      boolean
      tuple
      simpleprim
      simplefunction
      simplefunctionApplication
      complexfunction
      complexfunctionApplication
      //  partialfunctionApplication;
      partialfunctionApplication2
      ifstmt
      pattern
      patternApplication
      complexpatternApplication
      partialComplexpatternApplication
      booleanpatternApplication
      apply
      tuple
      adt
      adtpattern
      adtvalue ]
