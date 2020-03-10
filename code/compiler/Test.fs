module Test

open FSharp.Text.Lexing
open Program

let minus = fromString "x = -4\n";;

// let string = fromString "b = 'HELLO'\n";;

let boolean = fromString "c = true\n";;

let touple = fromString "t = (2,3)\n";;

// let record = fromString "x = { name: 'Jan', age: '25', type: monkey, favFruit: pineapple } \n";;

let simplefunction = fromString """
func f x = x + y
""";;

let simplefunction2 = fromString """
func f x =
    x = 2
    x
end
""";;

let complexfunction = fromString """
func f x y z =
  k = x + y * z
  k
end
""";;

let adta = fromString """type DisjointSum = Ctor1 Integer | Ctor2 String String"""

let adtb = fromString """type DisjointSum =
  Ctor1 Integer | Ctor2 String String
""";;

let adtc = fromString """type DisjointSum =
    Ctor1 Integer
  | Ctor2 String String
""";; // not working

let adtd = fromString """type DisjointSum =
   Ctor1 Integer |
   Ctor2 String String
""";;

let adte = fromString """type DisjointSum =
   | Ctor1 Integer
   | Ctor2 String String
""";; // not working

let pattern = fromString """
func f x y = match x b c with
   | Ctor1 => 42
   | Ctor2 => 43
   | Ctor3 => 44
   | _     => 45
end
""";; // not working

let ifstmt = fromString """
if 3>4
then 3
else 4
""";;



// l = [1, 2]
// 2 :: l


// let f = 
//   match x with
//     | Bicycle
//     | 
