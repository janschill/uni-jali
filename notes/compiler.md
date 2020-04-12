# Compiler

```
TODO:

(specialized) program point:
  - specialized function name (f, vs)
  - f: original function name
  - vs: static parameters of f
  - conditionals can also be pp

value store: holds all evaluated static expressions

value domain {S,D}:
  - S: ordinary values (static results)
  - D: residual expressions & values (dynamic results)

find division: assign variables either static or dynamic

two functions:
  - eval(exp, vs): evaluates static expression
  - reduce(exp, vs): applies constant folding of static parts
                     in dynamic expression

specialization algorithm:
  - pending: set of functions to bet specialized
  - marked: set with already specialized
  - as long as pending, construct version of f,
    specialized to the values vs of its static params
  - add new functions from f‘s body to pending

Should be parsed as AST:
Subject program:
--------
type Msg = Increment | Decrement
model = 1
func update msg model =
  if msg == Increment
  then model + 1
  else model - 1
end
model = update (Increment) (model)
model = update (Decrement) (model)
--------
Specialized/target program:
--------
type Msg = Increment | Decrement
model = 1
func update_increment model =
  model + 1
end
func update_decrement model =
  model - 1
end
model = update_increment (model)
model = update_decrement (model)
--------

poly [
    {update, update_increment model = model + 1}
    {update, update_decrement model = model - 1}
]

(pp, store)
computational state:
  pp:
    - current point of control
  store:
    - current values of all program variables

specialization time:
  - reduce program with given input (not all is given yet)
  - during specialization time (when not all inputs are given)
    we cannot evaluate all expression, due to missing inputs
  - thus store is incomplete
  - static: can be evaluate during specialization time
  - dynamic: cannot be evaluate - "" -

func f x = x + 1
z = 3;

func myFun x y =
  a = x + f (3);
  a
end

func hello x =
  b = 3 + myFun 4
  b
end

myFun (4) (5);
*)

(*
Chat:
12:27:34 From Søren Debois : Let f x y = x + y
12:27:55 From Søren Debois : Let f x  = x + 2
12:28:10 From Søren Debois : Constant 2
12:28:55 From Søren Debois : Rexpr1 = Variable x
12:29:01 From Søren Debois : Rexpr2 = Constant 2
12:29:15 From Søren Debois : Plus (Rexpr1, Rexpr2)
12:35:16 From Søren Debois : Let x = Cons (static, dynamic) in match x with Cons (s, _) -> s + 1 end
12:35:54 From Søren Debois : rexpr
12:36:04 From Søren Debois : x -> Dynamic (rexpr)
12:40:48 From Søren Debois : Match (Cons (static, dynamic) with Cons (s, …) -> s
12:43:40 From Søren Debois : Match (Cons (static, dynamic) with Cons (s, d) -> s


- are we on the right track?
- are we creating several functions for the same functions? E.g. update should be two functions, one for increment, and one for decrement?
- when do we do that? In the apply or the function definition?
- dynamic values?
- Should we follow Sestoft?:
    - Binding-time analysis by abstract interpretation
        (division - mapping function names to a binding-time environment T, which maps variables xj to their
        bindingtime tj, where tj E {S,D})
    - Annotate the program as dynamic/static calls
        (From division to annotation - creating a two-level syntax)
    - Specializing
        (reducing, unfolding based on certain rules)
    - Pre
- confused about env, value store (static), value domain (static and dynamic), list of function versions

Subject program:
--------
type Msg = Increment | Decrement
model = 1
func update msg model =
  if msg == Increment
  then model + 1
  else model - 1
end
model = update (Increment) (model)
model = update (Decrement) (model)
--------
Specialized/target program:
--------
type Msg = Increment | Decrement
model = 1
func update_increment model =
  model + 1
end
func update_decrement model =
  model - 1
end
model = update_increment (model)
model = update_decrement (model)
--------
```
