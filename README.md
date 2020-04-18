# uni-project

## Getting started

Before executing any commands from the Makefile, make sure that you have a FSharp installation and it is set in your enviroment variables.

```
export FSHARP_DIR=.fsharp
```

## Tests

To run the tests simply execute

```bash
$ make test
# or $ bin/test.sh
```

This will run the interpreter on all the `.jali` files in the `tests/` directory and compare it to the `.out` file.

```bash
$ make test
bin/test.sh
---------------------------------
Running tests from: bin/../tests
Test binding-boolean passed
Test binding-negative_integer passed
Test binding-string passed
Test binding-tuple passed
Test conditional-if_then_else passed
Test conditional-pattern-tuple_match passed
Test conditional-pattern-tuple_match_boolean passed
Test conditional-pattern passed
Test function-1_param passed
Test function-2_param passed
Test function-3_param passed
Test logical-and passed
Test logical-and2 passed
Test logical-and3 passed
Test logical-and4 passed
Test logical-or passed
Test logical-or2 passed
Test logical-or3 passed
Test logical-or4 passed
Test primitive-operation-addition passed
=================================
Total: 20 – 20 passed – 0 failed
```

## Svelte example projects

* [Simple React vs. Svelte to-do app](https://medium.com/javascript-in-plain-english/i-created-the-exact-same-app-in-react-and-svelte-here-are-the-differences-c0bd2cc9b3f8)
* [Simple Markdown app](https://snipcart.com/blog/svelte-js-framework-tutorial)
* [More complex book app](https://blog.logrocket.com/how-to-build-a-simple-svelte-js-app/)

## JaLi

### Syntax

```
myInteger = 1;
myString = "String";


 5 + 5
10 - 5
 5 * 5
25 / 5

func myFunction x y z =
  k = x + y + z;
  k
end

if a == 1
then expression1
else if a == 2
then expression2
else expression3

match x with
    Ctor1 -> expression
  | Ctor2 -> expression

myTuple = (1, 2);
myTuple2 = ((1, 2), 3);

type DisjointSum = Ctor1 Integer | Ctor2 String String;

type DisjointSum =
  Ctor1 Integer | Ctor2 String String;

type DisjointSum =
    Ctor1 Integer
  | Ctor2 Integer Integer
  | Ctor3 String String String
```
