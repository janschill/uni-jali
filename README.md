# uni-project

## Svelte example projects

* [Simple React vs. Svelte to-do app](https://medium.com/javascript-in-plain-english/i-created-the-exact-same-app-in-react-and-svelte-here-are-the-differences-c0bd2cc9b3f8)
* [Simple Markdown app](https://snipcart.com/blog/svelte-js-framework-tutorial)
* [More complex book app](https://blog.logrocket.com/how-to-build-a-simple-svelte-js-app/)

## JaLi

### Tests

To run the tests simply execute

```bash
$ make test
# or $ bin/test.sh
```

This will run the interpreter on all the `.jali` files in the `tests/` directory and compare it to the `.out` file.

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
