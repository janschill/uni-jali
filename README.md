# uni-project

## Svelte example projects

* [Simple React vs. Svelte to-do app](https://medium.com/javascript-in-plain-english/i-created-the-exact-same-app-in-react-and-svelte-here-are-the-differences-c0bd2cc9b3f8)
* [Simple Markdown app](https://snipcart.com/blog/svelte-js-framework-tutorial)
* [More complex book app](https://blog.logrocket.com/how-to-build-a-simple-svelte-js-app/)

## JaLi

### Syntax

#### Variable declaration

```
my_integer = 1
my_string = "String"
```

#### Operations

```
 5 + 5
10 - 5
 5 * 5
25 / 5
25 % 5
```

#### Types

```
my_integer = 1
my_float   = 1.0
my_boolean = false // true
my_char    = 'c'
my_string  = "String"
my_type    = "MyOwnType"
```

#### Functions

```
func my_function x y z =
  k = x + y + z
  k
end

func my_function x = x
```

#### If statement

```
if a == 1
then expression1
else if a == 2
then expression2
else expression3
```

#### Pattern matching

```
match x with
  | Ctor1 -> expression
  | Ctor2 -> expression
 
```

#### Tuple

```
my_tuple = (1, 2)
my_tuple2 = ((1, 2), 3)
```

#### Algebraic Data Type

```
type DisjointSum = Ctor1 Integer | Ctor2 String String

type DisjointSum =
  Ctor1 Integer | Ctor2 String String

type DisjointSum =
  Ctor1 Integer |
  Ctor2 Float Float |
  Ctor3 String String String
```
