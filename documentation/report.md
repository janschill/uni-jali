# Report

## Parser

## Interpreter

## Compiler

With an interpreter in place to evaluate all the introduced expressions, it is a good time to revisit the implementation and apply well-known compiler optimization techniques to shrink the initially parsed AST.
This will be done in a dedicated compiler module, to give structure and have room for more compiler related implementations.


### Optimization

In our optimization we will make use of two concepts: constant folding and partial evaluation.

Constant folding is the technique of evaluating constant expressions during compilation and then applying these at every occurence in the program.

TODO: Explain:
- Partial evaluation
- Constant folding

The reduce function to do the optimization takes initially an expression in form of the AST and returns a reduced version of the same type.
To do a successful reduction a value domain that holds evaluated expressions and differentiates between static and dynamic valuesis needed. Static variables or values are expression that are completely known during compile time and can be evaluated to a specific value, whereas dynamic values cannot be simply reduce, due to a lack of knowledge. For example the parameters of a function will be dynamic at its declaration, because it is unknown with what values the function is called.

The constant folding can be shown by looking at the `Let` and `Variable` cases in the pattern matching.
On the let binding the first expression, the one that is being bound to the name is firstly reduced and in case of a it being a constant it will be put into the store as a static value. That way when the variable is now being used it can be looked up in the value domain and be replaced by an evaluated expression.

Primitive operations are recursively reduced on their two expressions and based on the returned values, either directly applied with its operation or just used.

The following example program with a let binding and a primitive addition operation shall be used to show the two different ASTs that are being generated.

```
x = 1 + 1;
x
```

Non-reduced AST

```
Let ("x",Constant (IntegerValue 1),Variable "x")
```

Reduced AST with constant folding

```
Constant (IntegerValue 1)
```

It has removed the whole binding and even evaluated the operation.

Another interesting part in the optimization is at if statements. Here the condition can be evaluated and based on the outcome maybe even get rid of whole control point branches if they are never used.

The ADT declaration is similiar to a let binding as it will be evaluated, put into the store and then continued with the next expression.
Because the constructors of an ADT are basically functions, they can be put with their closure into the store as static.

As already mentioned that function parameters are dynamic on the declaration, the function body can still be tried to reduce.
The application on functions is a bit more complex.
Firstly ADTClosure will be matched on. All arguments will be reduced, if all arguments are static it can be directly returned as an ADTValue, otherwise it needs to be applied.
With a match on Dynamic, which will happen most of the time, the arguments can now be used to reduce the function body. The store will be updated and continued.
