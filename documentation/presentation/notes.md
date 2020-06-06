# Presentation notes


## What we wanted to achieve

In essence we wanted to show that DOM updates can be performed without executing the diff/patch cycle every time an action occurs.

The optimization that we had in mind was a compiler optimisation by partial evaluation. Known frameworks like Elm do not do partial evaluation during their compilation, but the compiler is way too complex to just implement partial evaluation in it and the language itself is also too complex to write a new compiler for it. Therefore, a compiler for a new language, which can make a small example work was just enough.
The language should then be able to implement recursive ADTs, expressions/functions and pattern matching. In order to evaluate a program an interpreter should be implemented.
A compiler that transforms the written program into an abstract syntax tree and then partially evaluates it is needed.
In the next step a compiler that transforms the optimized AST into JavaScript would make the program than runnable in the browser.

MVU

## What we achieved

We implemented a language with all the planned functionalities of pattern matching, functions, recursiveness, ADTs etc. It does have some awkwardnesses in the syntax and the overall parser design is probably not optimal and could be improved but has all features needed.

An interpreter that can run the program and evaluate it.





## What we did not achieve

## What we could, and how it would have looked like

The current state of the project cannot produce a complete browser-runnable program from just writing JaLi and giving it to the compiler.

How would it look like it with everything in place?
A JaLi program would be written as it is already known and done. When writing the program with the model holding the value of the counter in the button example, the reducer needs to know that it does not reduce that value. This could be achieved by setting some rules in the optimizer, for example -- just like in Elm a function could expect the model, view and update and then would not reduce the passed model. Otherwise the reducer would take the intial value of 0 and reduce the whole program, not generating the variable holding the state that is needed.

## Report improvements
