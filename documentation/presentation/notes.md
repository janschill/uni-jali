# Presentation notes

## What we wanted to achieve

In essence we wanted to show that DOM updates can be performed without executing the diff/patch cycle every time an action occurs.
In a web application designed with the MVU pattern, we have this large application to calculate the DOM update whenever an action is performed. In reality, this small plain HTML/JavaScript application achieves the same effect. We want to show that the MVU-application can be transformed into an application very similar to the plain HTML/JavaScript example, without the need for diffing and patching during runtime.
The optimization that we had in mind was a compiler optimisation by partial evaluation. Known frameworks like Elm do not do partial evaluation during their compilation, but the compiler is way too complex to just implement partial evaluation in it and the language itself is also too complex to write a new compiler for it. Therefore, a compiler for a new language, which can make a small example work was just enough.
The language should then be able to implement recursive ADTs, expressions/functions and pattern matching.
The diff and patch functions are part of the MVU pattern. Diff takes two ADTs that represent the view in HTML and produces then an ADT showing the differences between the two views, the patch function then updates the initial view with the changes. These two functions are also needed.
In order to evaluate a program an interpreter should be implemented.
A compiler that transforms the written program into an abstract syntax tree and then partially evaluates it is also needed.
In the next step a compiler that transforms the optimized AST into JavaScript would make the program then runnable in the browser.

## What we have achieved

Implemented a small functional language that can do pattern matching, realize ADTs, functions, recursion. For that we have a lexer and parser.

An interpreter is implemented that takes a program written in our language and can then evaluate it.

In order to realise the MVU pattern in our language we had to define a few specific ADTs and write some functions, like diff and patch. We implemented ADTs that represent the HTML structure that can be used to construct views. An update function that can do the diffing and patching.
And then also tying it all together by generating actual HTML and JavaScript from it.
We wrote a function that would generate HTML from its representation as an ADT, meaning a view could be passed to it and it would generate a string with valid HTML in it.
Also, a patchToJS function that would take the differences—represented by a Differ ADT—between two HTML ADTs (views) and generate JavaScript code showing what to execute in order to make the appropriate change.
This means we have a way of producing a valid HTML view from ADTs and we can generate a JavaScript snippet that makes the change in the DOM.
We do not have a compiler that takes the JaLi program and generates a complete program that can run the MVU cycle.

The reducer is able to reduce a complete JaLi program by partially evaluating and thereby reducing the initial AST drastically.

When using the reducer on the patchToJs function it will give an AST where the exact change has been evaluated and only the missing model needs to be applied. These can be attached to the buttons.

## What we have not achieved/how it would’ve looked like

What is missing, is the final compilation from JaLi to fully working, optimized JavaScript application.
We have shown that partial evaluation can be used to calculate the DOM change by a given action.
It seems that this can be used to generate specialized action handlers for each button. The task from hereon is to figure out how to assemble everything into a fully functioning, optimized JavaScript application.

(We cannot reduce the entire program into and ast, because that would reduce away the model.
What we think we should do is to have the ‘sandbox’ from ELM, where we parse in the view, update and model.
By translating the model into a script tag, we can make the initial mode available in the JavaScript application.
By evaluating the viewToHtml with our interpreter, we would have the HTML view. However, the problem we encounter is how to attach the optimized JavaScript functions to the buttons.

Reducing patchToJs(diff …) for each action, and translating them to JavaScript in the script tag, we would have a JavaScript function for each action that knows where to change the dom. However, these action handlers should not take the model as argument, but should read and update the state model. How to achieve this has yet to be figured out. )


## Backlog

The current state of the project cannot produce a complete browser-runnable program from just writing JaLi and giving it to the compiler.

How would it look like it with everything in place?
A JaLi program would be written as it is already known and done. When writing the program with the model holding the value of the counter in the button example, the reducer needs to know that it does not reduce that value. This could be achieved by setting some rules in the optimizer, for example -- just like in Elm a function could expect the model, view and update and then would not reduce the passed model. Otherwise the reducer would take the intial value of 0 and reduce the whole program, not generating the variable holding the state that is needed.

## Report improvements
