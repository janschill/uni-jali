# Project

## Context

Modern web frameworks that are being used to build reactive frontends are using mostly a virtual document object model (vDOM). Reactive frontends are websites that allow new content to be loaded without breaking the site (white screen in between the loading of content). This is achieved with the help of the vDOM and traversing this vDOM and comparing it to the old vDOM. When it detects a change it switches out the parts and tells the browser to re-render that particular part ii the real DOM. This comparison is expensive and we believe that we can optimize it.
An optimization would mean faster computation of the new view/DOM during run-time. With the idea to improve the compiler and thus the irrelevance of a vDOM, we believe we can have a web framework that has the ability to dynamically update the DOM and is much more efficient than the ones using a vDOM.

## Outline problem

The general problem is that frameworks maintain an in-memory representation of the real DOM of the view that had been rendered. When then an update is executed, the framework needs to compare the old view and the newly generated one by traversing the tree structure. This is slow and inefficient, when the change is minimal and already known, which part will always change on a certain action.

## Closest to solving this problem

## Outline idea to solve it

Because we are going to write our own programming language, we need a compiler, which will be designed with the help of the book “Programming Language Concepts” by Peter Sestoft. The book gives an introduction to the design of functional programming languages and describes all the needed parts to develop one.

Having a compiler allows us to do optimizations like:

* Partial evaluation
* Constant folding
* Symbolic execution

Especially partial evaluation will have a great impact on the optimization of our language.
The idea behind partial evaluation is that often times we can make assumptions about control points in our program even though parts of the input is missing. By doing this specialization new programs are produced that essentially run faster.
Consider this example to be a simple program that renders an integer value, based on a stateful value, that can by pressing the corresponding button either be incremented or decremented.

```
type Message = Increment | Decrement

func update model message =
  if message == Increment
  then model + 1
  else model - 1
end
```

In this program we know that the function update can only have two valid inputs, which are also already know during compile time: Increment and Decrement. These variables are called static data. The then called residual program, are all the program where the static data is precomputed during compiling.
The example program would produced two residual programs:

```
update_increment model = model + 1
update_decrement model = model - 1
```



partial evaluation to optimise update view diff cycle
https://en.wikipedia.org/wiki/Constant_folding
https://en.wikipedia.org/wiki/Partial_evaluation
https://en.wikipedia.org/wiki/Symbolic_execution

