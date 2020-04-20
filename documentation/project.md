# Project

## Context

Most reactive frontends use a virtual document object model (VDOM) to achieve content loading without breaking the site (white screen in between the loading of content). This is achieved by traversing the VDOM and comparing it to the old VDOM. When it detects a change it switches out the parts and tells the browser to re-render that particular part in the real DOM. This comparison is expensive and we believe that we can optimize it.

## Outline problem

The problem can be split into two areas:

1. Implement a partial evaluator for a small functional language
2. Use that implementation to investigate the use of partial evaluations to optimize VDOM based web page rendering.

## Outline idea to solve it

Take as example a website with a button that increments a single counter on the screen. We know exactly what part of the DOM should be changed, when this button is pressed. Therefore, it seems inefficient to compute a new VDOM, and traverse the new and old DOM to compare differences. We are going to exploit the fact that often, it is already well known at compile time, what parts of the page should change, when given a user input. We will do this using partial evaluation of functions: the resulting compiled program will consist of functions which, when given their last required input, already know what parts of the DOM to change. To do this, we are going to implement a small functional language, which users can use to define their web-application, and a compiler to construct a partially evaluated program, and transpile it into plain HTML and JavaScript.

```
type Message = Increment | Decrement;

func update model message =
  if message == Increment
  then model + 1
  else model - 1
end
```

In this program we know that the function update can only have two valid inputs, which are also already known during compile time: Increment and Decrement. These variables are called static data. The then called residual program, are all the programs where the static data is precomputed during compiling.
The example program would produce two residual programs:

```
update_increment model = model + 1
update_decrement model = model - 1
```

Which are more efficient during run-time due to them being already evaluated.
