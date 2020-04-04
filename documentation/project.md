# Project

## Context

Modern web frameworks that are being used to build reactive frontends are using mostly a virtual document object model (vDOM). Reactive frontends are websites that allow new content to be loaded without breaking the site (white screen in between the loading of content). This is achieved with the help of the vDOM and traversing this vDOM and comparing it to the old vDOM. When it detects a change it switches out the parts and tells the browser to re-render that particular part if the real DOM. This comparison is expensive and we believe that we can optimize it.

We are going to look at different frameworks, like:

* React
* Elm

We will heavily look into Elm, because it is doing a lot that we intend to do, but does not have the optimization part which we want to develop.
Because we are going to write our own programming language, we need a compiler, which will be designed with the help of the book “Programming Language Concepts” by Peter Sestoft. The book gives an introduction to the design of functional programming languages and describes all the needed parts to develop one.

## Outline problem

## Closest to solving this problem

## Outline idea to solve it


partial evaluation to optimise update view diff cycle
https://en.wikipedia.org/wiki/Constant_folding
https://en.wikipedia.org/wiki/Partial_evaluation
https://en.wikipedia.org/wiki/Symbolic_execution

