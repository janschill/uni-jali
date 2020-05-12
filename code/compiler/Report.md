general introduction of functional
what is this whole elm architecture
and little example
model update view cycle
talk about DOM rep (in our language) -> DOM representation
fable-elmish

Chapter 1
Introduction
  General introduction of the model update view cycle
    What is the whole Elm-architecture and little example.
        Describe the diffing and the patching. Describe this is how it works
        for not just Elm, this is implemented in Elm, React, Fable-Elmish,
        etc. In React they have JavaScript, in Elm they have magic JavaScript
        that happens under the hood, but all have this cycle where they diff
        and they patch and ...
  Show plain JavaScript button example maybe
    So the frameworks have this long chain of patching and diffing, while in
    reality, all the javascript they need is this small piece.
  ?Talk about DOM representation in our language ? (Did it go here?)

Chapter 2
The JaLi language
  Grammar
  Features: Abstract Data Types, Lists, rec functions and rec data types etc.
  Special things: untyped etc.
  Semantics: showing interpreter, code snippets
  So in all: it is a standard functional language, but untyped
    Awkwardness in the syntax, compare to FSharp

Chapter 3
Reducer
  What is partial evaluation, what does it do
  Reducer
    What is it
    How does it work
        Reference Peter's book
        (online evaluator)
    Go through code
        How do we reduce each expression, like a function, entire program
    What can do it, what cant
        limit on fold
    Syntax approximation

Chapter 4
Result: JaLi implementation of Elm model
  Give example: button inc/dec
  Show what it looks like in code
  Give all examples, what it suppose to do
  State what everything needs to be called if not reduced
    The intended execution model (and that is why we want to reduce: so we
    don’t need to go through all this)
  runtime, how much in the language itself (almost everything)
  Compile vs. having JaLi code generate JavaScript

Chapter 5
Reducing the Elm model
  Apply reducer, what do we get
  How far away is the plain JavScript button implementation to ours from JaLi
  Yeah