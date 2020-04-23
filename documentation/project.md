# Project

## Context

A popular approach among modern web frameworks for building reactive frontends is to use a a virtual document object model (vDOM) to achieve content loading without breaking the site (white screen in between the loading of content).

The approach works as follows: In response to user interactions, the client program generates first an update to an internal model, representing the state of the site, and then a representation of the entire DOM from this model. This representation is called the "Virtual DOM" or "vDOM". The vDOM is traversed and compared to the old vDOM. Whenever it detects a change it switches out the parts in the real DOM and tells the browser to re-render that particular part.

Some well-known frameworks using this approach are Elm, React and Vue, to name a few examples.

## Outline problem

We will attempt to optimise away (a) the generation of intermediary DOM representation and (b) diffing of intermediary and actual DOM by using techniques from partial evaluation. The problem can be split into two areas:
1. Implementing a partial evaluator for a small functional language
2. Use that implementation to investigate the use of partial evaluations to optimize VDOM based web page rendering.

## Outline idea to solve it

Take as example a website with a button that increments a single counter on the screen. We know exactly what part of the DOM should be changed, when this button is pressed. Therefore, it seems inefficient to compute a new VDOM, and traverse the new and old DOM to compare differences. We are going to exploit the fact that often, it is already well known at compile time, what parts of the page should change, when given a user input.

In an Elm-like model-view-update, we have two functions 'update' and 'view' and then an implicitly stated 'diff' functions:

update: Action -> Model -> Model
view: Model -> View
diff: View -> View -> DOM-Update-Actions

Having a previous view, and a user event that triggers som action a on the model m, the functions are applied as follows:

diff previous_view (view (update a m))

Assume we have a statically known action, like 'Increment' from the example above which increments m by 1, and m is a dynamic model. Then the call to view is a good candidate for partial evaluation:

view (update Increment m) => (view (m+1))

The same applies for the call to diff, provided we can find a good way to make “previous_view” static enough. Thus partial evaluation can give us a direct DOM-Update-Action at compile time.