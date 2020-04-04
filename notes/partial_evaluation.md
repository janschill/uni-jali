# Partial evaluation

```
type Msg = Increment | Decrement
func update model msg =
  if msg == Increment
  then model + 1
  else model - 1
end
--------
update_increment model = model + 1
update_decrement model = model - 1

vs/poly? [
    {update, update_increment model = model + 1}
    {update, update_decrement model = model - 1}
]

poly [
    {update, (Increment)}
    {update, (Decrement)}
]
```

Computational State

Denoted as `(pp, store)` where:

* pp
    - current point of control
* store
    - current values of all program variables
    - values of static variables
    - values of dynamic variables are unknown

Specialization Time is the time where optimize/reduce our Abstract Syntax Tree by returning a reduced one, that has been partially evaluated at some states.

Specialized program point (pp, vs) represents a set of states of the subject program's computation.

Poly is called the set of all specialized program points that are reachable.

* reduce program with given input (not all is given yet)
* during specialization time (when not all inputs are given)
    we cannot evaluate all expression, due to missing inputs
* thus store is incomplete
* static: can be evaluate during specialization time
* dynamic: cannot be evaluate - "" -



## Elm button example

```
update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]
```

## Notes from meeting

```
FSharp function
takes two model (old, new)
get value
return value
figure out where to change
compiler optimization

save views along the way
what does update do when given Increment
symbolic execution/constant folding/partial evaluation (update)

compare_vdom : Value -> Value -> Value
old_view : Value
compare_vdom (old_view) (view model)
model0, model1, model2, ...
compare_vdom (view model2) (view model3)
compare_vdom (view model3) (view model4)
compare_vdom (view (update Increment model2))
compare_vdom (view3) (view (update Increment model3))
update Increment model?
compare_vdom (view3) (view (update Increment model3))
“update Increment model” -> “update model”
“update Decrement model” -> “update model”
Update msg model
Update model = model + 1
```

Turn function of two arguments into function with one argument

Write two programs:
pI, pD: that uses then `update model` where the result is already computed and only on runtime it is decided what branch to take, depending on the then partially applied function


model = 2

view model =
  <div>
    model
  </div>

update msg model =
  msg:
    I -> model + 1
    D -> model - 1

old_view = view (update I model)
new_view = view (update I model)

compare :: Value -> Value -> Value
compare old_view new_view =

## Notes from book

__Book: Partial Evaluation by Peter Sestoft__

Partial application or currying is the specialization of a two-argument function to a one-argument function by returning a function with an argument.

A partial evaluation is the same concept but in program context:
Given some input data it produces a reduced residual program, with given inputs applied.

A partial evalutor is given some (static) input and a subject program that constructs a new program with the input applied, when given the remaining input (dynamic) it yields the same result that the subject program would have produced given both inputs.

Three partial evaluation techniques are well known:

1. Symbolic evaluation
2. Unfolding (function calls)
3. Program point specialization

### Equational definition

```
out = [[p]] [in1, in2]

p_in1 = [[mix]] [p, in1]
out = [[p_in1]] in2

[[p]] [in1, in2] = [[ [[mix]] [p, in1] ]] in2
```
