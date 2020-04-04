# Partial evaluation

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

