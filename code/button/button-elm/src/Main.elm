module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, Attribute, div, button, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  {
    num: Int,
    history: List Int,
    message: String
  }


init : Model
init =
  { num = 0
  , history = []
  , message = ""
  }



-- UPDATE


type Msg
  = Increment
  | Decrement
  | Jump
  | Reset
  | Change String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      { model | num = model.num + 1, history = model.num :: model.history }
    Decrement ->
      { model | num = model.num - 1, history = model.num :: model.history }
    Jump ->
      { model | num = model.num + 10, history = model.num :: model.history}
    Reset ->
      { model | num = 0, history = [] }
    Change newMsg ->   -- update function taking an argument
      { model | message = newMsg }





-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ text (String.fromInt model.num)

    , div []
      [ button [ onClick Decrement ] [ text "-" ]
      , button [ onClick Increment ] [ text "+" ]
      ]

    , div [] -- 'div' and 'button' are actually just elm functions, so we can create our own, fx. this 'clickButton':
      [ clickButton Jump "Jump"
      , clickButton Reset "Reset"
      ]

    , div [] [ text (Debug.toString model.history)  ]

    , div []
      [ input [ placeholder "Write message", value model.message, onInput Change ] []
      , viewValidation model
      ]
    ]

  -- two view helper functions for cleaner view code:
clickButton : msg -> String -> Html msg
clickButton msg txt =
  button [ onClick msg] [text txt]


  -- cool helper taking Model as arg. and returning div element
viewValidation : Model -> Html msg
viewValidation model =
  if String.length model.message == 0 then
    div [] []
  else if String.length model.message >= 8 then
    div [ style "color" "green" ] [ text "OK" ]
  else
    div [ style "color" "red" ] [ text "Message too short" ]

