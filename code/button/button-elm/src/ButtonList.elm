module ButtonList exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

main =
   Browser.sandbox { init = init, update = update, view = view }

type alias Model =
  { list : List Int
  , input : String
  }

init : Model
init =
  Model [] ""

type Msg
  = Add
  | Remove
  | InputNumber String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Add ->
      { list = model.input.toInt :: model.list
      , input = ""
      }

    Remove ->
      { list = model.list.tail
      , input = ""
      }

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Basic list example" ]
    , viewInput "text" "Number to add" model.input InputNumber
    , button [ onClick Remove ] [ text "-" ]
    , div [] [ text (Debug.toString model) ]
    , button [ onClick Add ] [ text "+" ]
    ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []
