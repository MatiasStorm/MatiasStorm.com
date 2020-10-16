module Main exposing (..)
import Browser
import Html exposing (Html, Attribute, button, div, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

main =
  Browser.sandbox { init = init, update = update, view = view }

type Msg = 
    Increment 
    | Decrement 
    | Reset
    | ChangeText String

type alias Model = 
    {
        counter : Int,
        text: String
    }

init : Model
init = 
    {
        counter = 0,
        text = ""
    }

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
        {model | counter = model.counter + 1}

    Decrement ->
        {model | counter = model.counter + 1}

    Reset ->
        {model | counter = 0}

    ChangeText newContent ->
        {model | text = newContent}


view : Model -> Html Msg
view model =
  div [] 
    [ 
        counterView model
        , div [] 
        [
            input [ placeholder "Text to reverse", value model.text, onInput ChangeText] []
            , div [] [text ( String.reverse model.text )]
        ]
    ]

counterView : Model -> Html Msg
counterView model  =
    div []
    [
        button [ onClick Decrement ] [ text "-" ]
        , div [] 
        [ 
            text (String.fromInt model.counter)
            , button [onClick Reset ] [text "Reset"]
        ]
        , button [ onClick Increment ] [ text "+" ]
    ]

