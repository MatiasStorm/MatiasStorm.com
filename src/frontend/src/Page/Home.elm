module Page.Home exposing (view, Model, init, Msg, update)
import Html exposing (Html, div, text)

type Msg
    = Click

type alias Model = 
    { content : String }

initialModel : Model
initialModel =
    {content = "HOME"}

init : (Model, Cmd Msg)
init =
    (initialModel, Cmd.none)

view : Model -> Html msg
view model =
    div [] [text "This is home"]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click ->
            ( { model | content = "Clicked" }, Cmd.none )
