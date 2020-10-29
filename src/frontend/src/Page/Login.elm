module Page.Login exposing (view, Model, init, Msg (..), update)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onInput, onClick)
import Http
import Api

type Msg
    = Login
    | Username String
    | Password String
    | LoginRecieved (Result Http.Error Api.JWT)

type alias Model =
    { username: String
    , password : String}

initialModel : Model
initialModel =
    { username = ""
    , password = ""}

init : (Model, Cmd Msg)
init =
    (initialModel, Cmd.none)

view : Model -> Html Msg
view model =
    div []
        [ input 
            [ Attr.type_ "text"
            , Attr.placeholder "Username"
            , Attr.value model.username
            , onInput Username 
            ] []
        , input 
            [ Attr.type_ "password"
            , Attr.placeholder "Password" 
            , Attr.value model.password
            , onInput Password
            ] []
        , button [ onClick Login ] [text "Login"]
        ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login ->
            let
                user = {username = model.username, password = model.password}
            in
            (model, Api.login user LoginRecieved )

        Username username ->
            ( { model | username = username }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )

        LoginRecieved _ ->
            (model, Cmd.none)
