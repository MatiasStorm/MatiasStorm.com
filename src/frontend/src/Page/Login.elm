module Page.Login exposing (view, Model, Msg, init, OutMsg (..), update)
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

type OutMsg
    = LoginSuccess Api.JWT

type alias Model =
    { username: String
    , password : String}

initialModel : Model
initialModel =
    { username = ""
    , password = ""}

init : (Model, Cmd Msg, Maybe OutMsg)
init =
    (initialModel, Cmd.none, Nothing)

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

update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update msg model =
    case msg of
        Login ->
            let
                user = {username = model.username, password = model.password}
            in
            (model, Api.login user LoginRecieved, Nothing )

        Username username ->
            ( { model | username = username }, Cmd.none, Nothing )

        Password password ->
            ( { model | password = password }, Cmd.none, Nothing )

        LoginRecieved result ->
            case result of 
                Ok jwt ->
                    (model, Cmd.none, Just ( LoginSuccess jwt ) )

                Err _ ->
                    (model, Cmd.none, Nothing)





