module Page.Login exposing 
    ( view
    , Model
    , Msg
    , init
    , subscriptions
    , update
    , toSession
    )
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onInput, onClick)
import Http
import Api exposing (Cred)
import Route
import Session exposing (Session)
import Json.Encode as Encode

type Msg
    = Login
    | Username String
    | Password String
    | LoginRecieved (Result Http.Error Cred)
    | GotSession Session

type alias Model =
    { username: String
    , password : String
    , session : Session
    }

initialModel : Session -> Model
initialModel session =
    { username = ""
    , password = ""
    , session = session
    }

init : Session -> (Model, Cmd Msg)
init session =
    (initialModel session, Cmd.none)

view : Model -> { title : String, content : Html Msg }
view model =
    let
        content = 
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
    in
    { title = "Login"
    , content = content
    }

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        Login ->
            let
                user = {username = model.username, password = model.password}
            in
            (model, login model LoginRecieved )

        Username username ->
            ( { model | username = username }, Cmd.none)

        Password password ->
            ( { model | password = password }, Cmd.none)

        LoginRecieved result ->
            case result of 
                Ok cred ->
                    (model, Api.storeCred cred )

                Err _ ->
                    (model, Cmd.none)

        GotSession session ->
            ( { model | session = session }
            , Route.back (Session.navKey session) 1
            )


-- HTTP
login : Model -> ( Result Http.Error Cred -> Msg ) -> Cmd Msg
login model msg =
    let
        body =
            Encode.object 
                [ ( "username", Encode.string model.username )
                , ("password", Encode.string model.password)
                ]
                |> Http.jsonBody
    in
    Api.login body msg


toSession : Model -> Session
toSession model =
    model.session

subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)

