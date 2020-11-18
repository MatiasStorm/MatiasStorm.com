module Page.About exposing 
    ( view
    , toSession
    , update
    , subscriptions
    , init
    , Model
    , Msg
    )
import Html exposing (..)
import Html.Attributes exposing (style)
import Session exposing ( Session )

type alias Model = 
    { session : Session 
    }

type Msg = None

init : Session -> ( Model, Cmd Msg )
init session = ({session = session}, Cmd.none)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

toSession : Model -> Session
toSession model = model.session


view : Model -> {title : String, content : Html msg}
view model = 
    { title = "About"
    , content = h2 [style "text-align" "center"] [text "Under construction"]
    }
