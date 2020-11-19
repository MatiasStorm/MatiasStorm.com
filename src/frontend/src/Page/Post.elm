module Page.Post exposing (view, toSession, subscriptions, Model, init, Msg, update)
import Html exposing (Html, div, text)
import Session exposing (Session)

type Msg
    = Click

type alias Model =
    { session: Session 
    , postId : String
    }


init : Session -> String -> (Model, Cmd Msg)
init session postId =
    ( 
        { session = session
        , postId = postId
        }
    , Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

toSession : Model -> Session
toSession model = model.session

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click ->
            ( model, Cmd.none )


view : Model -> {title: String, content: Html Msg}
view model =
    { title = "Post"
    , content = div [] [text ( "This is Post " ++ model.postId )]
    }
