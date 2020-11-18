module Page.Home exposing 
    (view
    , Model
    , init
    , Msg
    , update
    , toSession
    , subscriptions
    )
import Html exposing (..)
import Html.Attributes exposing(class, classList)
import Html.Events exposing (onClick)
import Http
import Views.MarkdownView exposing (renderMarkdown)
import Session exposing (Session)
import Data.Post exposing (getPosts, getPostCategories, Post, PostCategory)
import Views.PostView as PostView


type Msg
    = GotPosts (Result Http.Error (List Post))
    | GotCategories (Result Http.Error (List PostCategory))
    | GotSession Session

-- Model
type Status
    = Failure
    | Loading
    | Success

type alias Model =
    { posts : (List Post)
    , postCategories : (List PostCategory)
    , status : Status
    , session : Session
    }

initModel : Session -> Model
initModel session =
    { posts = []
    , postCategories = []
    , status = Loading
    , session = session
    }

-- Init
init : Session -> (Model, Cmd Msg)
init  session =
    (initModel session
    , Cmd.batch 
        [ getPosts Nothing GotPosts
        , getPostCategories Nothing GotCategories
        ]
    )

toSession : Model -> Session
toSession model =
    model.session

subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)

-- Update

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ( { model | session = session }, Cmd.none)

        GotCategories result ->
            case result of 
                Ok categoryList ->
                    ( { model 
                        | postCategories = categoryList
                        , status = Success 
                    }, Cmd.none)

                Err _ ->
                    ({model | status = Failure}, Cmd.none)

        GotPosts result ->
            case result of
                Ok postList ->
                    ( { model 
                        | posts = postList
                        , status = Success 
                    }, Cmd.none)

                Err _ ->
                    ({model | status = Failure}, Cmd.none)


-- View



view : Model -> {title : String, content : Html Msg}
view model =
    let
        content = 
            case model.status of
                Failure ->
                    text "Cannot load categories"

                Loading ->
                    text "Loading..."

                Success ->
                    let
                        renderCategory : PostCategory -> Html Msg
                        renderCategory category =
                            li [] [text category.category_name]
                    in
                    div [] [
                        div [class "container"] 
                            (List.map ( PostView.view) model.posts)
                        ]
    in
    { title =  "Home" 
    , content = content
    }

