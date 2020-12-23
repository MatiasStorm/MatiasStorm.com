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
import Route
import Data.Post as PostData exposing (Post)
import Data.PostCategory as CategoryData exposing (PostCategory)
import Views.PostView as PostView
import Views.StrippedPostView as StrippedPostView
import Data.StrippedPost as StrippedPostData exposing (StrippedPost)
import Html.Attributes exposing (style)


type Msg
    = GotStrippedPosts (Result Http.Error (List StrippedPost))
    | GotPostCategories (Result Http.Error (List PostCategory))
    | GotSession Session
    | GoToPost String

-- Model
type Status
    = Failure
    | Loading
    | Success

type alias Model =
    { posts : (List StrippedPost)
    , status : Status
    , session : Session
    , categoires : (List PostCategory)
    }

initModel : Session -> Model
initModel session =
    { posts = []
    , status = Loading
    , session = session
    , categoires = []
    }

-- Init
init : Session -> (Model, Cmd Msg)
init  session =
    (initModel session
    , Cmd.batch 
        [ StrippedPostData.getCount 5 Nothing GotStrippedPosts
        , CategoryData.list Nothing GotPostCategories
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

        GotStrippedPosts result ->
            case result of
                Ok postList ->
                    ( { model 
                        | posts = postList
                        , status = Success 
                    }, Cmd.none)

                Err _ ->
                    ({model | status = Failure}, Cmd.none)

        GotPostCategories result ->
            case result of
                Ok categoires ->
                    ( { model 
                        | categoires = categoires
                        , status = Success 
                    }, Cmd.none)

                Err _ ->
                    ({model | status = Failure}, Cmd.none)

        GoToPost postId ->
            let
                key = Session.navKey model.session
                route = Route.Post postId
            in
            (model, Route.pushUrl key route)



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
                    div [] [
                        div [class "container"] 
                            (List.map postView model.posts)
                        ]
    in
    { title =  "Home" 
    , content = content
    }

postView : StrippedPost -> Html Msg
postView post =
    div [ onClick ( GoToPost post.id )
        , style "cursor" "pointer"
        ] 
        [StrippedPostView.view post]




