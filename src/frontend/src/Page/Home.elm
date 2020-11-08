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
import Iso8601
import DateFormat
import Time
import Session exposing (Session)
import Post exposing (getPosts, Post, PostCategory)


import SyntaxHighlight as SH


type Msg
    = GotPosts (Result Http.Error (List Post))
    -- | GotCategories (Result Http.Error (List PostCategory))

-- Model
type Status
    = Failure
    | Loading
    | Success

type alias Model =
    { posts : (List Post)
    -- , postCategories : (List PostCategory)
    , status : Status
    , session : Session
    }

initModel : Session -> Model
initModel session =
    { posts = []
    , status = Loading
    , session = session}

-- Init
init : Session -> (Model, Cmd Msg)
init  session =
    (initModel session, Cmd.batch [getPosts Nothing GotPosts])
    -- (Model [] [] Loading jwt, Cmd.batch 
    --                         [ getBlogCategories GotCategories jwt 
    --                         , getBlogPosts GotPosts jwt
    --                         ]  
    -- )

toSession : Model -> Session
toSession model =
    model.session

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
    -- Session.changes GotSession (Session.navKey model.session)

-- Update

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- GotCategories result ->
        --     case result of 
        --         Ok categoryList ->
        --             ( { model 
        --                 | postCategories = categoryList
        --                 , status = Success 
        --             }, Cmd.none)

        --         Err _ ->
        --             ({model | status = Failure}, Cmd.none)

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

ourFormatter : Time.Zone -> Time.Posix -> String
ourFormatter =
    DateFormat.format
        [ DateFormat.monthNameFull
        , DateFormat.text " "
        , DateFormat.dayOfMonthSuffix
        , DateFormat.text ", "
        , DateFormat.yearNumber
        ]


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
                        div [class "container"] (List.map postView model.posts)
                        ]
    in
    { title =  "Home" 
    , content = content
    }

postView : Post -> Html Msg
postView post =
    let
        formatDate : String -> String
        formatDate date = 
            case Iso8601.toTime date of
                Ok time ->
                    ourFormatter Time.utc time

                Err error->
                    "No created time"
    in

    div [class "card my-3"] 
        [ div [class "card-body"] 
            [ h1 [class "card-title mb-0"] [text post.title] 
            , span [class "text-secondary"] [text ( formatDate post.created)]
            , case (renderMarkdown post.text) of
                Ok rendered ->
                    div [] rendered

                Err errors ->
                    text errors
            ]
        ]

