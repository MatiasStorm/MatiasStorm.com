module Page.Home exposing (view, Model, init, Msg, update)
import Html exposing (..)
import Html.Attributes exposing(class, classList)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Http
import Task exposing (Task)
import Views.MarkdownView exposing (renderMarkdown)
import Iso8601
import DateFormat
import Time

type Msg
    = Click
    | GotCategories (Result Http.Error (List PostCategory))
    | GotPosts (Result Http.Error (List Post))

-- Model
type alias PostCategory = 
    { id : String
    , category_name : String
    , description : String
    , created : String
    }

type alias Post =
    { id: String
    , title: String
    , text: String
    , categories : (List String)
    , serie : Maybe String
    , published : Bool
    , created : String
    , updated : String
    }

type Status
    = Failure
    | Loading
    | Success

type alias Model =
    { posts : (List Post)
    , postCategories : (List PostCategory)
    , status : Status
    }

-- Init
init : (Model, Cmd Msg)
init =
    (Model [] [] Loading, Cmd.batch [getBlogCategories, getBlogPosts]  )

-- API and decoders
serverUrl : String
serverUrl = "http://localhost:8000/api/"
-- serverUrl = "https://matiasstorm.com/api/"

getBlogCategories : Cmd Msg
getBlogCategories =
    Http.get
        { url = serverUrl ++ "post_category/"
        , expect = Http.expectJson GotCategories categoriesDecoder}

getBlogPosts : Cmd Msg
getBlogPosts = 
    Http.get
        { url = serverUrl ++ "post/"
        , expect = Http.expectJson GotPosts postsDecoder}

categoriesDecoder : JD.Decoder (List PostCategory)
categoriesDecoder =
    let
        categoryDecoder =
            JD.map4 PostCategory
                (JD.field "id" JD.string)
                (JD.field "category_name" JD.string)
                (JD.field "description" JD.string)
                (JD.field "created" JD.string)
    in
    JD.list categoryDecoder

postsDecoder : JD.Decoder (List Post)
postsDecoder = 
    let
        postDecoder : JD.Decoder Post
        postDecoder =
            JD.map8 Post
                (JD.field "id" JD.string)
                (JD.field "title" JD.string)
                (JD.field "text" JD.string)
                (JD.field "categories" (JD.list JD.string))
                (JD.field "serie" (JD.nullable JD.string ))
                (JD.field "published" JD.bool)
                (JD.field "created" JD.string)
                (JD.field "updated" JD.string)
    in
    JD.list postDecoder

-- Update

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let 
        _ = Debug.log "Message" msg
    in
    case msg of
        Click ->
            ( {model | status = Loading}, Cmd.none )

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

ourFormatter : Time.Zone -> Time.Posix -> String
ourFormatter =
    DateFormat.format
        [ DateFormat.monthNameFull
        , DateFormat.text " "
        , DateFormat.dayOfMonthSuffix
        , DateFormat.text ", "
        , DateFormat.yearNumber
        ]


view : Model -> Html Msg
view model =
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
            div [class "container"] (List.map postView model.posts)

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

