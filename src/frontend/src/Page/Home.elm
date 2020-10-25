module Page.Home exposing (view, Model, init, Msg, update)
import Html exposing (..)
import Html.Attributes exposing(class, classList)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Http
import Task exposing (Task)
import Markdown.Parser as Markdown
import Markdown.Renderer
import Iso8601

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
        getCreatedDate = 
            (Iso8601.toTime post.created)

    in
    div [class "card my-3"] 
        [ div [class "card-body"] 
            [ h1 [class "card-title"] [text post.title] 
            , span [] [text  getCreatedDate]
            , case 
                post.text
                    |> Markdown.parse
                    |> Result.mapError deadEndsToString
                    |> Result.andThen (\ast -> Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer ast)
              of
                Ok rendered ->
                    div [] rendered

                Err errors ->
                    text errors
            ]
        ]

deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.deadEndToString
        |> String.join "\n"
