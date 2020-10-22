module Page.Home exposing (view, Model, init, Msg, update)
import Html exposing (..)
import Html.Attributes exposing(class, classList)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Http

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
    , post: String
    , categories : (List String)
    , serie : String
    , published : Bool
    , created : String
    , updated : String
    }

type alias PostModel =
    { posts : (List Post)
    , postCategories : (List PostCategory)
    }

type Model 
    = Failure
    | Loading
    | Success (List PostModel)

-- initialModel : Model
-- initialModel =

-- Init

init : (Model, Cmd Msg)
init =
    (Loading, getBlogCategories)

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
        { url = serverUrl ++ "blog_post/"
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
        postDecoder =
            JD.map7 Post
                (JD.field "id" JD.string)
                (JD.field "post" JD.string)
                (JD.field "categories" (JD.list JD.string))
                (JD.field "serie" JD.string)
                (JD.field "published" JD.bool)
                (JD.field "created" JD.string)
                (JD.field "update" JD.string)
    in
    JD.list postDecoder

-- Update

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click ->
            ( Loading, Cmd.none )

        GotCategories result ->
            case result of 
                Ok categoryList ->
                    (Success categoryList, Cmd.none)

                Err _ ->
                    (Failure, Cmd.none)

        GotPosts result ->
            case result of
                Ok postList ->
                    (Success postList, Cmd.none)

                Err _ ->
                    (Failure, Cmd.none)


-- View

view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "Cannot load categories"

        Loading ->
            text "Loading..."

        Success categoryList ->
            let
                renderCategory : PostCategory -> Html Msg
                renderCategory category =
                    li [] [text category.category_name]
            in
            div [] 
                [ div [] [ button [ onClick Click ] [ text "Click" ] ] 
                , div [] 
                    (List.map renderCategory categoryList)
            ]

