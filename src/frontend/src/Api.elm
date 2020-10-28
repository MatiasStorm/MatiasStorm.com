module Api exposing (getBlogCategories, getBlogPosts, PostCategory, Post )
import Html
import Http
import Json.Decode as JD


type alias PostCategory = 
    { id : String
    , category_name : String
    , description : String
    , created : String
    }

type alias Post =
    { id: Maybe String
    , title: String
    , text: String
    , categories : (List String)
    , serie : Maybe String
    , published : Bool
    , created : Maybe String
    , updated : Maybe String
    }

-- API and decoders
serverUrl : String
serverUrl = "http://localhost:8000/api/"
-- serverUrl = "https://matiasstorm.com/api/"

getBlogCategories : (Result Http.Error (List PostCategory) -> msg) -> Cmd msg
getBlogCategories msg =
    Http.get
        { url = serverUrl ++ "post_category/"
        , expect = Http.expectJson msg categoriesDecoder}

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


getBlogPosts : (Result Http.Error (List Post) -> msg) -> Cmd msg
getBlogPosts msg = 
    Http.get
        { url = serverUrl ++ "post/"
        , expect = Http.expectJson msg postsDecoder}


postsDecoder : JD.Decoder (List Post)
postsDecoder = 
    let
        postDecoder : JD.Decoder Post
        postDecoder =
            JD.map8 Post
                (JD.field "id" ( JD.nullable JD.string ))
                (JD.field "title" JD.string)
                (JD.field "text" JD.string)
                (JD.field "categories" (JD.list JD.string))
                (JD.field "serie" (JD.nullable JD.string ))
                (JD.field "published" JD.bool)
                (JD.field "created" ( JD.nullable JD.string ))
                (JD.field "updated" ( JD.nullable JD.string ))
    in
    JD.list postDecoder
