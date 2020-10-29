module Api exposing (getBlogCategories, getBlogPosts, createPost, PostCategory, Post )
import Html
import Http
import Json.Decode as JD
import Json.Encode as JE


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
    , created :  String
    , updated :  String
    }

-- API and decoders
serverUrl : String
serverUrl = "http://localhost:8000/api/"
-- serverUrl = "https://matiasstorm.com/api/"


get : String -> JD.Decoder a -> ( Result Http.Error a -> msg) -> Cmd msg
get url decoder msg =
    Http.request 
        { method = "GET"
        , url = url 
        , expect = Http.expectJson msg decoder
        , headers = []
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


post : String -> Http.Body -> JD.Decoder a -> ( Result Http.Error a -> msg) -> Cmd msg
post url body decoder msg =
    Http.request 
        { method = "POST"
        , url = url 
        , expect = Http.expectJson msg decoder 
        , headers = []
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }



-- Get requests

getBlogCategories : ( Result Http.Error (List PostCategory) -> msg ) -> Cmd msg
getBlogCategories msg =
    get (serverUrl ++ "post_category/") categoriesDecoder msg



getBlogPosts : ( Result Http.Error (List Post) -> msg ) -> Cmd msg
getBlogPosts msg = 
    get (serverUrl ++ "post/") postsDecoder msg

-- Decoders
categoryDecoder : JD.Decoder PostCategory
categoryDecoder =
    JD.map4 PostCategory
        (JD.field "id" JD.string)
        (JD.field "category_name" JD.string)
        (JD.field "description" JD.string)
        (JD.field "created" JD.string)

categoriesDecoder : JD.Decoder (List PostCategory)
categoriesDecoder =
    JD.list categoryDecoder

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

postsDecoder : JD.Decoder (List Post)
postsDecoder = 
    JD.list postDecoder


-- Post Requests
createPost : Post -> ( Result Http.Error Post -> msg ) -> Cmd msg
createPost blogPost msg =
    let 
        body = newPostEncoder blogPost
    in
    post (serverUrl ++ "post/") body postDecoder msg

-- Encoders
newPostEncoder : Post -> Http.Body
newPostEncoder blogPost =
    JE.object
        [ ("title", JE.string blogPost.title)
        , ("text", JE.string blogPost.text)
        , ("categories", JE.list JE.string blogPost.categories)
        , ("published", JE.bool blogPost.published)
        ]
        |> Http.jsonBody









