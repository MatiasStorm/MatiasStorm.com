module Api exposing ( getBlogCategories
                    , getBlogPosts
                    , createPost
                    , PostCategory
                    , Post 
                    , JWT
                    , OutMsg(..)
                    , updatePost
                    , login
                    , jwtEncoder
                    , jwtDecoder
                    , refreshToken
                    )
import Html
import Http
import Json.Decode as JD
import Json.Encode as JE
import Browser
import Url.Builder


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

type alias JWT =
    { refresh : String
    , access : String
    }

type OutMsg 
    = Error401


-- API and decoders
serverUrl : String
serverUrl = Url.Builder.absolute [] [] ++ "api/"


get : String -> JD.Decoder a -> ( Result Http.Error a -> msg) -> Maybe JWT -> Cmd msg
get url decoder msg jwt =
    Http.request 
        { method = "GET"
        , url = url 
        , expect = Http.expectJson msg decoder
        , headers = getHeaders jwt
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


post : String -> Http.Body -> JD.Decoder a -> ( Result Http.Error a -> msg) -> Maybe JWT -> Cmd msg
post url body decoder msg jwt =
    Http.request 
        { method = "POST"
        , url = url 
        , expect = Http.expectJson msg decoder 
        , headers = getHeaders jwt
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }

put : String -> Http.Body -> JD.Decoder a -> ( Result Http.Error a -> msg) -> Maybe JWT -> Cmd msg
put url body decoder msg jwt =
    Http.request 
        { method = "PUT"
        , url = url 
        , expect = Http.expectJson msg decoder 
        , headers = getHeaders jwt
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }

getHeaders : Maybe JWT -> List Http.Header
getHeaders jwt =
    let 
        headers : List Http.Header
        headers = []
    in
    case jwt of
        Just actualJwt ->
            Http.header "Authorization" ("JWT " ++ actualJwt.access)
            :: headers

        Nothing ->
            headers

credentials :  JWT -> Http.Header
credentials jwt = Http.header "Authorization" ("JWT " ++ jwt.access)

-- Get requests
getBlogCategories : ( Result Http.Error (List PostCategory) -> msg ) -> Maybe JWT -> Cmd msg
getBlogCategories msg jwt =
    get (serverUrl ++ "post_category/") categoriesDecoder msg jwt 



getBlogPosts : ( Result Http.Error (List Post) -> msg ) -> Maybe JWT-> Cmd msg
getBlogPosts msg jwt = 
    get (serverUrl ++ "post/") postsDecoder msg jwt

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


jwtDecoder : JD.Decoder JWT
jwtDecoder =
    JD.map2 JWT
        ( JD.field "refresh" JD.string )
        ( JD.field "access" JD.string )

-- Post and PUT Requests
createPost : Post -> ( Result Http.Error Post -> msg ) -> Maybe JWT -> Cmd msg
createPost blogPost msg jwt =
    let 
        body = newPostEncoder blogPost
    in
    post (serverUrl ++ "post/") body postDecoder msg jwt

updatePost : Post -> ( Result Http.Error Post -> msg ) -> Maybe JWT -> Cmd msg
updatePost blogPost msg jwt =
    let 
        body = newPostEncoder blogPost
    in
    put (serverUrl ++ "post/" ++ blogPost.id ++ "/") body postDecoder msg jwt

login : {username : String, password : String} -> ( Result Http.Error JWT -> msg ) -> Maybe JWT -> Cmd msg
login {username, password} msg jwt = 
    let
        body = JE.object 
                    [ ("username", JE.string username)
                    , ("password", JE.string password)]
                |> Http.jsonBody
    in
    post (serverUrl ++ "token/obtain/") body jwtDecoder msg jwt

refreshToken : JWT -> (Result Http.Error JWT -> msg) -> Cmd msg
refreshToken jwt msg =
    let 
        body : Http.Body
        body = JE.object 
                    [("refresh", JE.string jwt.refresh)]
                |> Http.jsonBody    
    in
    post (serverUrl ++ "token/refresh/") body jwtDecoder msg Nothing


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

jwtEncoder : JWT -> JE.Value
jwtEncoder jwt =
    JE.object
        [ ("access", JE.string jwt.access) 
        , ("refresh", JE.string jwt.refresh) ]









