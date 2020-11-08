module Post exposing 
    ( PostCategory
    , Post
    , getPosts
    , getPostCategories
    , createPost
    , updatePost
    )

import Api exposing (get, post, put, Cred)
import Http
import Api.Endpoint as Endpoint
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


getPosts : Maybe Cred -> (Result Http.Error (List Post) -> msg) -> Cmd msg
getPosts maybeCred msg =
    Api.get ( Endpoint.post Nothing) maybeCred postsDecoder msg

getPostCategories : Maybe Cred -> (Result Http.Error (List PostCategory) -> msg) -> Cmd msg
getPostCategories maybeCred msg =
    Api.get Endpoint.postCategory maybeCred categoriesDecoder msg

createPost : Cred -> Post -> (Result Http.Error Post -> msg) -> Cmd msg
createPost cred post msg =
    Api.post ( Endpoint.post Nothing) (Just cred) (postEncoder post) postDecoder msg


updatePost : Cred -> Post -> (Result Http.Error Post -> msg) -> Cmd msg
updatePost cred post msg =
    let
        endpoint = Endpoint.post ( Just post.id )
        body = postEncoder post
    in
    Api.put endpoint cred body postDecoder msg


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

-- Encoders
postEncoder : Post -> Http.Body
postEncoder blogPost =
    JE.object
        [ ("title", JE.string blogPost.title)
        , ("text", JE.string blogPost.text)
        , ("categories", JE.list JE.string blogPost.categories)
        , ("published", JE.bool blogPost.published)
        ]
        |> Http.jsonBody
