module Data.Post exposing 
    ( Post
    , list
    , create
    , get
    , update
    )
import Api exposing (Cred)
import Http
import Api.Endpoint as Endpoint
import Json.Decode as JD
import Json.Encode as JE
import Data.PostCategory as CategoryData


type alias Post =
    { id: String
    , title: String
    , text: String
    , categories : (List CategoryData.PostCategory)
    , serie : Maybe String
    , published : Bool
    , created :  String
    , updated :  String
    }


list : Maybe Cred -> (Result Http.Error (List Post) -> msg) -> Cmd msg
list maybeCred msg =
    Api.get ( Endpoint.post Nothing) maybeCred listDecoder msg

get : String -> Maybe Cred -> (Result Http.Error Post -> msg) -> Cmd msg
get postId maybeCred msg =
    Api.get ( Endpoint.post (Just postId )) maybeCred decoder msg


create : Cred -> (Result Http.Error Post -> msg) -> Post -> Cmd msg
create cred msg post =
    Api.post ( Endpoint.post Nothing) (Just cred) (encoder post) decoder msg


update : Cred -> (Result Http.Error Post -> msg) -> Post -> Cmd msg
update cred msg post =
    let
        endpoint = Endpoint.post ( Just post.id )
        body = encoder post
    in
    Api.put endpoint cred body decoder msg


-- Decoders

decoder : JD.Decoder Post
decoder =
    JD.map8 Post
        (JD.field "id" JD.string)
        (JD.field "title" JD.string)
        (JD.field "text" JD.string)
        (JD.field "categories" CategoryData.listDecoder)
        (JD.field "serie" (JD.nullable JD.string ))
        (JD.field "published" JD.bool)
        (JD.field "created" JD.string)
        (JD.field "updated" JD.string)

listDecoder : JD.Decoder (List Post)
listDecoder = 
    JD.list decoder

-- Encoders

encoder : Post -> Http.Body
encoder post =
    let
        categoryIds = List.map (\c -> c.id) post.categories
    in
    JE.object
        [ ("title", JE.string post.title)
        , ("text", JE.string post.text)
        , ("categories", JE.list JE.string categoryIds)
        , ("published", JE.bool post.published)
        ]
        |> Http.jsonBody


