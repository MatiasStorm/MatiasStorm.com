module Data.PostCategory exposing
    ( PostCategory
    , listDecoder
    , encoder
    , list
    )
import Json.Decode as JD
import Json.Encode as JE
import Api exposing (get, post, put, Cred)
import Http
import Api.Endpoint as Endpoint

type alias PostCategory =
    { id : String
    , category_name : String
    , color : String
    }

list : Maybe Cred -> (Result Http.Error (List PostCategory) -> msg) -> Cmd msg
list maybeCred msg =
    Api.get Endpoint.postCategory maybeCred listDecoder msg

-- Decoders

decoder : JD.Decoder PostCategory
decoder =
    JD.map3 PostCategory
        (JD.field "id" JD.string)
        (JD.field "category_name" JD.string)
        (JD.field "color" JD.string)

listDecoder : JD.Decoder (List PostCategory)
listDecoder =
    JD.list decoder

-- Encoders
encoder : PostCategory -> JE.Value
encoder category =
    JE.object 
        [ ("id", JE.string category.id)
        , ("category_name", JE.string category.category_name)
        , ("color", JE.string category.color)
        ]
