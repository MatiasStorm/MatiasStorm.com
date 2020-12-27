module Data.StrippedPost exposing 
    ( StrippedPost
    , count
    , before
    , after
    , ascending
    , get
    , categoryIds
    )
import Data.PostCategory as CategoryData exposing (PostCategory)
import Http
import Api exposing (Cred)
import Api.Endpoint exposing (StrippedPostQueryParam(..), strippedPost)
import Json.Decode as JD
import Json.Encode as JE


type alias StrippedPost =
    { id: String
    , title: String
    , categories : (List PostCategory)
    , serie : Maybe String
    , published : Bool
    , created :  String
    }

count : Int -> List StrippedPostQueryParam -> List StrippedPostQueryParam
count c params = Count c :: params

before : String -> List StrippedPostQueryParam -> List StrippedPostQueryParam
before date params = Before date :: params

after : String -> List StrippedPostQueryParam -> List StrippedPostQueryParam
after date params = After date :: params

ascending : Bool -> List StrippedPostQueryParam -> List StrippedPostQueryParam
ascending b params = Ascending b :: params

categoryIds : List String -> List StrippedPostQueryParam -> List StrippedPostQueryParam
categoryIds ids params = (List.map (\i -> CategoryId i) ids) ++ params

get : List StrippedPostQueryParam -> Maybe Cred -> (Result Http.Error (List StrippedPost) -> msg) -> Cmd msg
get params cred msg =
    let
        endpoint = strippedPost params
    in
    Api.get endpoint cred listDecoder msg


decoder : JD.Decoder StrippedPost
decoder =
    JD.map6 StrippedPost
        (JD.field "id" JD.string)
        (JD.field "title" JD.string)
        (JD.field "categories" CategoryData.listDecoder)
        (JD.field "serie" (JD.nullable JD.string ))
        (JD.field "published" JD.bool)
        (JD.field "created" JD.string)

listDecoder : JD.Decoder (List StrippedPost)
listDecoder = 
    JD.list decoder
