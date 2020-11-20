module Data.StrippedPost exposing 
    ( StrippedPost
    , getCountAfter
    , getCountBefore
    , getCount
    )
import Data.PostCategory as CategoryData exposing (PostCategory)
import Http
import Api exposing (Cred)
import Api.Endpoint exposing (StrippedPostQueryParams(..), strippedPost)
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

getCountAfter : Int -> String -> Maybe Cred -> (Result Http.Error (List StrippedPost) -> msg) -> Cmd msg
getCountAfter count after cred msg =
    let
        params = Just ( CountAndAfter count after (Just True) )
    in
    Api.get (strippedPost params ) cred listDecoder msg

getCountBefore : Int -> String -> Maybe Cred -> (Result Http.Error (List StrippedPost) -> msg) -> Cmd msg
getCountBefore count before cred msg =
    let
        params = Just ( CountAndBefore count before Nothing )
    in
    Api.get (strippedPost params ) cred listDecoder msg

getCount : Int -> Maybe Cred -> (Result Http.Error (List StrippedPost) -> msg) -> Cmd msg
getCount count cred msg =
    let
        params = Just ( Count count )
    in
    Api.get (strippedPost params ) cred listDecoder msg

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
