module Api.Endpoint exposing 
    (Endpoint
    , login
    , request
    , post
    , postCategory
    , StrippedPostQueryParam(..) 
    , strippedPost
    )

import Http
import Url.Builder exposing (QueryParameter)
import Username exposing (Username)
import Url.Builder exposing (string)
import Url.Builder exposing (int)
import Array exposing (Array)


{-| Http.request, except it takes an Endpoint instead of a Url.
-}
request :
    { body : Http.Body
    , expect : Http.Expect msg
    , headers : List Http.Header
    , method : String
    , timeout : Maybe Float
    , url : Endpoint
    , tracker : Maybe String
    }
    -> Cmd msg
request config =
    Http.request
        { body = config.body
        , expect = config.expect
        , headers = config.headers
        , method = config.method
        , timeout = config.timeout
        , url = unwrap config.url
        , tracker = config.tracker
        -- , withCredentials = config.withCredentials
        }



-- TYPES


{-| Get a URL to the Conduit API.

This is not publicly exposed, because we want to make sure the only way to get one of these URLs is from this module.

-}
type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


url : List String -> List QueryParameter -> Endpoint
url paths queryParams =
    -- NOTE: Url.Builder takes care of percent-encoding special URL characters.
    -- See https://package.elm-lang.org/packages/elm/url/latest/Url#percentEncode
    Url.Builder.absolute ("api" :: addEndingSlash paths) queryParams
        |> Endpoint

addEndingSlash : List String -> List String
addEndingSlash paths =
    let
        pathsArray = Array.fromList paths
        length = Array.length pathsArray
        lastElement = Array.get (length - 1) pathsArray
    in
    case lastElement of
        Just str ->
            Array.toList ( Array.set (length - 1) (str ++ "/") pathsArray )
        Nothing ->
            paths




-- ENDPOINTS


login : Endpoint
login =
    url [ "login" ] []

post : Maybe String -> Endpoint
post maybeId =
    case maybeId of
        Just id -> url ["post", id] []
        Nothing -> url ["post"] []

postCategory : Endpoint
postCategory =
    url ["post_category"] []

type StrippedPostQueryParam
    = Count Int
    | Before String 
    | After String 
    | Ascending Bool

strippedPost : List StrippedPostQueryParam -> Endpoint
strippedPost queryParams =
    url ["stripped_post"] (parseStrippedPostQueryParams queryParams)

parseStrippedPostQueryParams: List StrippedPostQueryParam -> List QueryParameter
parseStrippedPostQueryParams queryParams =
    let 
        getOrder maybeAsc =
            case maybeAsc of
                Just asc -> string "asc" "true"
                Nothing -> string "" ""
        parseQueryParam param =
            case param of
                Count count ->
                    int "count" count
                Before date ->
                    string "before" date
                After date ->
                    string "after" date
                Ascending asc ->
                    string "asc" "true"
    in
    List.map (\p -> parseQueryParam p) queryParams
