module Api.Endpoint exposing 
    (Endpoint
    , login
    , request
    , post
    , postCategory
    , StrippedPostQueryParams(..) 
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

type StrippedPostQueryParams 
    = Count Int
    | Before String ( Maybe Bool )
    | After String ( Maybe Bool )
    | CountAndBefore Int String ( Maybe Bool )
    | CountAndAfter Int String ( Maybe Bool )

strippedPost : Maybe StrippedPostQueryParams -> Endpoint
strippedPost queryParams =
    url ["stripped_post"] ( strippedPostQueryParams queryParams )

strippedPostQueryParams : Maybe StrippedPostQueryParams -> List QueryParameter
strippedPostQueryParams queryParams =
    let 
        getOrder maybeAsc =
            case maybeAsc of
                Just asc -> string "asc" "true"
                Nothing -> string "" ""
    in
    case queryParams of
        Nothing ->
            []
        Just ( Count count )->
            [int "count" count]
        Just ( Before date maybeAsc) ->
            [string "before" date, getOrder maybeAsc]
        Just ( After date maybeAsc) ->
            [string "after" date, getOrder maybeAsc]
        Just ( CountAndBefore count date maybeAsc) -> 
            [int "count" count, string "before" date, getOrder maybeAsc]
        Just ( CountAndAfter count date maybeAsc) ->
            [int "count" count, string "after" date, getOrder maybeAsc]







