module Session exposing 
    ( Session
    , changes
    , fromCred
    , navKey
    , cred
    )

import Api exposing (Cred)
import Browser.Navigation as Nav
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Time



-- TYPES


type Session
    = LoggedIn Nav.Key Cred
    | Guest Nav.Key



-- INFO


cred : Session -> Maybe Cred
cred session =
    case session of
        LoggedIn _ val ->
            Just val

        Guest _ ->
            Nothing


navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn key _ ->
            key

        Guest key ->
            key



-- CHANGES


changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    Api.credChanges (\maybeCred -> toMsg (fromCred key maybeCred))


fromCred : Nav.Key -> Maybe Cred -> Session
fromCred key maybeCred =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    case maybeCred of
        Just credentials ->
            LoggedIn key credentials

        Nothing ->
            Guest key
