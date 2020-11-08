module Viewer exposing (Viewer, avatar, cred, decoder, minPasswordChars, store, username)

{-| The logged-in user currently viewing this page. It stores enough data to
be able to render the menu bar (username and avatar), along with Cred so it's
impossible to have a Viewer if you aren't logged in.
-}

import Api exposing (Cred)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)



-- TYPES


type Viewer
    = Viewer Avatar Cred



-- INFO


cred : Viewer -> Cred
cred (Viewer _ val) =
    val



{-| Passwords must be at least this many characters long!
-}
minPasswordChars : Int
minPasswordChars =
    6



-- SERIALIZATION


decoder : Decoder (Cred -> Viewer)
decoder =
    Decode.succeed Viewer
        |> custom (Decode.field "image" Avatar.decoder)


store : Viewer -> Cmd msg
store (Viewer avatarVal credVal) =
    Api.storeCredWith
        credVal
        avatarVal
