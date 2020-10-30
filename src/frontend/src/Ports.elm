port module Ports exposing (saveJwt)
import Api exposing (JWT)
import Json.Encode as JE


-- Ports
port storeJwt : String -> Cmd msg 


saveJwt : JWT -> Cmd msg
saveJwt jwt =
    Api.jwtEncoder jwt
        |> JE.encode 0
        |> storeJwt
