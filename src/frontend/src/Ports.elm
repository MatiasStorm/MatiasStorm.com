port module Ports exposing (saveJwt)
import Api exposing (JWT)
import Json.Encode as JE


-- Ports
port storeJwt : String -> Cmd msg 


saveJwt : Maybe JWT -> Cmd msg
saveJwt jwt =
    case jwt of
        Just actualJwt ->
            Api.jwtEncoder actualJwt
                |> JE.encode 0
                |> storeJwt
                 
        Nothing ->
            storeJwt ""
