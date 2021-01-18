module Route exposing 
    ( Route(..)
    , fromUrl
    , href
    , replaceUrl
    , pushUrl
    , back
    , AdminRoute(..)
    )

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s, string)
import Url.Parser.Query as Query
import Url.Builder exposing (QueryParameter)



-- ROUTING
type Route
    = Home ( Maybe String )
    | Logout
    | Login
    | Admin AdminRoute
    | About
    | Post String

type AdminRoute 
    = AdminHome (Maybe String)
    | AdminEditPost String
    | AdminNewPost



parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home (Parser.top <?> Query.string "search")
        , Parser.map Logout (s "logout")
        , Parser.map Login (s "login")
        , Parser.map Admin (s "admin" </> adminParser)
        , Parser.map About (s "about")
        , Parser.map Post (s "post" </> string)
        ]

adminParser : Parser (AdminRoute -> a) a
adminParser =
    oneOf
        [ Parser.map AdminHome ( Parser.top <?> Query.string "search" )
        , Parser.map AdminEditPost (s "edit" </> string)
        , Parser.map AdminNewPost (s "new")
        ]


-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)

pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl key route =
    Nav.pushUrl key (routeToString route)

fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url

back : Nav.Key -> Int -> Cmd msg
back key amount =
    Nav.back key amount

-- INTERNAL
routeToString : Route -> String
routeToString page =
    let
        ( path, parameters ) = routeToPieces page
    in
    Url.Builder.absolute path parameters


routeToPieces : Route -> ( List String, List QueryParameter )
routeToPieces page =
    case page of
        Home search ->
            case search of
                Just s ->
                    ( [], [Url.Builder.string "search" s] )
                Nothing ->
                    ([], [])

        Logout ->
            ( [ "logout" ], [] )

        Login ->
            ( [ "login" ], [] )

        Admin adminRoute ->
            case adminRoute of
                AdminHome maybeString ->
                    case maybeString of
                        Just string ->
                            ( ["admin"], [Url.Builder.string "search" string] )
                        Nothing ->
                            (["admin"], [])
                AdminNewPost ->
                    ( ["admin", "new"], [] )
                AdminEditPost postId ->
                    ( ["admin", "edit", postId], [] )

        About ->
            ( [ "about" ], [] )

        Post string ->
            ( [ "post", string ], [] )



