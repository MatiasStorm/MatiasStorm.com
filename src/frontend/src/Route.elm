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
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)
import Url.Builder



-- ROUTING
type Route
    = Home
    | Logout
    | Login
    | Admin AdminRoute
    | About
    | Post String

type AdminRoute 
    = AdminHome
    -- | AdminEditPost String
    -- | AdminNewPost


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Logout (s "logout")
        , Parser.map Login (s "login")
        , Parser.map Admin (s "admin" </> adminParser)
        , Parser.map About (s "about")
        , Parser.map Post (s "post" </> string)
        ]

adminParser : Parser (AdminRoute -> a) a
adminParser =
    oneOf
        [ Parser.map AdminHome Parser.top
        -- , Parser.map AdminEditPost (s "edit" </> string)
        -- , Parser.map AdminNewPost (s "new")
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
    Url.Builder.absolute (routeToPieces page) []


routeToPieces : Route -> List String
routeToPieces page =
    case page of
        Home ->
            []

        Logout ->
            [ "logout" ]

        Login ->
            [ "login" ]

        Admin adminRoute ->
            case adminRoute of
                AdminHome ->
                    [ "admin" ]
                -- AdminNewPost ->
                --     ["admin", "new"]
                -- AdminEditPost postId ->
                --     ["admin", "edit", postId]

        About ->
            [ "about" ]

        Post string ->
            [ "post", string ]
