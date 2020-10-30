module Main exposing (..)
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Home as Home
import Page.Blog as Blog
import Page.Login as Login
import Page.Admin as Admin
import Url exposing (Url)
import Url.Parser as Parser exposing(Parser)
import Url.Builder
import Http
import Api exposing (JWT)
import Json.Encode as JE
import Json.Decode as JD
import Ports
-- import Route exposing (Route)


-- Route
type Route
    = Home
    | Blog
    | Admin
    | Login


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Blog (Parser.s "blog")
        , Parser.map Admin (Parser.s "admin")
        , Parser.map Login (Parser.s "login")
        ]

navigateToLogin : Nav.Key -> Cmd Msg
navigateToLogin key =
    Url.Builder.absolute ["login"] []
        |> Nav.pushUrl key

-- Page
type Page
    = NotFound
    | HomePage Home.Model
    | BlogPage Blog.Model
    | AdminPage Admin.Model
    | LoginPage Login.Model


-- MODEL
type alias Model = 
    { page: Page
    , key: Nav.Key
    , jwt : Maybe JWT
    }

initModel : Nav.Key -> String -> Model
initModel key flags = 
    { page = NotFound
    , key = key
    , jwt = decodeJwt flags
    }

decodeJwt : String -> Maybe JWT
decodeJwt jwtJson =
    case JD.decodeString Api.jwtDecoder jwtJson of
        Ok jwt -> Just jwt
        Err _ -> Nothing

init : String -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    updateUrl url ( initModel key flags )

updateUrl : Url -> Model -> (Model, Cmd Msg)
updateUrl url model =
    case Parser.parse parser url of
        Just Home ->
            Home.init model.jwt
                |> toHome model

        Just Blog ->
            Blog.init
                |> toBlog model

        Just Admin ->
            case model.jwt of
                Just jwt ->
                    Admin.init model.jwt
                        |> toAdmin model
                Nothing ->
                    ( model
                    , navigateToLogin model.key
                    )

        Just Login ->
            Login.init
                |> toLogin model

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )





-- UPDATE
type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotHomeMsg Home.Msg
    | GotBlogMsg Blog.Msg
    | GotAdminMsg Admin.Msg
    | GotLoginMsg Login.Msg
    | RefreshToken Admin.Msg (Result Http.Error JWT )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let 
        _ = Debug.log "msg" msg
    in
    case msg of
        LinkClicked urlRequest ->
          case urlRequest of
            Browser.Internal url ->
              ( model, Nav.pushUrl model.key (Url.toString url) )

            Browser.External href ->
              ( model, Nav.load href )

        UrlChanged url ->
            updateUrl url model

        GotLoginMsg loginMsg ->
            case model.page of
                LoginPage loginModel ->
                    toLogin model (Login.update loginMsg loginModel)

                _ ->
                    (model, Cmd.none)

        GotHomeMsg homeMsg ->
            case model.page of
                HomePage homeModel ->
                    toHome model (Home.update homeMsg homeModel)
                _ ->
                    (model, Cmd.none)

        GotBlogMsg blogMsg ->
            case model.page of
                BlogPage blogModel ->
                    toBlog model (Blog.update blogMsg blogModel)
                _ ->
                    (model, Cmd.none)

        GotAdminMsg adminMsg ->
            case model.page of
                AdminPage adminModel ->
                    toAdmin model (Admin.update adminMsg adminModel)
                _ ->
                    (model, Cmd.none)

        RefreshToken adminMsg result ->
            case result of
                Ok jwt ->
                    case model.page of
                        AdminPage adminModel ->
                            let
                                (updatedAdminModel, adminCmd, outMsg) = Admin.update adminMsg {adminModel | jwt = Just jwt}
                            in
                            ({model | jwt = Just jwt}, Cmd.batch [Cmd.map GotAdminMsg adminCmd, Ports.saveJwt (Just jwt)] )

                        _ ->
                            (model, Cmd.none)
                Err _ ->
                    (model
                    , Cmd.batch 
                        [ navigateToLogin model.key
                        , Ports.saveJwt Nothing      -- Delete the saved jwt, if refresh doesn't work.
                        ]
                    )


toHome : Model -> (Home.Model, Cmd Home.Msg) -> (Model, Cmd Msg)
toHome model (homeModel, cmd) =
    ( {model | page = HomePage homeModel}
    , Cmd.map GotHomeMsg cmd)

toBlog : Model -> (Blog.Model, Cmd Blog.Msg) -> (Model, Cmd Msg)
toBlog model (blogModel, cmd) =
    ( {model | page = BlogPage blogModel}
    , Cmd.map GotBlogMsg cmd)

toAdmin : Model -> (Admin.Model, Cmd Admin.Msg, Maybe Admin.OutMsg) -> (Model, Cmd Msg)
toAdmin model (adminModel, cmd, outMsg) =
    let 
        redoRequest : Admin.Msg -> Cmd Msg
        redoRequest requestMethod =
            case model.jwt of
                Just jwt ->
                    Api.refreshToken jwt ( RefreshToken requestMethod)

                Nothing ->
                    navigateToLogin model.key
    in
    case outMsg of
        Just ( Admin.FailedRequest requestMethod ) ->
            ( { model | page = AdminPage adminModel } 
            , redoRequest requestMethod
            )

        Nothing ->
            ( {model | page = AdminPage adminModel}
            , Cmd.map GotAdminMsg cmd)

toLogin : Model -> (Login.Model, Cmd Login.Msg, Maybe Login.OutMsg) -> (Model, Cmd Msg)
toLogin model (loginModel, cmd, outMsg) =
    case outMsg of
        Just ( Login.LoginSuccess jwt ) ->
            ({ model | page = LoginPage loginModel
             , jwt = Just jwt
             }
            , Cmd.batch 
                [ Ports.saveJwt ( Just jwt )
                , Nav.back model.key 1
                ]
            )

        Nothing ->
            ( { model | page = LoginPage loginModel }
            , Cmd.map GotLoginMsg cmd
            )


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    let
        handlePageSubscriptions : Sub Msg
        handlePageSubscriptions = 
            case model.page of
                AdminPage adminModel ->
                    Sub.map GotAdminMsg <| Admin.subscriptions adminModel

                _ ->
                    Sub.none
    in
    Sub.batch [handlePageSubscriptions]

-- VIEW
view : Model -> Browser.Document Msg
view model =
    let
        content =
            case model.page of
                HomePage home ->
                    Home.view home |> Html.map GotHomeMsg

                BlogPage blog ->
                    Blog.view blog |> Html.map GotBlogMsg

                AdminPage admin ->
                    Admin.view admin |> Html.map GotAdminMsg

                LoginPage login ->
                    Login.view login |> Html.map GotLoginMsg

                NotFound ->
                    div [] [text "Not found"]

        loggedIn =
            case model.jwt of
                Just jwt -> True
                Nothing -> False
    in
    { title = "URL Interceptor"
    , body =
        [ navbarView model.page loggedIn
        , content
        ]
    }



navbarView : Page -> Bool -> Html msg
navbarView page loggedIn =
    let 
        logo = a [class "navbar-brand", href "/"] [text "matiasStorm"]

        loggedInLinks = 
            if loggedIn then
                [ navBarItem Admin { url = "/admin", caption = "Admin" }
                , navBarItem Login { url = "/login", caption = "Logout" } 
                ]
            else 
                [ navBarItem Login { url = "/login", caption = "Login" } ]
        
        links = 
            [ ul [class "navbar-nav", class "mr-auto"] 
                loggedInLinks
                    -- navBarItem Home {url="/", caption="Home" }
                -- , navBarItem Blog { url="/blog", caption="Blog" }
            ]

        navBarItem : Route -> {url : String, caption: String} -> Html msg
        navBarItem route {url, caption} =
            li [ class "nav-item" ] 
                [ a [ href url
                    , classList 
                        [ ( "nav-link", True )
                        , ("active", isActive { link = route, page = page }) 
                        ] 
                    ] 
                    [ text caption ] 
                ]
    in
    nav [ classList [ ("navbar", True)
                    , ("navbar-expand-sm", True)
                    , ("navbar-light", True)
                    , ("bg-light", True)
                    ] 
        ] 
        [ logo
        , div [class "collapse", class "navbar-collapse"] 
            links 
        ]


isActive : {link : Route, page : Page} -> Bool
isActive {link, page} = 
    case (link, page) of
        (Home, HomePage _ ) -> True
        (Home,  _ ) -> False
        (Blog, BlogPage _ ) -> True
        (Blog, _ ) -> False
        (Admin, AdminPage _ ) -> True
        (Admin, _ ) -> False
        (Login, LoginPage _ ) -> True
        (Login, _ ) -> False


-- MAIN


main : Program String Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }


