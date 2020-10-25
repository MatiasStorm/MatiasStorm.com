module Main exposing(..)
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Home as Home
import Page.Blog as Blog
import Url exposing (Url)
import Url.Parser as Parser exposing(Parser)
-- import Route exposing (Route)


-- Route
type Route
    = Home
    | Blog


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Blog (Parser.s "blog")
        ]


-- Page
type Page
    = NotFound
    | HomePage Home.Model
    | BlogPage Blog.Model



-- MODEL
type alias Model = {page: Page, key: Nav.Key}


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    updateUrl url { page = NotFound, key = key }


updateUrl : Url -> Model -> (Model, Cmd Msg)
updateUrl url model =
    case Parser.parse parser url of
        Just Home ->
            Home.init 
                |> toHome model

        Just Blog ->
            Blog.init
                |> toBlog model

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )



-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | GotHomeMsg Home.Msg
  | GotBlogMsg Blog.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
        updateUrl url model

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

toHome : Model -> (Home.Model, Cmd Home.Msg) -> (Model, Cmd Msg)
toHome model (homeModel, cmd) =
    ( {model | page = HomePage homeModel}
    , Cmd.map GotHomeMsg cmd)

toBlog : Model -> (Blog.Model, Cmd Blog.Msg) -> (Model, Cmd Msg)
toBlog model (blogModel, cmd) =
    ( {model | page = BlogPage blogModel}
    , Cmd.map GotBlogMsg cmd)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none




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

                NotFound ->
                    div [] [text "Not found"]
    in
    { title = "URL Interceptor"
    , body =
        [ navbarView model.page 
        , content
        ]
    }



navbarView : Page -> Html msg
navbarView page =
    let 
        logo = a [class "navbar-brand", href "/"] [text "matiasStorm"]
        
        links = 
            [ ul [class "navbar-nav", class "mr-auto"] 
                [ 
                    -- navBarItem Home {url="/", caption="Home" }
                -- , navBarItem Blog { url="/blog", caption="Blog" }
                ]
            ]

        navBarItem : Route -> {url : String, caption: String} -> Html msg
        navBarItem route {url, caption} =
            li [ class "nav-item" ] 
                [ a [ href url, classList [ ( "nav-link", True ), 
                                            ("active", isActive { link = route, page = page }) 
                                        ] 
                    ] 
                    [ text caption ] ]
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


-- MAIN


main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }


