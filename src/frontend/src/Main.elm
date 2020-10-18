module Main exposing(..)
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url exposing (Url)
import Url.Parser as Parser exposing(Parser)


-- Route

type Route
    = NotFound
    | Home
    | Blog

matchRoute : Parser (Route -> a) a
matchRoute =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Blog (Parser.s "blog")
        ]

parseUrl : Url -> Route
parseUrl url =
    case Parser.parse matchRoute url of
        Just route ->
            route
        Nothing ->
            NotFound


-- MODEL


type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , page : Route
  }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model key url NotFound, Cmd.none )



-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( {model | page = parseUrl url}, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW
viewer : Route -> Html msg
viewer page =
    case page of
        NotFound ->
            div [] [ text "Not found" ]
        Home ->
            homeView
        Blog ->
            blogView

view : Model -> Browser.Document Msg
view model =

  { title = "URL Interceptor"
  , body =
      [ navbarView 
      , text "The current URL is: "
      , viewer model.page
      , ul []
          [ viewLink "/"
          , viewLink "/blog"
          , viewLink "/NotFound"
          ]
      ]
  }


viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]


homeView : Html msg
homeView =
    div [] [text "This is home"]

blogView : Html msg
blogView =
    div [] [text "This is Blog"]

navBarItem : String -> String -> Html msg
navBarItem path name =
    li [ class "nav-item" ] [ a [ href path, class "nav-link" ] [ text name ] ]


navbarView : Html msg
navbarView =
    nav [ classList [ ("navbar", True)
                    , ("navbar-expand-sm", True)
                    , ("navbar-light", True)
                    , ("bg-light", True)
                    ] 
        ] 
        [ a [class "navbar-brand", href "/"] [text "Navbar"]
        , div [class "collapse", class "navbar-collapse"] 
            [ ul [class "navbar-nav" class "mr-auto"] 
                [ navBarItem "/" "Home"
                , navBarItem "/blog" "Blog"
                ]
            ]
        ]


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


