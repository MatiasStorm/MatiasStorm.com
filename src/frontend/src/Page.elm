module Page exposing (Page(..), view)
import Browser exposing (Document)
import Html exposing (Html, nav, div, text, footer, a, li, ul)
import Html.Attributes exposing(..)
import Html.Events exposing (onClick)
import Route exposing (Route)
import Api exposing (Cred)


type Page
    = Other
    | Home
    | Blog
    | Admin
    | Login


view : Maybe Cred -> Page -> {title : String , content: Html msg} -> Document msg
view cred page {title, content} = 
    { title = title 
    , body = navbarView page cred :: content :: []
    }




navbarView : Page -> Maybe Cred -> Html msg
navbarView page loggedIn =
    let 
        logo = a [class "navbar-brand", href "/"] [text "matiasStorm"]
        
        links = 
            [ ul [class "navbar-nav", class "mr-auto"] 
                [ navBarItem Route.Home {url="/", caption="Home"} 
                , navBarItem Route.Admin {url="/admin", caption="Admin"} 
                ]
            ]

        navBarItem : Route -> {url : String, caption: String} -> Html msg
        navBarItem route {url, caption} =
            li [ class "nav-item" ] 
                [ a [ href url
                    , classList 
                        [ ( "nav-link", True )
                        , ("active", isActive page route) 
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



isActive : Page -> Route -> Bool
isActive page route = 
    case (page, route) of
        (Home, Route.Home ) -> True
        (Admin, Route.Admin) -> True
        -- (Login, Route.Login ) -> True
        _ -> False

