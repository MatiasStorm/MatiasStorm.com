module Page exposing (Page(..), view)
import Browser exposing (Document)
import Html exposing (Html, nav, text, footer)


type Page
    = Other
    | Home
    | Blog
    | Admin
    | Login


view : Maybe Viewer -> Page -> {title : String , content: Html msg} -> Document msg
view maybeViewer page {title, content} = 
    { title = title 
    , body = navbarView page maybeViewer :: content :: [ viewFooter ]
    }




navbarView : Page -> Maybe Viewer -> Html msg
navbarView page loggedIn =
    let 
        logo = a [class "navbar-brand", href "/"] [text "matiasStorm"]

        loggedInLinks = 
            if loggedIn then
                [ navBarItem Admin { url = "/admin", caption = "Admin" }
                , li [ class "nav-item" ] 
                    [ a [ class "nav-link", onClick Logout ] 
                        [ text "Logout" ] 
                    ]
                ]
            else 
                [ navBarItem Login { url = "/login", caption = "Login" } ]
        
        links = 
            [ ul [class "navbar-nav", class "mr-auto"] 
                loggedInLinks
                    -- navBarItem Home {url="/", caption="Home" }
                -- , navBarItem Blog { url="/blog", caption="Blog" }
            ]

        navBarItem : Route -> {url : String, caption: String} -> Html Msg
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



isActive : Page -> Route -> Bool
isActive page route = 
    case (page, route) of
        (Home, Route.Home ) -> True
        (Blog, Route.Blog ) -> True
        (Admin, Route.Admin) -> True
        (Login, Route.Login ) -> True
        _ -> False

