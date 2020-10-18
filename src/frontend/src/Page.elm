module Page exposing (Page(..), view)
import Browser exposing (Document)
import Html exposing (Html, nav, text, footer)

type Page 
    = NotFoundPage
    | Home
    | Blog


viewHeader : Page -> Html msg
viewHeader page =
    nav [] [text "This is the navbar"]


viewFooter : Html msg
viewFooter =
    footer [] [text "Im the footer"]


view : Page -> {title: String, content: Html msg} -> Document msg
view page {title, content} =
    {title = title ++ " - Matias Storm"
    , body = viewHeader page :: content :: [viewFooter]
    }


