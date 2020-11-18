module Views.PostView exposing (view)
import Data.Post exposing (Post, PostCategory)
import Iso8601
import DateFormat
import Time
import Html.Attributes exposing (class, style)
import Html exposing (..)
import Views.MarkdownView exposing (renderMarkdown)


ourFormatter : Time.Zone -> Time.Posix -> String
ourFormatter =
    DateFormat.format
        [ DateFormat.monthNameFull
        , DateFormat.text " "
        , DateFormat.dayOfMonthSuffix
        , DateFormat.text ", "
        , DateFormat.yearNumber
        ]

view : Post -> Html msg
view post = 
    let
        formatDate : String -> String
        formatDate date = 
            case Iso8601.toTime date of
                Ok time ->
                    ourFormatter Time.utc time

                Err error->
                    "No created time"
    in
    div [class "card my-3"] 
        [ div [class "card-body"] 
            [ h1 [class "card-title mb-0"] [text post.title] 
            , div [class "my-2"] ( List.map postCategoryView post.categories ) 
            , span [class "text-secondary"] [text ( formatDate post.created)]
            , case (renderMarkdown post.text) of
                Ok rendered ->
                    div [] rendered

                Err errors ->
                    text errors
            ]
        ]

postCategoryView : PostCategory -> Html msg
postCategoryView category =
    h4 [class "d-inline m-0"] 
        [ span [class "badge badge-secondary mr-2 p-2", style "background-color" category.color] 
            [ text category.category_name ]
        ]






