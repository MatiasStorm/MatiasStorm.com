module Views.PostView exposing (view)
import Data.Post exposing (Post)
import Data.PostCategory exposing (PostCategory)
import Html.Attributes exposing (class, style)
import Html exposing (..)
import Views.MarkdownView exposing (renderMarkdown)
import Utils.DateFormat exposing (formatDate)
import Views.PostCategoryTag as TagView



view : Post -> Html msg
view post = 
    div [class "card my-3"] 
        [ div [class "card-body"] 
            [ h1 [class "card-title mb-0"] [text post.title] 
            , div [class "my-2"] ( List.map TagView.view post.categories ) 
            , span [class "text-secondary"] [text ( formatDate post.created)]
            , case (renderMarkdown post.text) of
                Ok rendered ->
                    div [] rendered

                Err errors ->
                    text errors
            ]
        ]







