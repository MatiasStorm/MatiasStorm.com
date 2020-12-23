module Views.StrippedPostView exposing (view)
import Data.StrippedPost exposing (StrippedPost)
import Html exposing (..)
import Html.Attributes exposing (class)
import Utils.DateFormat exposing (formatDate)
import Views.PostCategoryTag as TagView

view : StrippedPost -> Html msg 
view strippedPost =
    div [class "card my-3"] 
        [ div [class "card-body"] 
            [ h1 [class "card-title mb-0"] [text strippedPost.title] 
            , div [class "my-2"] ( List.map TagView.view strippedPost.categories ) 
            , span [class "text-secondary"] [text ( formatDate strippedPost.created)]
            ]
        ]
