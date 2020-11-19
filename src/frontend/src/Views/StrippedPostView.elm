module Views.StrippedPostView exposing (view)
import Data.StrippedPost exposing (StrippedPost)
import Html exposing (..)
import Html.Attributes exposing (class)
import Utils.DateFormat exposing (formatDate)

view : StrippedPost -> Html msg 
view strippedPost =
    div [class "card my-3"] 
        [ div [class "card-body"] 
            [ h1 [class "card-title mb-0"] [text strippedPost.title] 
            , span [class "text-secondary"] [text ( formatDate strippedPost.created)]
            ]
        ]
