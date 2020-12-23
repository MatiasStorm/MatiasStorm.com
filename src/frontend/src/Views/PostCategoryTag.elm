module Views.PostCategoryTag exposing (view)
import Data.PostCategory exposing (PostCategory)
import Html exposing (..)
import Html.Attributes exposing (class, style)

view : PostCategory -> Html msg
view category =
    h4 [class "d-inline m-0"] 
        [ span [class "badge badge-secondary mr-2 p-2", style "background-color" category.color] 
            [ text category.category_name ]
        ]
