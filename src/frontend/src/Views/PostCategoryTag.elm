module Views.PostCategoryTag exposing (view)
import Data.PostCategory exposing (PostCategory)
import Html exposing (..)
import Html.Attributes exposing (class, style)

view : Int -> Bool -> PostCategory ->  Html msg
view size active category =
    let
        header =
            case size of
                1 ->
                    h1
                2 ->
                    h2
                3 ->
                    h3
                4 ->
                    h4
                5 ->
                    h5
                _ ->
                    h6
    in
    header [class "d-inline m-0"] 
            [ span 
                [ class "badge badge-secondary mr-2 p-2"
                , style "background-color" category.color
                , style "filter" ( if active then "" else "brightness(50%)" )
                ]
                [ text category.category_name ]
            ]
