module Page.Admin exposing (view, Model, init, Msg, update)
import Html exposing (..)
import Html.Attributes as Attr

type Msg
    = Click

type alias Model =
    { content: String }

initialModel : Model
initialModel =
    { content = "Blog" }

init : (Model, Cmd Msg)
init =
    (initialModel, Cmd.none)

view : Model -> Html msg
view model =
    div [ Attr.class "container" ] 
        [ form [] 
            [ div [ Attr.class "form-group" ] 
                [ label [Attr.for "postTitle"] [ text "Post Title" ]
                , input [ Attr.class "form-control"
                        , Attr.type_ "text"
                        , Attr.placeholder "Post Title"
                        , Attr.id "postTitle"
                        ] [] 
                ] 
            , div [ Attr.class "form-group" ] 
                [ label [Attr.for "postText"] [ text "Post Text" ]
                , textarea [ Attr.class "form-control", Attr.id "postText"] [] 
                ] 
            , select [Attr.class "form-control", Attr.multiple True] 
                [ label [Attr.for "postCategories"] [ text "Post Categories" ]
                , option [ Attr.id "postCategories" ] [text "1"] 
                ]
            , div [ Attr.class "form-check" ] 
                [ input [ Attr.class "form-check-input"
                        , Attr.type_ "checkbox" 
                        , Attr.id "postPublished"
                        ] [] 
                , label [ Attr.for "postPublished"
                        , Attr.class "form-check-label"
                        ] [ text "Published" ]
                ]
            , button [ Attr.class "btn btn-primary" ] [ text "Post" ]
            ]
        ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click ->
            ( { model | content = "Clicked" }, Cmd.none )
