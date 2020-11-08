module Views.PostForm exposing 
    ( Msg
    , OutMsg(..)
    , update
    , initModel
    , Model
    , view
    , getPost
    , subscriptions)
import Post exposing (PostCategory, Post)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onInput, onClick, onCheck)
import Multiselect

type alias Model =
    { post : Post
    , postCategories : List PostCategory
    , postCategoryMultiselectModel : Multiselect.Model
    }

type Msg
    = Title String
    | Text String
    | Categories Multiselect.Msg
    | Published Bool
    | Cancel
    | Submit

type OutMsg 
    = SubmitSend Post
    | CancelSend


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map Categories <| Multiselect.subscriptions model.postCategoryMultiselectModel

-- Update 
update : Msg -> Model -> (Model, Cmd Msg, Maybe OutMsg)
update msg model = 
    let
        _ = Debug.log "model" model
    in
    case msg of
        Title title ->
            let
                updateTitle : Post -> Post
                updateTitle post = { post | title = title }
            in
            ( { model | post = updateTitle model.post }, Cmd.none, Nothing )

        Text text ->
            let
                updateText : Post -> Post
                updateText post = { post | text = text }
            in
            ( { model | post = updateText model.post }, Cmd.none, Nothing )

        Categories multiSelectMsg ->
            let
                (subModel, subCmd, _ ) = Multiselect.update multiSelectMsg model.postCategoryMultiselectModel

                updateCategories : Post -> Post
                updateCategories post =
                    { post | categories = 
                        List.map (\(id, name) -> id) 
                        ( Multiselect.getSelectedValues model.postCategoryMultiselectModel ) 
                    }

            in
            ({ model | postCategoryMultiselectModel = subModel, post = updateCategories model.post }, Cmd.map Categories subCmd, Nothing)

        Published published ->
            let 
                updatePublished : Post -> Post 
                updatePublished post = {post | published = published}
            in
            ( { model | post = updatePublished model.post }, Cmd.none, Nothing)

        Submit ->
            (model, Cmd.none, Just (SubmitSend model.post))

        Cancel ->
            (model, Cmd.none, Just CancelSend)

-- Multiselect
multiSelectModel : Post -> List PostCategory -> Multiselect.Model
multiSelectModel post postCategories =
    let
        model : Multiselect.Model
        model = Multiselect.initModel (multiselectCategories postCategories) "categories"

        multiselectCategories : List PostCategory -> List (String, String)
        multiselectCategories categories = List.map (\c -> (c.id, c.category_name)) categories

        selectedCategories : List (String, String)
        selectedCategories = 
            let 
                filterById : PostCategory -> Bool
                filterById category =
                    List.any ( \i -> category.id == i) post.categories

                filteredCategories : List PostCategory
                filteredCategories =  List.filter filterById postCategories

            in
            multiselectCategories filteredCategories
    in
    Multiselect.populateValues model (multiselectCategories postCategories) selectedCategories


getPost : Model -> Post
getPost model = model.post

initModel : Post -> List PostCategory -> Model 
initModel post postCategories = 
    { post = post
    , postCategories = postCategories
    , postCategoryMultiselectModel = multiSelectModel post postCategories
    }

view : Model -> Html Msg
view model =
    let 
        post = model.post
    in
    div [ Attr.class "col-6" ] 
        [ h2 [] [ text post.title ]
        , div [ Attr.class "form-group" ] 
            [ label [Attr.for "postTitle"] [ text "Post Title" ]
            , input [ Attr.class "form-control"
                    , Attr.type_ "text"
                    , Attr.placeholder "Post Title"
                    , Attr.id "postTitle"
                    , Attr.value post.title
                    , onInput Title
                    ] [] 
            ] 
        , div [ Attr.class "form-group" ] 
            [ label [Attr.for "postText"] [ text "Post Text" ]
            , textarea 
                [ Attr.class "form-control"
                , Attr.style "height" "60vh"
                , Attr.id "postText"
                , Attr.value post.text 
                , onInput Text
                ] [] 
            ] 
        , Html.map Categories <| Multiselect.view model.postCategoryMultiselectModel
        , div [ Attr.class "form-check" ] 
            [ input [ Attr.class "form-check-input"
                    , Attr.type_ "checkbox" 
                    , Attr.id "postPublished"
                    , Attr.checked post.published
                    , onCheck Published
                    ] [] 
            , label [ Attr.for "postPublished"
                    , Attr.class "form-check-label"
                    ] [ text "Published" ]
            ]
        , button [ Attr.class "btn btn-primary", onClick Submit ] [ text "Post" ]
        , button [ Attr.class "btn btn-secondary", onClick Cancel ] [ text "Cancel" ]
        ]
