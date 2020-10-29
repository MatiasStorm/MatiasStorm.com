module Page.Admin exposing (view, Model, init, Msg, update, subscriptions)
import Html exposing (..)
import Api exposing (PostCategory, Post, getBlogCategories, getBlogPosts)
import Html.Attributes as Attr
import Http
import Html.Events exposing (onClick)
import Html.Events exposing (onInput)
import Html.Events exposing (onCheck)
import Multiselect

type Msg
    = GotPosts (Result Http.Error (List Post))
    | GotPost (Result Http.Error Post)
    | GotCategories (Result Http.Error (List PostCategory))
    | SetShowNewPost Bool
    | SetEditingPost Post
    | SetShowEditingPost Bool
    | Title String
    | Text String
    | Categories Multiselect.Msg
    | Published Bool
    | CreatePost
    | UpdatePost


-- Model
type Status
    = Failure
    | Loading
    | Success

-- Update
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let 
        _ = Debug.log "Message" model

    in
    case msg of
        GotCategories result ->
            case result of 
                Ok categoryList ->
                    ( { model 
                        | postCategories = categoryList
                        , status = Success 
                        , postCategoryMultiselectModel = Multiselect.initModel (multiselectCategories categoryList) "categories"
                    }, Cmd.none)

                Err _ ->
                    ({model | status = Failure}, Cmd.none)

        GotPosts result ->
            case result of
                Ok postList ->
                    ( { model 
                        | posts = postList
                        , status = Success 
                    }, Cmd.none)

                Err _ ->
                    ({model | status = Failure}, Cmd.none)

        GotPost result ->
            let 
                updatedPosts : Post -> List Post
                updatedPosts post =
                    post :: (List.filter (\p -> p.id /= post.id ) model.posts )
            in
            case result of
                Ok post ->
                    ( { model 
                        | posts = updatedPosts post
                        , showNewPost = False
                        , showExistingPost = False
                        , newPost = initialPost
                        , existingPost = initialPost
                    }, Cmd.none)

                Err _ ->
                    ({ model | status = Failure }, Cmd.none)

        SetShowNewPost bool ->
            let
                subModel = Multiselect.populateValues model.postCategoryMultiselectModel (multiselectCategories model.postCategories) []
            in
            (
                { model | showNewPost = bool
                , showExistingPost = False
                , postCategoryMultiselectModel = subModel
                }
                , Cmd.none
            )

        SetEditingPost post ->
            let
                categories : List (String, String)
                categories =  multiselectCategories model.postCategories 

                selectedCategories : List (String, String)
                selectedCategories = multiselectSelectedCategories post model.postCategories

                subModel : Multiselect.Model
                subModel = 
                    Multiselect.populateValues model.postCategoryMultiselectModel categories selectedCategories
            in
            ( 
                { model | existingPost = post
                , showExistingPost = True
                , postCategoryMultiselectModel = subModel
                } 
              , Cmd.none
            )

        SetShowEditingPost bool ->
            ({model | showExistingPost = bool, showNewPost = False}, Cmd.none)

        Title title ->
            let
                updateTitle : Post -> Post
                updateTitle post = { post | title = title }
            in
            if model.showNewPost then
                ( { model | newPost = updateTitle model.newPost }, Cmd.none )
            else if model.showExistingPost then
                ( { model | existingPost = updateTitle model.existingPost }, Cmd.none )
            else
                (model, Cmd.none)

        Text text ->
            let
                updateText : Post -> Post
                updateText post = { post | text = text }
            in
            if model.showNewPost then
                ( { model | newPost = updateText model.newPost }, Cmd.none )
            else if model.showExistingPost then
                ( { model | existingPost = updateText model.existingPost }, Cmd.none )
            else
                (model, Cmd.none)

        Categories multiSelectMsg ->
            let
                (subModel, subCmd, _ ) = Multiselect.update multiSelectMsg model.postCategoryMultiselectModel

                updateCategories : Post -> Post
                updateCategories post =
                    {post | categories = List.map (\(id, name) -> id) ( Multiselect.getSelectedValues model.postCategoryMultiselectModel ) }

            in
            if model.showNewPost then
                ({ model | postCategoryMultiselectModel = subModel, newPost = updateCategories model.newPost }, Cmd.map Categories subCmd)
            else if model.showExistingPost then
                ({ model | postCategoryMultiselectModel = subModel, existingPost = updateCategories model.existingPost }, Cmd.map Categories subCmd)
            else
                (model, Cmd.none)

        Published published ->
            let 
                updatePublished : Post -> Post 
                updatePublished post = {post | published = published}
            in
            if model.showNewPost then
                ( { model | newPost = updatePublished model.newPost }, Cmd.none )

            else if model.showExistingPost then
                ( { model | existingPost = updatePublished model.existingPost }, Cmd.none )

            else
                (model, Cmd.none)

        CreatePost ->
            (model, Api.createPost model.newPost GotPost )

        UpdatePost ->
            (model, Api.updatePost model.existingPost GotPost)




-- Helper
multiselectCategories : List PostCategory -> List (String, String)
multiselectCategories categories = List.map (\c -> (c.id, c.category_name)) categories

multiselectSelectedCategories : Post -> List PostCategory -> List (String, String)
multiselectSelectedCategories post categories = 
    let 
        filterById : PostCategory -> Bool
        filterById category =
            List.any ( \i -> category.id == i) post.categories

        filteredCategories : List PostCategory
        filteredCategories =  List.filter filterById categories

    in
    multiselectCategories filteredCategories

-- Model

type alias Model =
    { posts : (List Post)
    , postCategories : (List PostCategory)
    , status : Status
    , showNewPost : Bool
    , newPost : Post
    , existingPost : Post
    , showExistingPost : Bool
    , postCategoryMultiselectModel : Multiselect.Model
    }

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map Categories <| Multiselect.subscriptions model.postCategoryMultiselectModel


-- Init
initialPost : Post
initialPost = 
    { id = ""
    , title = ""
    , text = ""
    , categories = [""]
    , published = False
    , serie = Nothing
    , created = ""
    , updated = ""
    } 

initialModel : Model
initialModel = 
    { posts = []
    , postCategories = []
    , status = Loading
    , showNewPost = False
    , newPost = initialPost
    , existingPost = initialPost
    , showExistingPost = False
    , postCategoryMultiselectModel = Multiselect.initModel [] "categories" 
    }


init : (Model, Cmd Msg)
init =
    (initialModel, Cmd.batch [getBlogCategories GotCategories, getBlogPosts GotPosts ]  )



-- View 

htmlIf : (Bool, Html msg) -> Html msg
htmlIf (bool, component) =
    case bool of
        True ->
            component

        False ->
            text ""

view : Model -> Html Msg
view model =
    let
        optionView : PostCategory -> Html msg
        optionView category = option [ Attr.id "postCategories" ] [text category.category_name] 

    in
    div [ Attr.class "container-fluid" ] 
        [ newPostButtonView model
        , editNewPostView model
        , postTableView model
        , editExistingPostView model
        ]

newPostButtonView : Model -> Html Msg
newPostButtonView model = 
    htmlIf 
        ( (not model.showNewPost) && ( not model.showExistingPost )
        , button 
            [ Attr.class "btn btn-success"
            , onClick (SetShowNewPost True)
            ] [ text "New Post" ])

editNewPostView : Model -> Html Msg
editNewPostView model =
    htmlIf 
        (model.showNewPost
        , editPostView 
            {model = model
            , post = model.newPost
            , cancelMsg = ( SetShowNewPost False )
            , submitMsg = CreatePost
            , title = "Your are creating a new post"
            , titleMsg = Title
            , textMsg = Text
            , categoryMsg = Categories
            , publishMsg = Published
            }
        )

editExistingPostView : Model -> Html Msg
editExistingPostView model =
    htmlIf 
        ( model.showExistingPost 
        , editPostView 
            {model = model
            , post = model.existingPost
            , cancelMsg = ( SetShowEditingPost False )
            , submitMsg = UpdatePost
            , title = "Your are editing: '" ++ model.existingPost.title ++ "'"
            , titleMsg = Title
            , textMsg = Text
            , categoryMsg = Categories
            , publishMsg = Published
            }
        )

editPostView : 
    { model: Model
    , title : String
    , post: Post 
    , cancelMsg: Msg
    , submitMsg : Msg
    , titleMsg : String -> Msg
    , textMsg : String -> Msg
    , categoryMsg : Multiselect.Msg -> Msg
    , publishMsg : Bool -> Msg
    } -> Html Msg
editPostView 
    { model
    , post
    , cancelMsg
    , submitMsg
    , title
    , titleMsg 
    , textMsg 
    , categoryMsg 
    , publishMsg 
    } =
    div [ Attr.class "col-6" ] 
        [ h2 [] [ text title ]
        , div [ Attr.class "form-group" ] 
            [ label [Attr.for "postTitle"] [ text "Post Title" ]
            , input [ Attr.class "form-control"
                    , Attr.type_ "text"
                    , Attr.placeholder "Post Title"
                    , Attr.id "postTitle"
                    , Attr.value post.title
                    , onInput titleMsg
                    ] [] 
            ] 
        , div [ Attr.class "form-group" ] 
            [ label [Attr.for "postText"] [ text "Post Text" ]
            , textarea 
                [ Attr.class "form-control"
                , Attr.style "height" "400px"
                , Attr.id "postText"
                , Attr.value post.text 
                , onInput textMsg
                ] [] 
            ] 
        , Html.map categoryMsg <| Multiselect.view model.postCategoryMultiselectModel
        , div [ Attr.class "form-check" ] 
            [ input [ Attr.class "form-check-input"
                    , Attr.type_ "checkbox" 
                    , Attr.id "postPublished"
                    , Attr.checked post.published
                    , onCheck publishMsg
                    ] [] 
            , label [ Attr.for "postPublished"
                    , Attr.class "form-check-label"
                    ] [ text "Published" ]
            ]
        , button [ Attr.class "btn btn-primary", onClick submitMsg ] [ text "Post" ]
        , button [ Attr.class "btn btn-secondary", onClick cancelMsg ] [ text "Cancel" ]
        ]

postTableView : Model -> Html Msg
postTableView model =
    let
        postRow : Post -> Html Msg
        postRow post =
            div [Attr.class "row border-bottom py-2 border-grey justify-content-between"] 
                [ h5 [ Attr.class "col-auto" ] [ text post.title ]  
                , button [ Attr.class "col-auto btn btn-sm btn-secondary", onClick (SetEditingPost post) ] [ text "Edit" ]
                ]

    in
    htmlIf 
        ( ( not model.showNewPost ) && ( not model.showExistingPost )
        , div [ Attr.class "col-6" ] 
            [ h2 [] [ text "Blog Posts:" ]
            , div [] (List.map postRow model.posts)
            ]
        )

