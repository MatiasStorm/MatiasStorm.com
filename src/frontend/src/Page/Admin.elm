module Page.Admin exposing (view, Model, init, Msg, update)
import Html exposing (..)
import Api exposing (PostCategory, Post, getBlogCategories, getBlogPosts)
import Html.Attributes as Attr
import Http
import Html.Events exposing (onClick)
import Html.Events exposing (onInput)
import Html.Events exposing (onCheck)
import Multiselect

-- subscriptions : Model -> Sub Msg
-- subscriptions model =
--     Sub.map Categories <| Multiselect.subscriptions categories

type Msg
    = GotPosts (Result Http.Error (List Post))
    | GotCategories (Result Http.Error (List PostCategory))
    | SetShowNewPost Bool
    | SetEditingPost Post
    | SetShowEditingPost Bool
    | Title PostType String
    | Text PostType String
    | Categories PostType Multiselect.Msg
    | Published PostType Bool

-- Model
type Status
    = Failure
    | Loading
    | Success

type Login 
    = Authorized
    | Unauthorized

type PostType
    = ExistingPost
    | NewPost

-- Update
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let 
        _ = Debug.log "Message" model

        updateTitle : Post -> String -> Post
        updateTitle post title = { post | title = title }

        updateText : Post -> String -> Post
        updateText post text = { post | text = text }
        
        updateCategories : Post -> String -> Post
        updateCategories post categoryId =
            if List.any (\cId -> cId == categoryId) post.categories then
                { post | categories = List.filter (\cId -> cId /= categoryId ) post.categories }
            else
                { post | categories = categoryId :: post.categories}

        updatePublished : Post -> Bool -> Post 
        updatePublished post published = {post | published = published}
    in
    case msg of
        GotCategories result ->
            case result of 
                Ok categoryList ->
                    ( { model 
                        | postCategories = categoryList
                        , status = Success 
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

        SetShowNewPost bool ->
            ({model | showNewPost = bool, showExistingPost = False}, Cmd.none)

        SetEditingPost post ->
            ({model | existingPost = post, showExistingPost = True}, Cmd.none)

        SetShowEditingPost bool ->
            ({model | showExistingPost = bool, showNewPost = False}, Cmd.none)

        Title postType title ->
            case postType of
                NewPost ->
                    ( { model | newPost = updateTitle model.newPost title}, Cmd.none )

                ExistingPost ->
                    ( { model | existingPost = updateTitle model.existingPost title}, Cmd.none )

        Text postType text ->
            case postType of
                NewPost ->
                    ( { model | newPost = updateText model.newPost text}, Cmd.none )

                ExistingPost ->
                    ( { model | existingPost = updateText model.existingPost text}, Cmd.none )

        Categories postType multiSelectMsg ->
            (model, Cmd.none)


        Published postType published ->
            case postType of
                NewPost ->
                    ( { model | newPost = updatePublished model.newPost published}, Cmd.none )

                ExistingPost ->
                    ( { model | existingPost = updatePublished model.existingPost published}, Cmd.none )


-- Model

type alias Model =
    { posts : (List Post)
    , postCategories : (List PostCategory)
    , status : Status
    , login : Login
    , showNewPost : Bool
    , newPost : Post
    , existingPost : Post
    , showExistingPost : Bool
    }


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
    , login = Unauthorized
    , showNewPost = False
    , newPost = initialPost
    , existingPost = initialPost
    , showExistingPost = False
    }


init : (Model, Cmd Msg)
init =
    (initialModel, Cmd.batch [getBlogCategories GotCategories, getBlogPosts GotPosts ]  )

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
            , title = "Your are creating a new post"
            , titleMsg = Title NewPost
            , textMsg = Text NewPost
            , categoryMsg = Categories NewPost
            , publishMsg = Published NewPost
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
            , title = "Your are editing: '" ++ model.existingPost.title ++ "'"
            , titleMsg = Title ExistingPost
            , textMsg = Text ExistingPost
            , categoryMsg = Categories ExistingPost
            , publishMsg = Published ExistingPost
            }
        )

editPostView : 
    { model: Model
    , title : String
    , post: Post 
    , cancelMsg: Msg
    , titleMsg : String -> Msg
    , textMsg : String -> Msg
    , categoryMsg : Multiselect.Msg -> Msg
    , publishMsg : Bool -> Msg
    } -> Html Msg
editPostView 
    { model
    , post
    , cancelMsg
    , title
    , titleMsg 
    , textMsg 
    , categoryMsg 
    , publishMsg 
    } =
    let
        _ = Debug.log "Cat" categories

        categories : List (String, String)
        categories =
            List.map (\c -> (c.category_name, c.id)) model.postCategories

        multiSelectModel : Multiselect.Model
        multiSelectModel = Multiselect.initModel categories "categories"
    in
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
        , Html.map categoryMsg <| Multiselect.view multiSelectModel
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
        , button [ Attr.class "btn btn-primary"] [ text "Post" ]
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

