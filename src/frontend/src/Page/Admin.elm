module Page.Admin exposing (view, Model, init, Msg, update)
import Html exposing (..)
import Api exposing (PostCategory, Post, getBlogCategories, getBlogPosts)
import Html.Attributes as Attr
import Http
import Html.Events exposing (onClick)

type EditablePost 
    = FullPost Post
    | HalfPost NewPost

type alias NewPost = 
    { title: String
    , text: String
    , categories : (List String)
    , serie : Maybe String
    , published : Bool
    }

type Msg
    = GotPosts (Result Http.Error (List Post))
    | GotCategories (Result Http.Error (List PostCategory))
    | SetShowNewPost Bool
    | SetEditingPost Post
    | SetShowEditingPost Bool


-- Model
type Status
    = Failure
    | Loading
    | Success

type Login 
    = Authorized
    | Unauthorized


-- Update
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let 
        _ = Debug.log "Message" msg
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
            ({model | showNewPost = bool, showEditingPost = False}, Cmd.none)

        SetEditingPost post ->
            ({model | editingPost = post, showEditingPost = True}, Cmd.none)

        SetShowEditingPost bool ->
            ({model | showEditingPost = bool, showNewPost = False}, Cmd.none)



-- Model

type alias Model =
    { posts : (List Post)
    , postCategories : (List PostCategory)
    , status : Status
    , login : Login
    , showNewPost : Bool
    , newPost : NewPost
    , editingPost : Post
    , showEditingPost : Bool
    }


-- Init

initialNewPost : NewPost
initialNewPost = 
    { title = ""
    , text = ""
    , categories = [""]
    , published = False
    , serie = Nothing
    } 


initialEditingPost : Post
initialEditingPost = 
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
    , newPost = initialNewPost
    , editingPost = initialEditingPost
    , showEditingPost = False
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
        ( (not model.showNewPost) && ( not model.showEditingPost )
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
            , post = HalfPost model.newPost
            , cancelMsg = ( SetShowNewPost False )
            , title = "Your are creating a new post"
            }
        )

editExistingPostView : Model -> Html Msg
editExistingPostView model =
    htmlIf 
        ( model.showEditingPost 
        , editPostView 
            {model = model
            , post = FullPost model.editingPost
            , cancelMsg = ( SetShowEditingPost False )
            , title = "Your are editing: '" ++ model.editingPost.title ++ "'"
            }
        )

editPostView : 
    { model: Model
    , post: EditablePost 
    , cancelMsg: Msg
    , title : String
    } -> Html Msg
editPostView 
    { model
    , post
    , cancelMsg
    , title
    } =
    let
        optionView : PostCategory -> Html msg
        optionView category = option [ Attr.id "postCategories" ] [text category.category_name] 
    in
    div [ Attr.class "col-6" ] 
        [ h2 [] [ text title ]
        , div [ Attr.class "form-group" ] 
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
            ( label [Attr.for "postCategories"] [ text "Post Categories" ] 
            :: (List.map optionView model.postCategories)
            )
            
        , div [ Attr.class "form-check" ] 
            [ input [ Attr.class "form-check-input"
                    , Attr.type_ "checkbox" 
                    , Attr.id "postPublished"
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
        ( ( not model.showNewPost ) && ( not model.showEditingPost )
        , div [ Attr.class "col-6" ] 
            [ h2 [] [ text "Blog Posts:" ]
            , div [] (List.map postRow model.posts)
            ]
        )

