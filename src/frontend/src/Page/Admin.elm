module Page.Admin exposing (view, Model, init, Msg, update)
import Html exposing (..)
import Api exposing (PostCategory, Post, getBlogCategories, getBlogPosts)
import Html.Attributes as Attr
import Http
import Html.Events exposing (onClick)

type Msg
    = GotPosts (Result Http.Error (List Post))
    | GotCategories (Result Http.Error (List PostCategory))
    | ChangeShowNewPost Bool


-- Model
type Status
    = Failure
    | Loading
    | Success

type Login 
    = Authorized
    | Unauthorized

type alias Model =
    { posts : (List Post)
    , postCategories : (List PostCategory)
    , status : Status
    , login : Login
    , showNewPost : Bool
    , newPost : Post
    }

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

        ChangeShowNewPost bool ->
            ({model | showNewPost = bool}, Cmd.none)


-- Init
initialNewPost : Post
initialNewPost = 
    { id = Nothing
    , title = ""
    , text = ""
    , categories = [""]
    , published = False
    , serie = Nothing
    , created = Nothing
    , updated = Nothing
    } 

initialModel : Model
initialModel = 
    { posts = []
    , postCategories = []
    , status = Loading
    , login = Unauthorized
    , showNewPost = False
    , newPost = initialNewPost
    }


init : (Model, Cmd Msg)
init =
    (initialModel, Cmd.batch [getBlogCategories GotCategories, getBlogPosts GotPosts ]  )

view : Model -> Html Msg
view model =
    let
        optionView : PostCategory -> Html msg
        optionView category = option [ Attr.id "postCategories" ] [text category.category_name] 

        htmlIf : (Bool, Html msg) -> Html msg
        htmlIf (bool, component) =
            case bool of
                True ->
                    component

                False ->
                    text ""
    in
    div [ Attr.class "container-fluid" ] 
        [ htmlIf 
            ( not model.showNewPost
            , button 
                [ Attr.class "btn btn-success"
                , onClick (ChangeShowNewPost True)
                ] [ text "New Post" ])
        , htmlIf (model.showNewPost, editPostView {model = model, post = Nothing, cancelMsg = ( ChangeShowNewPost False )})
        , h2 [] [ text "Blog Posts:" ]
        , postTableView model
        ]

editPostView : {model: Model, post: Maybe Post, cancelMsg: Msg} -> Html Msg
editPostView {model, post, cancelMsg} =
    let
        optionView : PostCategory -> Html msg
        optionView category = option [ Attr.id "postCategories" ] [text category.category_name] 
    in
    div [ Attr.class "col-6" ] 
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

postTableView : Model -> Html msg
postTableView model =
    let
        postRow : Post -> Html msg
        postRow post =
            div [Attr.class "row border-bottom py-2 border-grey justify-content-between"] 
                [ h5 [ Attr.class "col-auto" ] [ text post.title ]  
                , button [ Attr.class "col-auto btn btn-sm btn-secondary" ] [ text "Edit" ]
                ]
    in
    div [ Attr.class "col-6" ] 
        (List.map postRow model.posts)

