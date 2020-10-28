module Page.Admin exposing (view, Model, init, Msg, update)
import Html exposing (..)
import Api exposing (PostCategory, Post, getBlogCategories, getBlogPosts)
import Html.Attributes as Attr
import Http

type Msg
    = GotPosts (Result Http.Error (List Post))
    | GotCategories (Result Http.Error (List PostCategory))

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


-- Init
init : (Model, Cmd Msg)
init =
    (Model [] [] Loading Unauthorized, Cmd.batch [getBlogCategories GotCategories, getBlogPosts GotPosts ]  )

view : Model -> Html msg
view model =
    let
        optionView : PostCategory -> Html msg
        optionView category = option [ Attr.id "postCategories" ] [text category.category_name] 
    in
    div [ Attr.class "container" ] 
        [ editPostView {model = model, post = Nothing}
        ]

editPostView : {model: Model, post: Maybe Post} -> Html msg
editPostView {model, post} =
    let
        optionView : PostCategory -> Html msg
        optionView category = option [ Attr.id "postCategories" ] [text category.category_name] 
    in
    form [] 
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
        , button [ Attr.class "btn btn-primary" ] [ text "Post" ]
        ]

postTableView : Model -> Html msg
postTableView model =
    div
