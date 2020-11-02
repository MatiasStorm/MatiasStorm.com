module Page.Admin exposing (OutMsg(..), view, Model, init, Msg, update, subscriptions)
import Views.MarkdownView exposing (renderMarkdown)
import Html exposing (..)
import Api exposing (JWT, PostCategory, Post)
import Html.Attributes as Attr
import Http
import Html.Events exposing (onClick)
import Html.Events exposing (onInput)
import Html.Events exposing (onCheck)
import Multiselect
import Views.PostForm as PostForm

-- types
type Msg
    = GotPosts (Result Http.Error (List Post))
    | GotPost (Result Http.Error Post)
    | GotCategories (Result Http.Error (List PostCategory))
    | EditPost Post PostEditing
    | RedoRequest
    | GotPostFormMsg PostForm.Msg


type Status
    = Failure
    | Loading
    | Success

type OutMsg 
    = FailedRequest Msg 

type PostEditing 
    = CreateNewPost
    | EditExistingPost

-- Update
update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update msg model =
    let
        requestMethod =
            if model.newPost then
                Api.createPost
            else
                Api.updatePost
    in
    case msg of
        GotCategories result ->
            case result of 
                Ok categoryList ->
                    ( { model 
                        | postCategories = categoryList
                        , status = Success 
                    }, Cmd.none, Nothing)

                Err _ ->
                    ({model | status = Failure}, Cmd.none, Nothing)

        GotPosts result ->
            case result of
                Ok postList ->
                    ( { model 
                        | posts = postList
                        , status = Success 
                    }, Cmd.none, Nothing)

                Err _ ->
                    ({model | status = Failure}, Cmd.none, Nothing)

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
                        , showPostForm = False
                    }, Cmd.none, Nothing)

                Err error ->
                    case error of 
                        Http.BadStatus status ->
                            if status == 401 then
                                ( model
                                , Cmd.none
                                , Just (FailedRequest RedoRequest))
                            else
                                (model, Cmd.none, Nothing)
                        _  ->
                            ({model | status = Failure}, Cmd.none, Nothing)

        EditPost post postEditing ->
            let 
                newPost = 
                    case postEditing of
                        CreateNewPost -> True
                        EditExistingPost -> False
            in
            ( 
                { model | showPostForm = True
                , postFormModel = createPostFormModel post ( Just model.postCategories )
                , newPost = newPost
                } 
              , Cmd.none
              , Nothing
            )

        RedoRequest ->
            let
                post = PostForm.getPost model.postFormModel
            in
            (model, requestMethod post GotPost model.jwt, Nothing)

        GotPostFormMsg postFormMsg ->
            let
                (formModel, cmd, outMsg) = PostForm.update postFormMsg model.postFormModel
            in
            case outMsg of
                Just PostForm.CancelSend ->
                    ( {model | showPostForm = False}, Cmd.none, Nothing )

                Just (PostForm.SubmitSend post) ->
                    ( { model | showPostForm = False }, requestMethod post GotPost model.jwt, Nothing )

                Nothing ->
                    ( { model | postFormModel = formModel }, Cmd.none, Nothing)



-- Model

type alias Model =
    { posts : (List Post)
    , postCategories : (List PostCategory)
    , status : Status
    , showPostForm : Bool
    , newPost : Bool
    , postFormModel : PostForm.Model
    , jwt: Maybe JWT
    }

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map GotPostFormMsg <| PostForm.subscriptions model.postFormModel


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

createPostFormModel : Post -> Maybe (List PostCategory) -> PostForm.Model
createPostFormModel post categories =
    case categories of
        Just actualCategories ->
            PostForm.initModel post actualCategories
        Nothing ->
            PostForm.initModel post []

initialModel : Maybe JWT -> Model
initialModel jwt = 
    { posts = []
    , postCategories = []
    , status = Loading
    , showPostForm = False
    , newPost = False
    , jwt = jwt
    , postFormModel = createPostFormModel initialPost Nothing
    }


init : Maybe JWT -> (Model, Cmd Msg, Maybe OutMsg)
init jwt =
    ( initialModel jwt
    , Cmd.batch 
        [ Api.getBlogCategories GotCategories jwt
        , Api.getBlogPosts GotPosts jwt
        ]  
    , Nothing
    )



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
        , postTableView model
        , editView model
        ]

editView : Model -> Html Msg
editView model =
    if model.showPostForm then
        div [ Attr.class "row" ] 
            [ Html.map GotPostFormMsg <| PostForm.view model.postFormModel
            , markdownPreview ( PostForm.getPost model.postFormModel )
            ]
    else 
        text ""

markdownPreview : Post -> Html Msg
markdownPreview post =
    div [ Attr.class "col-6" ]
        [ h1 [] [text post.title ] 
        , case (renderMarkdown post.text) of
            Ok rendered ->
                div 
                    [ Attr.class "border border-black p-4"
                    , Attr.style "height" "80vh"
                    , Attr.style "overflow" "scroll"
                    , Attr.] 
                    rendered
            Err errors ->
                text errors
        ]


newPostButtonView : Model -> Html Msg
newPostButtonView model = 
    htmlIf 
        (not model.showPostForm
        , button 
            [ Attr.class "btn btn-success"
            , onClick (EditPost initialPost CreateNewPost)
            ] [ text "New Post" ])


postTableView : Model -> Html Msg
postTableView model =
    let
        postRow : Post -> Html Msg
        postRow post =
            div [Attr.class "row border-bottom py-2 border-grey justify-content-between"] 
                [ h5 [ Attr.class "col-auto" ] [ text post.title ]  
                , button [ Attr.class "col-auto btn btn-sm btn-secondary", onClick (EditPost post EditExistingPost) ] [ text "Edit" ]
                ]

    in
    htmlIf 
        ( not model.showPostForm
        , div [ Attr.class "col-6" ] 
            [ h2 [] [ text "Blog Posts:" ]
            , div [] (List.map postRow model.posts)
            ]
        )

