module Page.Admin exposing 
    ( OutMsg(..)
    , view
    , Model
    , init
    , Msg
    , update
    , subscriptions
    , toSession
    )
import Views.MarkdownView exposing (renderMarkdown)
import Html exposing (..)
import Api exposing (Cred)
import Data.Post as Post exposing (Post, PostCategory, getPosts, getPostCategories)
import Route
import Html.Attributes as Attr
import Session exposing (Session)
import Http
import Html.Events exposing (onClick, onInput, onCheck)
import Multiselect
import Views.PostForm as PostForm

-- types
type Msg
    = GotPosts (Result Http.Error (List Post))
    | GotPost (Result Http.Error Post)
    | GotCategories (Result Http.Error (List PostCategory))
    | EditPost Post PostEditing
    | GotPostFormMsg PostForm.Msg
    | GotSession Session


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
update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
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
                    }, Cmd.none)

                Err error ->
                    case error of 
                        Http.BadStatus status ->
                            if status == 401 then
                                ( model, Cmd.none)
                            else
                                (model, Cmd.none)
                        _  ->
                            ({model | status = Failure}, Cmd.none)

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
            )

        GotPostFormMsg postFormMsg ->
            let
                (formModel, cmd, outMsg) = PostForm.update postFormMsg model.postFormModel
                requestMethod =
                    if model.newPost then
                        Post.createPost
                    else
                        Post.updatePost

                doRequest : Post -> Cmd Msg
                doRequest post =
                    case Session.cred model.session of
                        Just cred ->
                            requestMethod cred post GotPost 
                            
                        Nothing ->
                            Cmd.none
            in
            case outMsg of
                Just PostForm.CancelSend ->
                    ( {model | showPostForm = False}, Cmd.none )

                Just (PostForm.SubmitSend post) ->
                    ( { model | showPostForm = False }, doRequest post)

                Nothing ->
                    ( { model | postFormModel = formModel }, Cmd.none)

        GotSession session ->
            ( { model | session = session }, Cmd.none)


-- Model

type alias Model =
    { posts : (List Post)
    , postCategories : (List PostCategory)
    , status : Status
    , showPostForm : Bool
    , newPost : Bool
    , postFormModel : PostForm.Model
    , session : Session
    }

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
        [ Sub.map GotPostFormMsg <| PostForm.subscriptions model.postFormModel
        , Session.changes GotSession (Session.navKey model.session)
        ]


-- Init
initialPost : Post
initialPost = 
    { id = ""
    , title = ""
    , text = ""
    , categories = []
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

initialModel : Session -> Model
initialModel session = 
    { posts = []
    , postCategories = []
    , status = Loading
    , showPostForm = False
    , newPost = False
    , session = session
    , postFormModel = createPostFormModel initialPost Nothing
    }


init : Session -> (Model, Cmd Msg)
init session =
    let
        commands =
            case Session.cred session of
                Just cres -> 
                    [ getPosts (Session.cred session) GotPosts
                    , getPostCategories (Session.cred session) GotCategories
                    ]
                Nothing -> [ Route.pushUrl (Session.navKey session) Route.Login ]
    in
    ( initialModel session
    , Cmd.batch commands  
    )



-- View 

htmlIf : (Bool, Html msg) -> Html msg
htmlIf (bool, component) =
    case bool of
        True ->
            component

        False ->
            text ""

view : Model -> { title : String, content : Html Msg }
view model = 
    { title = "Admin", content = contentView model }


contentView : Model -> Html Msg
contentView model =
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
                    ]
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
            [ h2 [] [ text "Post Posts:" ]
            , div [] (List.map postRow model.posts)
            ]
        )

toSession : Model -> Session
toSession model =
    model.session
