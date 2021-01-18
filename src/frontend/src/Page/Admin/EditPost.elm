module Page.Admin.EditPost exposing
    ( Model
    , Msg
    , view
    , init
    , update
    , subscriptions
    , toSession
    )

import Views.MarkdownView exposing (renderMarkdown)
import Html exposing (..)
import Api exposing (Cred)
import Data.Post as PostData exposing (Post)
import Data.PostCategory as CategoryData exposing (PostCategory)
import Route
import Html.Attributes as Attr
import Session exposing (Session)
import Http
import Html.Events exposing (onClick, onInput, onCheck)
import Views.PostForm as PostForm
import Views.PostView as PostView
import Route
import Data.Post exposing (create)


type Msg
    = GotPost (Result Http.Error Post)
    | GotCategories (Result Http.Error (List PostCategory))
    | GotPostFormMsg PostForm.Msg
    | GotSession Session
    | GotPostResponse (Result Http.Error Post)
    | TogglePreview
    | Cancel
    | Submit

type Status
    = Failure
    | Loading
    | Success

-- Update
update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        GotCategories result ->
            let
                getMaybePost =
                    case model.postId of
                        Just postId -> PostData.get postId ( Just model.cred ) GotPost
                        Nothing -> Cmd.none
            in
            case result of 
                Ok categories ->
                    ( { model 
                        | postFormModel = createPostFormModel initialPost categories
                        , categories = categories
                        , status = Success 
                    }, getMaybePost)

                Err _ ->
                    ({model | status = Failure}, Cmd.none)


        GotPost result ->
            case result of
                Ok post ->
                    ( { model | postFormModel = createPostFormModel post model.categories } 
                      , Cmd.none 
                    )

                Err error ->
                    ({model | status = Failure}, Cmd.none)

        GotPostResponse result ->
            case result of 
                Ok post ->
                    (model, Route.pushUrl (Session.navKey model.session) (Route.Admin <| Route.AdminHome Nothing))

                Err _ ->
                    (model, Cmd.none) -- Maybe display error at some point.


        GotPostFormMsg postFormMsg ->
            let
                (formModel, cmd) = PostForm.update postFormMsg model.postFormModel
            in
            ({model | postFormModel = formModel}, Cmd.map GotPostFormMsg cmd)

        Cancel ->
            ( model
            , Route.pushUrl (Session.navKey model.session) (Route.Admin <| Route.AdminHome Nothing) 
            )

        Submit ->
            let
                request = 
                    case model.postId of
                        Just postId ->
                            PostData.update model.cred GotPostResponse 
                        Nothing ->
                            PostData.create model.cred GotPostResponse
                post = PostForm.getPost model.postFormModel
            in
            ( model, request post)

        GotSession session ->
            ( { model | session = session }, Cmd.none)

        TogglePreview ->
            ( {model | showPreview = not model.showPreview}, Cmd.none )

type alias Model =
    { status : Status
    , postFormModel : PostForm.Model
    , session : Session
    , postId : Maybe String
    , showPreview: Bool
    , cred : Cred
    , categories : List PostCategory
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

createPostFormModel : Post -> List PostCategory -> PostForm.Model
createPostFormModel post categories =
    PostForm.initModel post categories

initialModel : Session -> Cred -> Maybe String -> Model
initialModel session cred maybePostId = 
    { status = Loading
    , session = session
    , cred = cred
    , showPreview = False
    , postId = maybePostId
    , postFormModel = createPostFormModel initialPost []
    , categories = []
    }


init : Session -> Cred -> Maybe String-> (Model, Cmd Msg)
init session cred maybePostId=
    let 
        commands = 
            case maybePostId of
                Just postId ->
                    [CategoryData.list (Session.cred session) GotCategories
                    , PostData.get postId ( Just cred ) GotPost ]

                Nothing ->
                    [CategoryData.list (Session.cred session) GotCategories]
    in
    ( initialModel session cred maybePostId
    , CategoryData.list (Session.cred session) GotCategories )



-- View 
view : Model -> { title : String, content : Html Msg }
view model = 
    { title = "Admin", content = contentView model }


contentView : Model -> Html Msg
contentView model =
    let
        pageContent = 
            case model.showPreview of
                True ->
                    preview ( PostForm.getPost model.postFormModel )

                False ->
                    editView model.postFormModel
    in
    div [ Attr.class "container-fluid" ] 
    [ previewToggler model.showPreview
    , pageContent 
    ]

previewToggler : Bool -> Html Msg
previewToggler showPreview =
    let 
        tab : Msg -> String -> Bool -> Html Msg
        tab onclick title isActive = 
            li [ Attr.class "nav-item" ] 
                [ button 
                    [ Attr.classList [ ("nav-link", True), ("active disabled", isActive ) ]
                    , onClick onclick 
                    ] 
                    [ text title ] 
                ]
    in
    ul [ Attr.class "my-2 nav nav-tabs" ] 
        [ tab TogglePreview "Edit" (not showPreview)
        , tab TogglePreview "Show Preview" showPreview
        ]

editView : PostForm.Model -> Html Msg
editView postFormModel =
    div []
        [ Html.map GotPostFormMsg <| PostForm.view postFormModel
        , button [ Attr.class "btn btn-primary", onClick Submit ] [ text "Post" ]
        , button [ Attr.class "btn btn-secondary", onClick Cancel ] [ text "Cancel" ]
        ]


preview : Post -> Html Msg
preview post =
    PostView.view post


toSession : Model -> Session
toSession model =
    model.session

