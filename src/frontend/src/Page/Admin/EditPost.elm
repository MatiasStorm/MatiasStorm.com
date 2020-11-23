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


type Msg
    = GotPost (Result Http.Error Post)
    | GotCategories (Result Http.Error (List PostCategory))
    | GotPostFormMsg PostForm.Msg
    | GotSession Session

type Status
    = Failure
    | Loading
    | Success

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


        GotPost result ->
            case result of
                Ok post ->
                    ( { model | postFormModel = createPostFormModel post ( Just model.postCategories ) } 
                      , Cmd.none 
                    )

                Err error ->
                    ({model | status = Failure}, Cmd.none)


        GotPostFormMsg postFormMsg ->
            let
                (formModel, cmd, outMsg) = PostForm.update postFormMsg model.postFormModel
            in
            case outMsg of
                Just PostForm.CancelSend ->
                    ( model
                    , (Route.Admin Route.AdminHome) 
                        |> Route.pushUrl (Session.navKey model.session)  )

                Just (PostForm.SubmitSend post) ->
                    ( model, model.request post)

                Nothing ->
                    ( { model | postFormModel = formModel }, Cmd.none)

        GotSession session ->
            ( { model | session = session }, Cmd.none)

type alias Model =
    { postCategories : (List PostCategory)
    , status : Status
    , postFormModel : PostForm.Model
    , session : Session
    , request : Post -> Cmd Msg
    , cred : Cred
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

initialModel : Session -> Cred -> Maybe String -> Model
initialModel session cred maybePostId = 
    let 
        request = 
            case maybePostId of
                Just postId ->
                    PostData.update cred GotPost 
                Nothing ->
                    PostData.create cred GotPost
    in
    { postCategories = []
    , status = Loading
    , session = session
    , cred = cred
    , postFormModel = createPostFormModel initialPost Nothing
    , request = request
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
    , Cmd.batch commands )



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
        [ editView model ]

editView : Model -> Html Msg
editView model =
    div [ Attr.class "row" ] 
        [ Html.map GotPostFormMsg <| PostForm.view model.postFormModel
        , markdownPreview ( PostForm.getPost model.postFormModel )
        ]

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


toSession : Model -> Session
toSession model =
    model.session

