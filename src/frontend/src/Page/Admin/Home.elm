module Page.Admin.Home exposing 
    ( Model
    , view
    , update
    , Msg
    , init
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
import Multiselect
import Views.PostForm as PostForm
import Views.SearchBar as SearchBar

-- types
type Msg
    = GotPosts (Result Http.Error (List Post))
    | GotSession Session
    | GoToEditPost ( Maybe String )
    | GotSearchBarMsg SearchBar.Msg


type Status
    = Failure
    | Loading
    | Success

-- Update
update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        GotPosts result ->
            case result of
                Ok postList ->
                    (   { model | posts = postList
                        , status = Success 
                    }, Cmd.none)

                Err _ ->
                    ({model | status = Failure}, Cmd.none)

        GotSession session ->
            ( { model | session = session }, Cmd.none)

        GoToEditPost maybePostId ->
            let
                navKey = Session.navKey model.session
                route = 
                    case maybePostId of
                        Just postId -> Route.Admin ( Route.AdminEditPost postId )
                        Nothing -> Route.Admin Route.AdminNewPost
            in
            (model, Route.pushUrl navKey route )

        GotSearchBarMsg searchBarMsg ->
            let
                (subModel, subCmd, outMsg) = SearchBar.update searchBarMsg model.searchBarModel

                getCommands =
                    case outMsg of
                        Just SearchBar.DoSearch ->
                            [Cmd.map GotSearchBarMsg subCmd
                            ,Route.pushUrl (Session.navKey model.session) ( Route.Home (Just ( SearchBar.getSearchText subModel )) ) 
                            ]
                        Nothing ->
                            [Cmd.map GotSearchBarMsg subCmd]
            in
            ( {model | searchBarModel = subModel}
            , Cmd.batch getCommands)


type alias Model =
    { posts : (List Post)
    , status : Status
    , session : Session
    , searchBarModel : SearchBar.Model
    }


-- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
        [ Session.changes GotSession (Session.navKey model.session)]


-- Init

initialModel : Session -> Model
initialModel session = 
    { posts = []
    , status = Loading
    , session = session
    , searchBarModel = SearchBar.initModel
    }


init : Session -> (Model, Cmd Msg)
init session =
    let
        commands =
            case Session.cred session of
                Just cres -> 
                    [ PostData.list (Session.cred session) GotPosts]

                Nothing -> 
                    [ Route.pushUrl (Session.navKey session) Route.Login ]
    in
    ( initialModel session
    , Cmd.batch commands  
    )



-- View 
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
        , Html.map GotSearchBarMsg ( SearchBar.view model.searchBarModel ) 
        , postTableView model
        ]


newPostButtonView : Model -> Html Msg
newPostButtonView model = 
    button 
        [ Attr.class "btn btn-success"
        , onClick ( GoToEditPost Nothing )
        ] [ text "New Post" ]


postTableView : Model -> Html Msg
postTableView model =
    let
        postRow : Post -> Html Msg
        postRow post =
            div [Attr.class "row border-bottom py-2 border-grey justify-content-between"] 
                [ h5 [ Attr.class "col-auto" ] [ text post.title ]  
                , button 
                    [ Attr.class "col-auto btn btn-sm btn-secondary"
                    , onClick (GoToEditPost ( Just post.id ))
                    ]
                    [ text "Edit" ]
                ]

    in
    div [ Attr.class "col-6" ] 
        [ h2 [] [ text "Post Posts:" ]
        , div [] (List.map postRow model.posts)
        ]

toSession : Model -> Session
toSession model =
    model.session

