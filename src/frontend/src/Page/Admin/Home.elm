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
import Data.StrippedPost as SPD exposing (StrippedPost)
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
    = GotPosts (Result Http.Error (List StrippedPost))
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
                route = Route.Admin (Route.AdminHome (Just (SearchBar.getSearchText subModel))) 

                getCommands =
                    case outMsg of
                        Just SearchBar.DoSearch ->
                            [Cmd.map GotSearchBarMsg subCmd
                            , route |> Route.pushUrl (Session.navKey model.session)
                            ]
                        Nothing ->
                            [Cmd.map GotSearchBarMsg subCmd]
            in
            ( {model | searchBarModel = subModel}
            , Cmd.batch getCommands)


type alias Model =
    { posts : (List StrippedPost)
    , status : Status
    , session : Session
    , searchBarModel : SearchBar.Model
    , search : String
    }


-- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
        [ Session.changes GotSession (Session.navKey model.session)]


-- Init

initialModel : Session -> String -> Model
initialModel session search = 
    { posts = []
    , status = Loading
    , session = session
    , searchBarModel = SearchBar.initModel
    , search = search
    }


init : Session -> Maybe String -> (Model, Cmd Msg)
init session maybeSearch =
    let
        search = 
            case maybeSearch of
                Just s -> s
                Nothing -> ""

        commands =
            case Session.cred session of
                Just cres -> 
                    [ SPD.get (SPD.search search [])(Session.cred session) GotPosts]

                Nothing -> 
                    [ Route.pushUrl (Session.navKey session) Route.Login ]
    in
    ( initialModel session search
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
    div [ Attr.class "container mt-2" ] 
        [ header model
        , Html.map GotSearchBarMsg ( SearchBar.view model.searchBarModel ) 
        , postTableView model
        ]


header : Model -> Html Msg
header model = 
    div [Attr.class "row justify-content-between"]
        [ h2 [Attr.class "col-auto"] [ text "Posts:" ]
        , button 
            [ Attr.class "btn btn-success col-auto"
            , onClick ( GoToEditPost Nothing )
            ] [ text "New Post" ]
        ]


postTableView : Model -> Html Msg
postTableView model =
    let
        postRow : StrippedPost -> Html Msg
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
    div [Attr.class "mt-2"] (List.map postRow model.posts)

toSession : Model -> Session
toSession model =
    model.session

