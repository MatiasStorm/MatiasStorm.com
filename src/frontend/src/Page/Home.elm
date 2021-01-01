module Page.Home exposing 
    (view
    , Model
    , init
    , Msg
    , update
    , toSession
    , subscriptions
    )
import Html exposing (..)
import Html.Attributes exposing(class, classList, style, value)
import Html.Events exposing (onClick, onSubmit, onInput)
import Http
import Views.MarkdownView exposing (renderMarkdown)
import Session exposing (Session)
import Route
import Data.Post as PostData exposing (Post)
import Data.PostCategory as CategoryData exposing (PostCategory)
import Views.PostView as PostView
import Views.StrippedPostView as StrippedPostView
import Data.StrippedPost as SPD exposing (StrippedPost)
import Views.PostCategoryTag as TagView


type Msg
    = GotStrippedPosts (Result Http.Error (List StrippedPost))
    | GotPostCategories (Result Http.Error (List PostCategory))
    | GotSession Session
    | GoToPost String
    | DoSearch
    | ChangeSearch String
    | ToggleCategory String

-- Model
type Status
    = Failure
    | Loading
    | Success

type alias Model =
    { posts : (List StrippedPost)
    , status : Status
    , session : Session
    , categories : (List PostCategory)
    , search : String
    , activeCategories : (List String)
    }

initModel : Session -> String -> Model
initModel session search =
    { posts = []
    , status = Loading
    , session = session
    , categories = []
    , search = search
    , activeCategories = []
    }

-- Init
init : Session -> Maybe String -> (Model, Cmd Msg)
init  session maybeSearch =
    let
        search =
            case maybeSearch of
                Just s -> s
                Nothing -> ""
    in
    (initModel session search
    , Cmd.batch 
        [ SPD.get [] Nothing GotStrippedPosts
        , CategoryData.list Nothing GotPostCategories
        ]
    )

toSession : Model -> Session
toSession model =
    model.session

subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)

-- Update
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ( { model | session = session }, Cmd.none)

        GotStrippedPosts result ->
            case result of
                Ok postList ->
                    ( { model 
                        | posts = postList
                        , status = Success 
                    }, Cmd.none)

                Err _ ->
                    ({model | status = Failure}, Cmd.none)

        GotPostCategories result ->
            case result of
                Ok categories ->
                    ( { model 
                        | categories = categories
                        , status = Success 
                    }, Cmd.none)

                Err _ ->
                    ({model | status = Failure}, Cmd.none)

        GoToPost postId ->
            let
                key = Session.navKey model.session
                route = Route.Post postId
            in
            (model, Route.pushUrl key route)

        DoSearch ->
            ( model, Route.replaceUrl (Session.navKey model.session) ( Route.Home (Just model.search) ) )

        ChangeSearch search ->
            ({model | search = search}, Cmd.none)

        ToggleCategory id ->
            let 
                activeCategories = 
                    if List.member id model.activeCategories then
                        List.filter ( \c -> c /= id ) model.activeCategories 
                    else 
                        id :: model.activeCategories
            in
            ({model | activeCategories = activeCategories }
            , SPD.get (SPD.categoryIds activeCategories []) Nothing GotStrippedPosts
            )


-- View
view : Model -> {title : String, content : Html Msg}
view model =
    let
        filteredPosts =
            if List.length model.activeCategories > 0 then
                List.filter (\p -> List.any (\c -> List.member c.id model.activeCategories ) p.categories) model.posts
            else
                model.posts
        content = 
            case model.status of
                Failure ->
                    text "Cannot load categories"

                Loading ->
                    text "Loading..."

                Success ->
                    div [class "container"]  
                        [ div []
                                [ searchBarView model
                                , selectCategoryView model ]
                        , div []
                                (List.map postView model.posts)
                        ]
    in
    { title =  "Home" 
    , content = content
    }

postView : StrippedPost -> Html Msg
postView post =
    div [ onClick ( GoToPost post.id )
        , style "cursor" "pointer"
        ] 
        [StrippedPostView.view post]

searchBarView : Model -> Html Msg
searchBarView model =
    form [class "mt-2 input-group", onSubmit DoSearch] 
        [ button [class "btn btn-dark bt-lg mr-1"] [text "Search"]
        , input [class "form-control", onInput ChangeSearch, value model.search] [] 
        ]

selectCategoryView : Model -> Html Msg
selectCategoryView model =
    let
        categoryWrapper : PostCategory -> Html Msg
        categoryWrapper category =
            span [ style "cursor" "pointer", onClick ( ToggleCategory category.id )] 
                 [ TagView.view 5 (List.member category.id model.activeCategories) category ]
    in
    div [ class "d-flex flex-wrap mt-2" ] 
         ( List.map categoryWrapper model.categories ) 


