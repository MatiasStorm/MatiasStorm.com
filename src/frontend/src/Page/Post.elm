module Page.Post exposing (view, toSession, subscriptions, Model, init, Msg, update)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Session exposing (Session)
import Data.StrippedPost as StrippedPostData exposing (StrippedPost)
import Data.Post as PostData exposing (Post)
import Views.PostView as PostView
import Http
import Route exposing (Route)
import Html.Events exposing (onClick)

type Msg
    = GotPost ( Result Http.Error Post )
    | GotPrevPost ( Result Http.Error ( List StrippedPost ) )
    | GotNextPost ( Result Http.Error ( List StrippedPost ) )
    | NavigateToPost StrippedPost

type alias Model =
    { session: Session 
    , postId : String
    , post : Maybe Post
    , nextPost : Maybe StrippedPost
    , prevPost : Maybe StrippedPost
    }


init : Session -> String -> (Model, Cmd Msg)
init session postId =
    ( 
        { session = session
        , postId = postId
        , post = Nothing
        , nextPost = Nothing
        , prevPost = Nothing
        }
    , PostData.get postId (Session.cred session) GotPost)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

toSession : Model -> Session
toSession model = model.session

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPost result ->
            case result of
                Ok post ->
                    let 
                        cred = Session.cred model.session
                    in
                    ( {model | post = Just post}
                    , Cmd.batch 
                        [ StrippedPostData.getCountBefore 1 post.created cred GotPrevPost
                        , StrippedPostData.getCountAfter 1 post.created cred GotNextPost
                        ] 
                    )
                Err _ ->
                    (model, Cmd.none)

        GotPrevPost result ->
            case result of
                Ok prevPost ->
                    ({model | prevPost = List.head prevPost}, Cmd.none)
                Err _ ->
                    (model, Cmd.none)

        GotNextPost result ->
            case result of
                Ok nextPost ->
                    ({model | nextPost = List.head nextPost}, Cmd.none)
                Err _ ->
                    (model, Cmd.none)

        NavigateToPost post ->
            let
                key = Session.navKey model.session
                route = Route.Post post.id
            in
            (model, Route.pushUrl key route)




view : Model -> {title: String, content: Html Msg}
view model =
    { title = "Post"
    , content = content model
    }

content : Model -> Html Msg
content model = 
    div [class "container"] 
        [ div [] 
            [ postView model.post ]
        , div []
            [ postButtons {maybePrevPost = model.prevPost, maybeNextPost = model.nextPost} ]
        ]

postView : Maybe Post -> Html Msg
postView maybePost = 
    case maybePost of
        Just post ->
            PostView.view post
        Nothing ->
            text "Loading"

postButtons : {maybePrevPost : Maybe StrippedPost, maybeNextPost : Maybe StrippedPost} -> Html Msg
postButtons {maybePrevPost, maybeNextPost} = 
    let
        leftButton prevPost = 
            div [class "col-6", style "cursor" "pointer", onClick (NavigateToPost prevPost)] 
                [ h3 [] [ text "Previous Post" ]
                , h5 [] [ text prevPost.title ] ]

        rightButton nextPost = 
            div [class "col-6 text-right", style "cursor" "pointer", onClick (NavigateToPost nextPost)] 
                [ h3 [] [ text "Next Post" ]
                , h5 [] [ text nextPost.title ]
                ]

        container : List (Html Msg) -> Html Msg
        container containerContent =
            div [class "row"] containerContent
    in
    case (maybePrevPost, maybeNextPost) of
        (Just prevPost, Just nextPost) ->
            container [ leftButton prevPost
                      , rightButton nextPost
                      ]

        (Just prevPost, Nothing) ->
            container 
                [ leftButton prevPost
                , div [class "col-6"] []
                ]

        (Nothing , Just nextPost) ->
            container 
                [ div [class "col-6"] []
                , rightButton nextPost
                ]

        (Nothing, Nothing) ->
            text ""


















