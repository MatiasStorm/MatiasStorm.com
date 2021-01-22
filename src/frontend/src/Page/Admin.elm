module Page.Admin exposing 
    ( Model
    , view
    , init
    , Msg
    , update
    , subscriptions
    , toSession
    )
import Page.Admin.Home as Home
import Page.Admin.EditPost as EditPost
import Page.NotFound as NotFound
import Page.Blank as Blank 
import Session exposing (Session)
import Route exposing (AdminRoute)
import Html exposing (Html)
import Page exposing (AdminPage)

-- types
type Msg
    = GotHomeMsg Home.Msg
    | GotEditMsg EditPost.Msg
    | GotSession Session

-- Update
update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case (msg, model) of
        ( GotSession session, _ ) ->
            ( model, Cmd.none)

        ( GotHomeMsg subMsg, Home home ) ->
            Home.update subMsg home
                |> updateWith Home GotHomeMsg model

        ( GotEditMsg subMsg, Edit edit )->
            EditPost.update subMsg edit
                |> updateWith Edit GotEditMsg model

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


-- Model
type Model
    = Redirect Session
    | NotFound Session
    | Home Home.Model
    | Edit EditPost.Model

    -- | Edit


-- Init
init : Session -> AdminRoute -> (Model, Cmd Msg)
init session adminRoute =
    case Session.cred session of
        Just cred -> 
            case adminRoute of
                Route.AdminHome maybeSearch ->
                    Home.init session maybeSearch
                        |> updateWith Home GotHomeMsg (Redirect session)

                Route.AdminEditPost postId ->
                    EditPost.init session cred (Just postId)
                        |> updateWith Edit GotEditMsg (Redirect session)

                Route.AdminNewPost ->
                    EditPost.init session cred Nothing
                        |> updateWith Edit GotEditMsg (Redirect session)

        Nothing -> 
            ( Redirect session, Route.pushUrl (Session.navKey session) Route.Login )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotFound _ ->
            Sub.none

        Redirect _ ->
            Session.changes GotSession (Session.navKey (toSession model))

        Home home ->
            Sub.none

        Edit edit ->
            Sub.map GotEditMsg <| EditPost.subscriptions edit




-- View 

view : Model -> { title : String, content : Html Msg }
view model = 
    let
        maybeCred =
            Session.cred (toSession model)

        viewPage toMsg config =
            let
                { title, content } = config
            in
            { title = title
            , content = Html.map toMsg content
            }
    in
    case model of
        Redirect _ ->
            Blank.view
            
        NotFound _ ->
            NotFound.view

        Home home ->
            viewPage GotHomeMsg (Home.view home)

        Edit edit ->
            viewPage GotEditMsg (EditPost.view edit)





toSession : Model -> Session
toSession model =
    case model of
        Redirect session ->
            session

        NotFound session ->
            session

        Home home ->
            Home.toSession home

        Edit edit ->
            EditPost.toSession edit


