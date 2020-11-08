module Main exposing (main)

import Api exposing (Cred)
-- import Avatar exposing (Avatar)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Page exposing (Page)
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Blank as Blank 
import Page.Login as Login
import Page.Admin as Admin
import Route exposing (Route)
import Session exposing (Session)
import Task
import Time
import Url exposing (Url)


type Model
    = Redirect Session
    | NotFound Session
    | Home Home.Model
    | Login Login.Model
    | Admin Admin.Model



-- MODEL


init : Maybe Cred -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeCred url navKey =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.fromCred navKey maybeCred))



-- VIEW


view : Model -> Document Msg
view model =
    let
        maybeCred =
            Session.cred (toSession model)

        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view maybeCred page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Redirect _ ->
            Page.view maybeCred Page.Other Blank.view

        NotFound _ ->
            Page.view maybeCred Page.Other NotFound.view

        Home home ->
            viewPage Page.Home GotHomeMsg (Home.view home)

        Login login ->
            viewPage Page.Login GotLoginMsg (Login.view login)

        Admin admin ->
            viewPage Page.Admin GotAdminMsg (Admin.view admin)



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotHomeMsg Home.Msg
    | GotSession Session
    | GotLoginMsg Login.Msg
    | GotAdminMsg Admin.Msg


toSession : Model -> Session
toSession page =
    case page of
        Redirect session ->
            session

        NotFound session ->
            session

        Home home ->
            Home.toSession home

        Login login ->
            Login.toSession login

        Admin admin ->
            Admin.toSession admin

changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
        _ = Debug.log "Change route" session
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Logout ->
            ( model, Api.logout )

        Just Route.Home ->
            Home.init session
                |> updateWith Home GotHomeMsg model

        Just Route.Login ->
            Login.init session
                |> updateWith Login GotLoginMsg model

        Just Route.Admin ->
            Admin.init session
                |> updateWith Admin GotAdminMsg model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let 
        _ = Debug.log "Main" msg
    in
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url) )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( GotHomeMsg subMsg, Home home ) ->
            Home.update subMsg home
                |> updateWith Home GotHomeMsg model

        ( GotLoginMsg subMsg, Login login ) ->
            Login.update subMsg login
                |> updateWith Login GotLoginMsg model

        ( GotAdminMsg subMsg, Admin admin ) ->
            Admin.update subMsg admin
                |> updateWith Admin GotAdminMsg model

        ( GotSession session, Redirect _ ) ->
            ( Redirect session
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotFound _ ->
            Sub.none

        Redirect _ ->
            Session.changes GotSession (Session.navKey (toSession model))

        Home home ->
            Sub.map GotHomeMsg (Home.subscriptions home)

        Login login ->
            Sub.map GotLoginMsg (Login.subscriptions login)

        Admin admin ->
            Sub.map GotAdminMsg (Admin.subscriptions admin)


-- MAIN

main : Program Value Model Msg
main =
    Api.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
