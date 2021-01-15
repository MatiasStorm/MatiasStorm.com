module Views.SearchBar exposing 
    ( view  
    , update
    , OutMsg(..)
    , Msg
    , initModel
    , getSearchText
    , Model
    )
import Html exposing (..)
import Html.Attributes exposing(class,type_, classList, style, value)
import Html.Events exposing (onClick, onSubmit, onInput)


type alias Model = { search : String }

initModel : Model
initModel = {search = ""}

getSearchText : Model -> String
getSearchText model = model.search

type Msg
    = Reset
    | Search
    | Change String

type OutMsg = DoSearch

update : Msg -> Model -> (Model, Cmd Msg, Maybe OutMsg)
update msg model =
    case msg of
        Search ->
            (model, Cmd.none, Just DoSearch)
        Reset ->
            ({model | search = ""}, Cmd.none, Just DoSearch)
        Change search ->
            ({model | search = search}, Cmd.none, Nothing)

view : Model -> Html Msg
view model =
    form [class "mt-2 input-group", onSubmit Search] 
        [ button [class "btn btn-dark bt-lg mr-1", type_ "submit"] [text "Search"]
        , button [class "btn btn-secondary bt-lg mr-1", type_ "button", onClick Reset] [text "Reset"]
        , input [class "form-control", onInput Change, value model.search] [] 
        ]
