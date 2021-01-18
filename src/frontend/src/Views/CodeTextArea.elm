module Views.CodeTextArea exposing
    ( view
    , Model
    , update
    , Msg
    , initModel
    )
import Html exposing (..)
import Html.Lazy
import Html.Attributes exposing (..)
import SyntaxHighlight as SH
import Dict exposing (Dict)
import Parser
import Html.Events exposing (onInput)
import Json.Decode as Json


type alias Scroll = 
    { top : Int
    , left : Int
    }

type alias Model =
    { scroll : Scroll
    , code : String
    , theme : String
    }

initModel : String -> Model
initModel code =
    { scroll = Scroll 0 0 
    , code = code
    , theme = "GitHub"
    }

type Msg 
    = SetText String
    | OnScroll Scroll

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetText code ->
            ({model | code = code}, Cmd.none)

        OnScroll scroll ->
            ({model | scroll = scroll}, Cmd.none)

view : Model -> Html Msg
view model =
    div []
        [ Html.Lazy.lazy textareaStyle model.theme
        , Html.Lazy.lazy syntaxThemeStyle model.theme
        , viewLanguage model toHtmlElm
        ]

textareaStyle : String -> Html msg
textareaStyle theme =
    let
        style a b =
            Html.node "style"
                []
                [ text
                    (String.join "\n"
                        [ ".code-text-textarea {caret-color: " ++ a ++ ";}"
                        , ".code-text-textarea::selection { background-color: " ++ b ++ "; }"
                        ]
                    )
                ]
    in
    if List.member theme [ "Monokai", "One Dark", "Custom" ] then
        style "#f8f8f2" "rgba(255,255,255,0.2)"

    else
        style "#24292e" "rgba(0,0,0,0.2)"


syntaxThemeStyle : String -> Html msg
syntaxThemeStyle selectedTheme =
    case selectedTheme of
        "Monokai" ->
            SH.useTheme SH.monokai

        "GitHub" ->
            SH.useTheme SH.gitHub

        "One Dark" ->
            SH.useTheme SH.oneDark

        _ ->
            SH.useTheme SH.gitHub

viewLanguage : Model -> (Maybe Int -> String -> Html Msg) -> Html Msg
viewLanguage model parser =
    div
        [ classList
            [ ( "code-text-container", True )
            , ( "elmsh", True )
            ]
        ]
        [ div
            [ class "code-text-view-container"
            , style "transform"
                ("translate("
                    ++ String.fromInt -model.scroll.left
                    ++ "px, "
                    ++ String.fromInt -model.scroll.top
                    ++ "px)"
                )
            , style "will-change" "transform"
            ]
            [ Html.Lazy.lazy2 parser (Just 1) model.code]
        , viewTextarea model.code
        ]


viewTextarea : String -> Html Msg
viewTextarea body =
    textarea
        [ value body
        , classList
            [ ( "code-text-textarea", True )
            , ( "code-text-textarea-lc", True )
            ]
        , onInput SetText
        , spellcheck False
        , Html.Events.on "scroll"
            (Json.map2 Scroll
                (Json.at [ "target", "scrollTop" ] Json.int)
                (Json.at [ "target", "scrollLeft" ] Json.int)
                |> Json.map OnScroll
            )
        ]
        []



-- Helpers function for Html.Lazy.lazy


toHtmlElm : Maybe Int -> String -> Html Msg
toHtmlElm =
    toHtml SH.elm


-- toHtmlXml : Maybe Int -> String -> HighlightModel -> Html Msg
-- toHtmlXml =
--     toHtml SH.xml


-- toHtmlJavascript : Maybe Int -> String -> HighlightModel -> Html Msg
-- toHtmlJavascript =
--     toHtml SH.javascript


-- toHtmlCss : Maybe Int -> String -> HighlightModel -> Html Msg
-- toHtmlCss =
--     toHtml SH.css


-- toHtmlPython : Maybe Int -> String -> HighlightModel -> Html Msg
-- toHtmlPython =
--     toHtml SH.python


-- toHtmlSql : Maybe Int -> String -> HighlightModel -> Html Msg
-- toHtmlSql =
--     toHtml SH.sql


-- toHtmlJson : Maybe Int -> String -> HighlightModel -> Html Msg
-- toHtmlJson =
--     toHtml SH.json


-- toHtmlNoLang : Maybe Int -> String -> HighlightModel -> Html Msg
-- toHtmlNoLang =
--     toHtml SH.noLang

toHtml : (String -> Result (List Parser.DeadEnd) SH.HCode) -> Maybe Int -> String -> Html Msg
toHtml parser maybeStart str =
    parser str
        |> Result.map (SH.toBlockHtml maybeStart)
        |> Result.mapError Parser.deadEndsToString
        |> (\result ->
                case result of
                    Result.Ok a ->
                        a

                    Result.Err x ->
                        text x
           )
