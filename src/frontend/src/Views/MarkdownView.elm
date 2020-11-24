module Views.MarkdownView exposing(renderMarkdown)
import Html
import Markdown.Parser
import Markdown.Renderer exposing (Renderer, render)
import Markdown.Block as Block exposing (Block, Inline, ListItem, Task)
import Html.Attributes as Attr
import Markdown.Html
import SyntaxHighlight as SH
import Dict exposing (Dict)
import Parser

renderMarkdown : String -> Result String (List (Html.Html msg))
renderMarkdown markdown =
    case
        markdown
            |> Markdown.Parser.parse
    of
        Ok okAst ->
            case Markdown.Renderer.render renderer okAst of
                Ok rendered ->
                    Ok rendered

                Err errors ->
                    Err errors

        Err error ->
            Err (error |> List.map Markdown.Parser.deadEndToString |> String.join "\n")


initLanguagesModel : Dict String (String -> Result (List Parser.DeadEnd) SH.HCode) 
initLanguagesModel =
    Dict.fromList
        [ ( "elm", SH.elm )
        , ( "xml", SH.xml )
        , ( "javascript", SH.javascript )
        , ( "css", SH.css )
        , ( "python", SH.python )
        , ( "sql", SH.sql )
        , ( "json", SH.json )
        , ( "nolang", SH.noLang )
        ]

getSyntax : {language: Maybe String, body: String} -> (Result (List Parser.DeadEnd) SH.HCode) 
getSyntax {language, body} =
    let 
        getLanguage =
            case language of
                Just actualLanguage ->
                    String.replace "\r" "" actualLanguage

                Nothing ->
                    "nolang"
    in
    case Dict.get getLanguage initLanguagesModel of
        Just func ->
            func body

        Nothing ->
            SH.noLang body


codeBlock : {body: String, language: Maybe String} -> Html.Html msg
codeBlock { body, language } =
    Html.div [   ]
        [ SH.useTheme SH.gitHub 
         ,getSyntax {language = language, body = body}
            |> Result.map (SH.toBlockHtml (Just 1))
            |> Result.withDefault
                (Html.pre [] [ Html.code [] [ Html.text body ]])

        ]

renderer : Renderer (Html.Html msg)
renderer =
    { heading =
        \{ level, children } ->
            case level of
                Block.H1 ->
                    Html.h1 [] children

                Block.H2 ->
                    Html.h2 [] children

                Block.H3 ->
                    Html.h3 [] children

                Block.H4 ->
                    Html.h4 [] children

                Block.H5 ->
                    Html.h5 [] children

                Block.H6 ->
                    Html.h6 [] children
    , codeBlock = codeBlock
    , paragraph = Html.p []
    , hardLineBreak = Html.br [] []
    , blockQuote = Html.blockquote [Attr.class "mx-5"]
    , strong =
        \children -> Html.strong [] children
    , emphasis =
        \children -> Html.em [] children
    , codeSpan =
        \content -> Html.code [] [ Html.text content ]
    , link =
        \link content ->
            case link.title of
                Just title ->
                    Html.a
                        [ Attr.href link.destination
                        , Attr.title title
                        ]
                        content

                Nothing ->
                    Html.a [ Attr.href link.destination ] content
    , image =
        \imageInfo ->
            case imageInfo.title of
                Just title ->
                    Html.img
                        [ Attr.src imageInfo.src
                        , Attr.alt imageInfo.alt
                        , Attr.title title
                        ]
                        []

                Nothing ->
                    Html.img
                        [ Attr.src imageInfo.src
                        , Attr.alt imageInfo.alt
                        ]
                        []
    , text =
        Html.text
    , unorderedList =
        \items ->
            Html.ul []
                (items
                    |> List.map
                        (\item ->
                            case item of
                                Block.ListItem task children ->
                                    let
                                        checkbox =
                                            case task of
                                                Block.NoTask ->
                                                    Html.text ""

                                                Block.IncompleteTask ->
                                                    Html.input
                                                        [ Attr.disabled True
                                                        , Attr.checked False
                                                        , Attr.type_ "checkbox"
                                                        ]
                                                        []

                                                Block.CompletedTask ->
                                                    Html.input
                                                        [ Attr.disabled True
                                                        , Attr.checked True
                                                        , Attr.type_ "checkbox"
                                                        ]
                                                        []
                                    in
                                    Html.li [] (checkbox :: children)
                        )
                )
    , orderedList =
        \startingIndex items ->
            Html.ol
                (case startingIndex of
                    1 ->
                        [ Attr.start startingIndex ]

                    _ ->
                        []
                )
                (items
                    |> List.map
                        (\itemBlocks ->
                            Html.li []
                                itemBlocks
                        )
                )
    , html = Markdown.Html.oneOf []
    , thematicBreak = Html.hr [] []
    , table = Html.table []
    , tableHeader = Html.thead []
    , tableBody = Html.tbody []
    , tableRow = Html.tr []
    , tableHeaderCell =
        \maybeAlignment ->
            let
                attrs =
                    maybeAlignment
                        |> Maybe.map
                            (\alignment ->
                                case alignment of
                                    Block.AlignLeft ->
                                        "left"

                                    Block.AlignCenter ->
                                        "center"

                                    Block.AlignRight ->
                                        "right"
                            )
                        |> Maybe.map Attr.align
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
            in
            Html.th attrs
    , tableCell =
        \maybeAlignment ->
            let
                attrs =
                    maybeAlignment
                        |> Maybe.map
                            (\alignment ->
                                case alignment of
                                    Block.AlignLeft ->
                                        "left"

                                    Block.AlignCenter ->
                                        "center"

                                    Block.AlignRight ->
                                        "right"
                            )
                        |> Maybe.map Attr.align
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
            in
            Html.td attrs
    }
