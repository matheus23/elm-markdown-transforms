module FormatMarkdown exposing (main)

import BeautifulExample
import Browser exposing (Document)
import Color
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import List.Extra as List
import Markdown.Block as Markdown
import Markdown.Html
import Markdown.Parser as Markdown
import Markdown.Renderer as Markdown
import Markdown.Scaffolded as Scaffolded
import Result.Extra as Result


type alias Model =
    { markdown : String
    , parsed : Result String (List Markdown.Block)
    }


type Msg
    = MarkdownChanged String


exampleMarkdown : String
exampleMarkdown =
    """# Format _beautiful_ Markdown

> Markdown is only fine
> When *pretty printed*
> It has to be **BEAUTIFUL** I SAID.
## Links
Let's try [a `link`](https://example.com)

What about images? ![alt text](https://github.com/adam-p/markdown-here/raw/master/src/common/images/icon48.png "Logo Title Text 1")



## Lists

Markdown formatting is:
* [ ] Stupid
* [X] Okay-ish
* [ ] Works
- Let's not do checkboxes.

1. Don't use this project
1. What are you doing?
1. Ok. Please go now.

---------

0. Write library
1. Publish
2. ???
3. Profit!

## Code blocks

```elm
viewMarkdown : String -> List (Html Msg)
viewMarkdown markdown =
    [ Html.h2 [] [ Html.text "Prettyprinted:" ]
    , Html.hr [] []
    , Html.pre [ Attr.style "word-wrap" "pre-wrap" ] [ Html.text markdown ]
    ]
```

```
Please use more beautiful
Code blocks!
```
"""


view : Model -> Document Msg
view model =
    { title = "Elm Markdown Link Checker"
    , body =
        List.concat
            [ [ Html.h2 [] [ Html.text "This Markdown Document:" ]
              , Html.textarea
                    [ Attr.style "height" "400px"
                    , Attr.style "min-width" "100%"
                    , Attr.style "max-width" "100%"
                    , Attr.style "font-family" "monospace"
                    , Attr.value model.markdown
                    , Events.onInput MarkdownChanged
                    ]
                    []
              ]
            , model.parsed
                |> Result.andThen (Markdown.render customHtmlRenderer)
                |> Result.map (String.join "\n\n")
                |> Result.unpack viewError viewMarkdown
            ]
    }


viewMarkdown : String -> List (Html Msg)
viewMarkdown markdown =
    [ Html.h2 [] [ Html.text "Prettyprinted:" ]
    , Html.hr [] []
    , Html.pre [ Attr.style "white-space" "pre-wrap" ] [ Html.text markdown ]
    ]


viewError : String -> List (Html Msg)
viewError errorMessage =
    [ Html.pre [ Attr.style "white-space" "pre-wrap" ]
        [ Html.text errorMessage ]
    ]



-- FORMATTING


customHtmlRenderer : Markdown.Renderer String
customHtmlRenderer =
    Scaffolded.toRenderer
        { renderHtml = Markdown.Html.oneOf []
        , renderMarkdown = Scaffolded.foldPretty
        }


foldPretty : Scaffolded.Block String -> String
foldPretty block =
    let
        escape toEscape =
            String.replace toEscape ("\\" ++ toEscape)
    in
    case block of
        Scaffolded.Heading { level, children } ->
            (case level of
                Markdown.H1 ->
                    "# "

                Markdown.H2 ->
                    "## "

                Markdown.H3 ->
                    "### "

                Markdown.H4 ->
                    "#### "

                Markdown.H5 ->
                    "##### "

                Markdown.H6 ->
                    "###### "
            )
                ++ String.concat children

        Scaffolded.Text content ->
            content

        Scaffolded.Paragraph children ->
            String.concat children

        Scaffolded.BlockQuote children ->
            children
                |> String.concat
                |> String.split "\n"
                |> List.map (\line -> "> " ++ line)
                |> String.join "\n"

        Scaffolded.Strong children ->
            -- TODO Escaping
            "**" ++ String.replace "**" "\\**" (String.concat children) ++ "**"

        Scaffolded.Emphasis children ->
            "_" ++ escape "_" (String.concat children) ++ "_"

        Scaffolded.CodeSpan content ->
            "`" ++ content ++ "`"

        Scaffolded.Link { destination, children } ->
            "[" ++ escape "]" (escape ")" (String.concat children)) ++ "](" ++ destination ++ ")"

        Scaffolded.Image { alt, src } ->
            "![" ++ escape "]" (escape ")" alt) ++ "](" ++ src ++ ")"

        Scaffolded.UnorderedList { items } ->
            items
                |> List.map
                    (\(Markdown.ListItem task children) ->
                        case task of
                            Markdown.NoTask ->
                                " - " ++ String.concat children

                            Markdown.IncompleteTask ->
                                " - [ ] " ++ String.concat children

                            Markdown.CompletedTask ->
                                " - [X] " ++ String.concat children
                    )
                |> String.join "\n"

        Scaffolded.OrderedList { startingIndex, items } ->
            items
                |> List.indexedMap
                    (\index children ->
                        String.fromInt (index + startingIndex)
                            ++ ". "
                            ++ String.concat children
                    )
                |> String.join "\n"

        Scaffolded.CodeBlock { body, language } ->
            case language of
                Just langName ->
                    "```"
                        ++ langName
                        ++ "\n"
                        ++ body
                        ++ "\n```"

                Nothing ->
                    let
                        bodyLines =
                            body
                                |> String.split "\n"
                    in
                    if bodyLines |> List.any (not << String.startsWith " ") then
                        bodyLines
                            |> List.map ((++) "    ")
                            |> String.join "\n"

                    else
                        "```\n" ++ body ++ "```"

        Scaffolded.HardLineBreak ->
            "\n\n"

        Scaffolded.ThematicBreak ->
            "--------------------\n"

        -- Currently, elm-markdown doesn't support tables (they're not parsed)
        Scaffolded.Table _ ->
            ""

        Scaffolded.TableHeader _ ->
            ""

        Scaffolded.TableBody _ ->
            ""

        Scaffolded.TableRow _ ->
            ""

        Scaffolded.TableCell _ ->
            ""

        Scaffolded.TableHeaderCell _ _ ->
            ""



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MarkdownChanged newMarkdown ->
            ( { model
                | markdown = newMarkdown
                , parsed =
                    newMarkdown
                        |> Markdown.parse
                        |> Result.mapError (List.map Markdown.deadEndToString >> String.join "\n")
              }
            , Cmd.none
            )



-- MAIN


main : Platform.Program () Model Msg
main =
    BeautifulExample.document
        { title = "Format markdown by printing it back to text."
        , details = Nothing
        , color = Just (Color.rgb255 69 133 136)
        , maxWidth = 800
        , githubUrl = Just "https://github.com/matheus23/elm-markdown-transforms/blob/master/examples/src/FormatMarkdown.elm"
        , documentationUrl = Just "https://package.elm-lang.org/packages/matheus23/elm-markdown-transforms/latest"
        }
        { init =
            \() ->
                update (MarkdownChanged exampleMarkdown)
                    { markdown = ""
                    , parsed = Err "This should be impossible. Report a bug if you see this."
                    }
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
