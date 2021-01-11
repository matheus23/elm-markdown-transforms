module FormatTables exposing (main)

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
import Markdown.PrettyTables as Tables
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
    """# Tables

| foo | bar |
| --- | --- |
| baz | bim |

(Yes, this is valid markdown)

| abc | defghi |
:-: | -----------:
bar | baz
"""


view : Model -> Document Msg
view model =
    { title = "Format Markdown Tables"
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
                |> Result.map
                    (List.map ((|>) 0)
                        >> Tables.fold
                        >> Tables.resolve
                        >> String.join "\n\n"
                    )
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


customHtmlRenderer : Markdown.Renderer (Int -> Tables.TableInfo String)
customHtmlRenderer =
    Scaffolded.toRenderer
        { renderHtml = Markdown.Html.oneOf []
        , renderMarkdown = Tables.reducePrettyTable Tables.defaultStyle
        }



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
