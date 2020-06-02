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
        , renderMarkdown = Scaffolded.reducePretty
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
