module GenerateAndCheckAnchorLinks exposing (main)

import BeautifulExample
import Browser exposing (Document)
import Color
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import List.Extra as List
import Markdown.AnchorValidation as AnchorValidation
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
    """# Anchor Links

I bet you know that you can link to specific sections of wikipedia pages, right?
The way to do this with html is with anchor links. If the url in your browser ends with
something like `#history`, then the browser would jump to the first html element that has
an attribute `id="history"` set.

However, if we write markdown, we don't want to have to provide id attributes ourselves,
wikipedia editors don't have to do that as well!

We can generate them with `elm-markdown-transforms` by looking at what words a heading
contains and attaching a fitting id attribute. This way, we can make sure that links like
[`#anchor-links`](#anchor-links) work.

However, what if we wrote `#anchor-link` instead? We wouldn't notice this mistake in our
markdown, and it just wouldn't work!

Try changing the above link to be invalid, you'll notice it'll give an error message.
This is done by making sure all anchor links only refer to anchors that were generated.

This is quite an interesting and advanced transformation.

Oh, and we also check that we don't generate duplicated anchor links! :)
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
                |> Result.andThen (AnchorValidation.resolve AnchorValidation.errorToString)
                |> Result.unpack viewError viewMarkdown
            ]
    }


viewMarkdown : List (Html Msg) -> List (Html Msg)
viewMarkdown markdown =
    [ Html.h2 [] [ Html.text "Renders to this Markdown:" ]
    , Html.hr [] []
    , Html.section [] markdown
    ]


viewError : String -> List (Html Msg)
viewError errorMessage =
    [ Html.pre [ Attr.style "word-space" "pre-wrap" ]
        [ Html.text errorMessage ]
    ]



-- ANCHOR LINK CHECKING


customHtmlRenderer : Markdown.Renderer (AnchorValidation.Validated (Html Msg))
customHtmlRenderer =
    Scaffolded.toRenderer
        { renderHtml = Markdown.Html.oneOf []
        , renderMarkdown = renderMarkdown
        }


renderMarkdown : Scaffolded.Block (AnchorValidation.Validated (Html Msg)) -> AnchorValidation.Validated (Html Msg)
renderMarkdown block =
    case block of
        -- For headings we generate anchors with `mapWithGeneratedAnchor`
        Scaffolded.Heading _ ->
            block
                |> AnchorValidation.fold
                |> AnchorValidation.mapWithGeneratedAnchor
                    (\anchor -> Scaffolded.foldHtml [ Attr.id anchor ])

        -- For links we validate, that their links are fine. validateLink would generate
        -- an error otherwise.
        -- validateLink also only validates links that start with "#".
        Scaffolded.Link { destination } ->
            block
                |> AnchorValidation.fold
                |> AnchorValidation.validateLink destination
                |> AnchorValidation.map (Scaffolded.foldHtml [])

        -- Anything else just propagates the validation, but doesn't do anything special
        _ ->
            block
                |> AnchorValidation.fold
                |> AnchorValidation.map (Scaffolded.foldHtml [])



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
        { title = "Do feature-rich Html tag handling in Markdown easily"
        , details = Just "The markdown from this example contains an html tag (quiz) that resolves to an elm widget with access to the model."
        , color = Just (Color.rgb255 69 133 136)
        , maxWidth = 800
        , githubUrl = Just "https://github.com/matheus23/elm-markdown-transforms/blob/master/examples/src/GenerateAndCheckAnchorLinks.elm"
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
