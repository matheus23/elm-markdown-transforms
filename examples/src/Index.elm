module Index exposing (main)

import BeautifulExample
import Color
import Html exposing (Html)
import Markdown.Block as Markdown
import Markdown.Parser as Markdown
import Markdown.Renderer as Markdown
import Result.Extra as Result


indexText : Html msg
indexText =
    """
Check out the examples:
* [Render your markdown to an interactive function of type `Model -> Html Msg`](/render-with-model.html) ([Source](https://github.com/matheus23/elm-markdown-transforms/blob/master/examples/src/RenderWithModel.elm))
* [Extract all external links from a markdown file and check whether they're dead or not](/check-links-via-http.html) ([Source](https://github.com/matheus23/elm-markdown-transforms/blob/master/examples/src/CheckLinksViaHttp.elm))
* [Check markdown anchor links for duplicates and valid internal linking](/generate-and-check-anchor-links.html) ([Source](https://github.com/matheus23/elm-markdown-transforms/blob/master/examples/src/GenerateAndCheckAnchorLinks.elm))
* [Format Markdown files (print them back to parsable markdown text)](/format-markdown.html) ([Source](https://github.com/matheus23/elm-markdown-transforms/blob/master/examples/src/FormatMarkdown.elm))

Need more information? Check out [the documentation](https://package.elm-lang.org/packages/matheus23/elm-markdown-transforms/latest).

More examples to come! If you have any use case in mind, don't hesitate contacting me by
writing an issue on [github](https://github.com/matheus23/elm-markdown-transforms/issues).
"""
        |> Markdown.parse
        |> Result.mapError (List.map Markdown.deadEndToString >> String.join "\n")
        |> Result.andThen (Markdown.render Markdown.defaultHtmlRenderer)
        |> Result.map (Html.main_ [])
        |> Result.withDefault (Html.text "Error occurred. This shouldn't happen.")


main : Platform.Program () {} ()
main =
    BeautifulExample.sandbox
        { title = "elm-markdown-transforms examples"
        , details = Nothing
        , color = Just (Color.rgb255 69 133 136)
        , maxWidth = 800
        , githubUrl = Just "https://github.com/matheus23/elm-markdown-transforms"
        , documentationUrl = Just "https://package.elm-lang.org/packages/matheus23/elm-markdown-transforms/latest"
        }
        { init = {}
        , view = always indexText
        , update = \() _ -> {}
        }
