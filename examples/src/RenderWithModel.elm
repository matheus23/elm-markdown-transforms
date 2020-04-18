module RenderWithModel exposing (main)

import BeautifulExample
import Color
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Markdown.Block as Markdown
import Markdown.Html as MarkdownHtml
import Markdown.Parser as Markdown
import Markdown.Renderer as Markdown
import Markdown.Scaffolded as Scaffolded
import Result.Extra as Result


exampleMarkdown : String
exampleMarkdown =
    """# This Markdown contains a Quiz

We can write interactive markdown by rendering to a function.

Using `elm-markdown-transforms` we parameterize our markdown rendering with our `Model`.
The markdown is essentially rendered to a function of type `Model -> Html Msg`.

So let's do a quiz:

## Which of these icons **is not** included in [Steve Schoger's 'Heroicons'](https://www.heroicons.com/) icon library?
* [ ] This Download Icon: ![Download Icon](/resources/md-download.svg)
* [ ] This Trash Icon: ![Trash Icon](/resources/md-trash.svg)
* [X] This Move Icon: ![Move Icon](/resources/md-move.svg)
* [ ] This Sun Icon: ![Sun Icon](/resources/md-sun.svg)
"""



-- MODEL


type alias Model =
    { revealed : Bool }


type Msg
    = Reveal


update : Msg -> Model -> Model
update Reveal model =
    { model | revealed = True }


renderedMarkdown : Result String (List (Model -> Html Msg))
renderedMarkdown =
    exampleMarkdown
        |> Markdown.parse
        |> Result.mapError (List.map Markdown.deadEndToString >> String.join "\n")
        |> Result.andThen
            (Markdown.render
                (Scaffolded.toRenderer
                    { renderHtml = MarkdownHtml.oneOf []
                    , renderMarkdown = Scaffolded.parameterized renderMarkdown
                    }
                )
            )


view : Model -> Html Msg
view model =
    Html.main_ [] <|
        case renderedMarkdown of
            Err message ->
                viewError message

            Ok blockRenders ->
                blockRenders
                    |> List.map (\render -> render model)


renderMarkdown : Scaffolded.Block (Html Msg) -> Model -> Html Msg
renderMarkdown markdown model =
    case markdown of
        -- We handle unordered lists specially, so we can generate events and not reveal
        -- the answer before the users has chosen one.
        Scaffolded.UnorderedList { items } ->
            items
                |> List.map
                    (\(Markdown.ListItem task children) ->
                        case task of
                            Markdown.NoTask ->
                                Html.li [] children

                            _ ->
                                Html.li []
                                    (Html.input
                                        [ Attr.type_ "checkbox"
                                        , Events.onClick Reveal
                                        , Attr.checked (task == Markdown.CompletedTask && model.revealed)
                                        , Attr.style "margin-right" "4px"
                                        ]
                                        []
                                        :: children
                                    )
                    )
                |> Html.ul []

        -- any other markdown gets rendered in the standard way
        _ ->
            Scaffolded.foldHtml [] markdown


viewError : String -> List (Html Msg)
viewError errorMessage =
    [ Html.pre [ Attr.style "word-space" "pre-wrap" ]
        [ Html.text errorMessage ]
    ]



-- MAIN


main : Platform.Program () Model Msg
main =
    BeautifulExample.sandbox
        { title = "Render Markdown to Interactive Views"
        , details = Just "The markdown from this example renders to a function of type 'Model -> Html Msg' so that it can be interactive."
        , color = Just (Color.rgb255 69 133 136)
        , maxWidth = 800
        , githubUrl = Nothing
        , documentationUrl = Nothing
        }
        { init = { revealed = False }
        , view = view
        , update = update
        }
