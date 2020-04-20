module GenerateAndCheckAnchorLinks exposing (main)

import BeautifulExample
import Browser exposing (Document)
import Color
import Html exposing (Attribute, Html)
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


main : Platform.Program () Model Msg
main =
    BeautifulExample.document
        { title = "Do feature-rich Html tag handling in Markdown easily"
        , details = Just "The markdown from this example contains an html tag (quiz) that resolves to an elm widget with access to the model."
        , color = Just (Color.rgb255 69 133 136)
        , maxWidth = 800
        , githubUrl = Nothing
        , documentationUrl = Nothing
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


view : Model -> Document Msg
view model =
    { title = "Elm Markdown Link Checker"
    , body =
        List.concat
            [ [ heading "This Markdown Document:"
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
                |> Result.andThen runRendered
                |> Result.unpack viewError viewMarkdown
            ]
    }


viewMarkdown : List (Html Msg) -> List (Html Msg)
viewMarkdown markdown =
    [ heading "Renders to this Markdown:"
    , hairline
    , Html.section [] markdown
    ]


viewError : String -> List (Html Msg)
viewError errorMessage =
    [ Html.pre [ Attr.style "word-space" "pre-wrap" ]
        [ Html.text errorMessage ]
    ]


heading : String -> Html msg
heading content =
    Html.h2 [] [ Html.text content ]


hairline : Html msg
hairline =
    Html.hr [] []


bulletpoints : List String -> Html msg
bulletpoints =
    List.map (Html.text >> List.singleton >> Html.li []) >> Html.ul []


paragraph : String -> Html msg
paragraph =
    Html.text >> List.singleton >> Html.p []



-- ANCHOR LINK CHECKING


type alias Rendered a =
    { html : Slugs -> Result String a
    , words : List String
    , slugs : Slugs
    }


type alias Slugs =
    List String


runRendered : List (Rendered a) -> Result String (List a)
runRendered rendereds =
    let
        globalSlugs =
            List.concatMap .slugs rendereds

        showList showElement =
            List.map showElement
                >> String.join ", "
                >> (\str -> "[ " ++ str ++ " ]")

        groupedSlugs =
            globalSlugs
                |> List.sort
                |> List.group
                |> List.map (\( a, b ) -> a :: b)
    in
    if List.all (\ls -> List.length ls == 1) groupedSlugs then
        rendereds
            |> List.map (\child -> child.html globalSlugs)
            |> Result.combine

    else
        Err <|
            String.join "\n"
                [ "There are some heading slugs that were generated multiple times."
                , "This means, that if you try to refer to one of these slugs, the"
                , "destination is going to be ambiguous. Maybe you have mistakenly"
                , "added two headings with the same name?"
                , ""
                , "Here is a list of the duplicated slugs that were generated:"
                , groupedSlugs
                    |> List.filter (\ls -> List.length ls > 1)
                    |> showList (showList identity)
                ]


renderedHtmlRenderer :
    Markdown.Html.Renderer (List a -> a)
    -> Markdown.Html.Renderer (List (Rendered a) -> Rendered a)
renderedHtmlRenderer =
    Markdown.Html.map
        (\basicRenderChildren rendereds ->
            { words = List.concatMap .words rendereds
            , slugs = List.concatMap .slugs rendereds
            , html =
                \globalSlugs ->
                    rendereds
                        |> List.map (\child -> child.html globalSlugs)
                        |> Result.combine
                        |> Result.map basicRenderChildren
            }
        )


customHtmlRenderer : Markdown.Renderer (Rendered (Html Msg))
customHtmlRenderer =
    Scaffolded.toRenderer
        { renderHtml = Markdown.Html.oneOf []
        , renderMarkdown = renderMarkdown
        }


renderMarkdown : Scaffolded.Block (Rendered (Html Msg)) -> Rendered (Html Msg)
renderMarkdown block =
    let
        words =
            block
                |> Scaffolded.map .words
                |> Scaffolded.foldWords

        htmlDefault attributes =
            \globalSlugs ->
                block
                    |> Scaffolded.map
                        (\rendered -> rendered.html globalSlugs)
                    |> Scaffolded.foldResults
                    |> Result.map (Scaffolded.bumpHeadings 1 >> Scaffolded.foldHtml attributes)

        renderDefault =
            { html = htmlDefault []
            , words = words
            , slugs = []
            }
    in
    case block of
        Scaffolded.Heading _ ->
            let
                slug =
                    words
                        |> String.join "-"
                        |> String.toLower
            in
            { renderDefault
                | slugs = [ slug ]
                , html = htmlDefault [ Attr.id slug ]
            }

        Scaffolded.Link link ->
            { renderDefault
                | html =
                    \globalSlugs ->
                        if isValidLink globalSlugs link.destination then
                            htmlDefault [] globalSlugs

                        else
                            Err ("Invalid slug link: " ++ link.destination)
            }

        _ ->
            renderDefault


isValidLink : Slugs -> String -> Bool
isValidLink globalSlugs destination =
    not (String.startsWith "#" destination)
        || List.member (String.dropLeft 1 destination) globalSlugs


applyModel : model -> List (model -> view) -> List view
applyModel model =
    List.map (\renderChild -> renderChild model)


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
