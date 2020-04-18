port module CheckLinksViaHttp exposing (main)

import BeautifulExample
import Browser exposing (Document)
import Color
import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Html.Events as Events
import Markdown.Block as Markdown
import Markdown.Html as Markdown
import Markdown.Parser as Markdown
import Markdown.Renderer as Markdown
import Markdown.Scaffolded as Scaffolded
import Result.Extra as Result


type alias Model =
    { markdown : String
    , parsed : Result String (List Markdown.Block)
    , linkValidity : Maybe LinkValidity
    }


type alias LinkValidity =
    { valid : List String, invalid : List String }


type Msg
    = MarkdownChanged String
    | CheckLinks (List String)
    | SuccessfullyPinged String
    | UnsuccessfullyPinged String


exampleMarkdown : String
exampleMarkdown =
    """# This is an example file

There are some valid links, so for example a link to https://example.org, both inline, and
also [non-inline](https://example.org).

The thing is: You can have dead links, without noticing it!
Which one of these is dead?
* A link to the [elm packages website](https://packages.elm-lang.org)
* A link to the docs of [elm-markdown-transforms](https://package.elm-lang.org/packages/matheus23/elm-markdown-transforms/1.0.0/)
* Or, a link to [a cool talk](https://www.youtube.com/watch?v=mrwn2HuWUiA)

We don't check for invalid [hash links](#invalid-link), though."""


main : Platform.Program () Model Msg
main =
    BeautifulExample.document
        { title = "Check Markdown files for dead links using elm-markdown-transforms"
        , details = Just "Input some markdown and it will run elm/http requests and check for any dead links that start with 'http'."
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
                    , linkValidity = Nothing
                    }
        , view = view
        , update = update
        , subscriptions =
            always <|
                Sub.batch
                    [ successfullyPinged SuccessfullyPinged
                    , unsuccessfullyPinged UnsuccessfullyPinged
                    ]
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
                |> Result.andThen computeMarkdownInfo
                |> Result.unpack viewError viewInfo
            , model.linkValidity
                |> Maybe.map viewLinkValidity
                |> Maybe.withDefault []
            ]
    }


type alias MarkdownInfo =
    { html : List (Html Msg)
    , links : List String
    }


computeMarkdownInfo : List Markdown.Block -> Result String MarkdownInfo
computeMarkdownInfo blocks =
    Ok MarkdownInfo
        |> Result.andMap (Markdown.render Markdown.defaultHtmlRenderer blocks)
        |> Result.andMap
            (blocks
                |> Markdown.render
                    (Scaffolded.toRenderer
                        { renderHtml = Markdown.oneOf []
                        , renderMarkdown = extractHttpLinks
                        }
                    )
                |> Result.map List.concat
            )


extractHttpLinks : Scaffolded.Block (List String) -> List String
extractHttpLinks =
    Scaffolded.reduce
        { accumulate = List.concat
        , extract =
            \block ->
                case block of
                    Scaffolded.Link { destination } ->
                        if destination |> String.startsWith "http" then
                            [ destination ]

                        else
                            []

                    _ ->
                        []
        }


viewInfo : MarkdownInfo -> List (Html Msg)
viewInfo { html, links } =
    [ heading "Renders to this Markdown:"
    , hairline
    , Html.section [] html
    , hairline
    , heading "Using elm-markdown-transforms, we extracted following links:"
    , bulletpoints links
    , Html.text "Check, whether these links can be pinged: "
    , Html.button [ Events.onClick (CheckLinks links) ] [ Html.text "Check Links" ]
    ]


viewError : String -> List (Html Msg)
viewError errorMessage =
    [ Html.pre [ Attr.style "word-space" "pre-wrap" ]
        [ Html.text errorMessage ]
    ]


viewLinkValidity : LinkValidity -> List (Html Msg)
viewLinkValidity { valid, invalid } =
    [ heading "These links are valid:"
    , bulletpoints valid
    , heading "These links couldn't be pinged:"
    , bulletpoints invalid
    , paragraph """This Example's link checking doesn't seem very reliable. Just keep this
in mind. If you get a weird result, just click the 'Check Links' button above, again."""
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

        CheckLinks links ->
            ( { model | linkValidity = Nothing }
            , Cmd.batch (List.map verifyLink links)
            )

        SuccessfullyPinged link ->
            ( { model | linkValidity = Just (addValidLink link model.linkValidity) }
            , Cmd.none
            )

        UnsuccessfullyPinged link ->
            ( { model | linkValidity = Just (addInvalidLink link model.linkValidity) }
            , Cmd.none
            )


addValidLink : String -> Maybe LinkValidity -> LinkValidity
addValidLink link maybeValids =
    case maybeValids of
        Just { valid, invalid } ->
            { valid = valid ++ [ link ], invalid = invalid }

        Nothing ->
            { valid = [ link ], invalid = [] }


addInvalidLink : String -> Maybe LinkValidity -> LinkValidity
addInvalidLink link maybeValids =
    case maybeValids of
        Just { valid, invalid } ->
            { valid = valid, invalid = invalid ++ [ link ] }

        Nothing ->
            { valid = [], invalid = [ link ] }


port verifyLink : String -> Cmd msg


port successfullyPinged : (String -> msg) -> Sub msg


port unsuccessfullyPinged : (String -> msg) -> Sub msg
