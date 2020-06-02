module Markdown.AnchorValidation exposing
    ( Validated, Anchors, Error(..)
    , fold
    , map, mapWithGeneratedAnchor, mapWithCustomGeneratedAnchor
    , validateLink
    , liftHtmlRenderer
    , resolve, errorToString
    )

{-|


# Validate internal markdown links

These internal markdown links (what appear when you link to sections in wikipedia, for
example) are called 'Anchor Links'.

Check out the [example](https://elm-markdown-transforms.netlify.app/generate-and-check-anchor-links.html)
that uses this module and it's [source code](https://github.com/matheus23/elm-markdown-transforms/blob/master/examples/src/GenerateAndCheckAnchorLinks.elm).

@docs Validated, Anchors, Error


## Render with Validation

@docs fold

@docs map, mapWithGeneratedAnchor, mapWithCustomGeneratedAnchor

@docs validateLink

@docs liftHtmlRenderer


## Resolve Validation

@docs resolve, errorToString

-}

import List.Extra as List
import Markdown.Html
import Markdown.Scaffolded as Scaffolded
import Result.Extra as Result
import ResultME exposing (ResultME)


{-| The type we use for folds. You don't need to worry about it. You can use the API for
working with it exclusively. That is, `resolve`, `fold`, the various `map`s and
`validateLink`.

If you're curious what this type is about:
Imagine anchor link checking to work in two phases:

1.  The markdown gets reduced to the list of words and all used anchors at the same time
    (the `words` and `generatedAnchors` fields).
2.  With the information about what anchors there are in the markdown, we can now render
    it, validating that our links are not invalid at the same time (the `validate` field).

-}
type alias Validated view =
    { validate : Anchors -> ResultME Error view
    , words : List String
    , generatedAnchors : Anchors
    }


{-| We model all existing anchors simply as a list of strings.
-}
type alias Anchors =
    List String


{-| Anchor link checking can go wrong in two ways:

  - `DuplicatedAnchors`: We generated two headings or so with the same anchor.
    This is an issue, since linking to one of them won't work.
  - `InvalidAnchorLink`: We generated a link to an anchor that doesn't exist.

-}
type Error
    = DuplicatedAnchors (List Anchors)
    | InvalidAnchorLink String


{-| Resolve validation errors
-}
resolve : List (Validated a) -> ResultME Error (List a)
resolve validations =
    let
        allGeneratedAnchors =
            List.concatMap .generatedAnchors validations

        groupedAnchors =
            allGeneratedAnchors
                |> List.sort
                |> List.group
                |> List.map (\( a, b ) -> a :: b)
    in
    if List.all (\ls -> List.length ls == 1) groupedAnchors then
        validations
            |> List.map (\{ validate } -> validate allGeneratedAnchors)
            |> ResultME.combineList

    else
        groupedAnchors
            |> List.filter (\ls -> List.length ls > 1)
            |> DuplicatedAnchors
            |> ResultME.error


{-| Generate fairly descriptive error messages
-}
errorToString : Error -> String
errorToString error =
    let
        withParens str =
            "[ " ++ str ++ " ]"
    in
    case error of
        DuplicatedAnchors generatedAnchors ->
            String.join "\n"
                [ "There are some heading anchors that were generated multiple times."
                , "This means, that if you try to refer to one of these anchors, the"
                , "destination is going to be ambiguous. Maybe you have mistakenly"
                , "added two headings with the same name?"
                , ""
                , "Here is a list of the duplicated anchors that were generated:"
                , generatedAnchors
                    |> List.map (String.join ", " >> withParens)
                    |> String.join "\n"
                ]

        InvalidAnchorLink link ->
            "Invalid slug link: " ++ link


{-| If you just started building in your anchor validation, but haven't updated your
Html renderers to reduce to `Validated` values, just use this function to lift them
automatically.
-}
liftHtmlRenderer :
    Markdown.Html.Renderer (List a -> a)
    -> Markdown.Html.Renderer (List (Validated a) -> Validated a)
liftHtmlRenderer =
    Markdown.Html.map
        (\basicRenderChildren validated ->
            { words = List.concatMap .words validated
            , generatedAnchors = List.concatMap .generatedAnchors validated
            , validate =
                \allGeneratedAnchors ->
                    validated
                        |> List.map (\{ validate } -> validate allGeneratedAnchors)
                        |> Result.combine
                        |> Result.map basicRenderChildren
            }
        )


{-| Fold a Validated value through your block.

If you don't know how to use this function, take a look at the [example].

If you want to know more about what `fold`s are, take a look at the [docs for
`Scaffolded`].

[example]: https://github.com/matheus23/elm-markdown-transforms/blob/master/examples/src/GenerateAndCheckAnchorLinks.elm
[docs for `Scaffolded`]: Markdown/Scaffolded#what-are-folds-

-}
fold : Scaffolded.Block (Validated view) -> Validated (Scaffolded.Block view)
fold block =
    { validate =
        \allGeneratedAnchors ->
            block
                |> Scaffolded.map (\{ validate } -> validate allGeneratedAnchors)
                |> Scaffolded.foldResultME
    , words =
        block
            |> Scaffolded.map .words
            |> Scaffolded.reduceWords
    , generatedAnchors =
        block
            |> Scaffolded.map .generatedAnchors
            -- TODO Maybe this function is useful on its own? so extract `accumulate`?
            |> Scaffolded.reduce
                { accumulate = List.concat
                , extract = always []
                }
    }


{-| Map over the view inside a `Validated` value.

You can use this to construct a view function

    myViewReducer : Scaffolded.Block (Html Msg) -> Html Msg
    myViewReducer =
        -- or any other implementation
        Scaffolded.reduceHtml []

    viewValidated : Scaffolded.Block (Validated (Html Msg)) -> Validated (Html Msg)
    viewValidated block =
        block
            |> fold
            -- Now, we have a `Validated (Scaffolded.Block (Html Msg))`
            |> map myViewReducer

-}
map : (a -> b) -> Validated a -> Validated b
map f { validate, generatedAnchors, words } =
    { validate = validate >> Result.map f
    , generatedAnchors = generatedAnchors
    , words = words
    }


{-| Map over the `Validated` value and generate an anchor at the same time!

    viewValidated : Scaffolded.Block (Validated (Html Msg)) -> Validated (Html Msg)
    viewValidated block =
        case block of
            Scaffolded.Heading _ ->
                block
                    |> fold
                    |> mapWithGeneratedAnchor
                        (\anchor -> Scaffolded.reduceHtml [ Attr.id anchor ])

            _ ->
                block
                    |> fold
                    |> map (Scaffolded.reduceHtml [])

(See also the [docs for `map`].)

The extracted words from markdown here are transformed into an 'anchor', which is a
string, consisting only of the alphanumeric characters of the contained markdown joined by
dashes.

[docs for `map`]: #map

-}
mapWithGeneratedAnchor : (String -> a -> b) -> Validated a -> Validated b
mapWithGeneratedAnchor =
    let
        wordsToAnchor : List String -> String
        wordsToAnchor words =
            words
                |> List.map (String.filter Char.isAlphaNum)
                |> List.filter (not << String.isEmpty)
                |> String.join "-"
                |> String.toLower
    in
    mapWithCustomGeneratedAnchor wordsToAnchor


{-| Same as [`mapWithGeneratedAnchor`], but you decide how to extract an anchor link from
the words inside markdown.

[`mapWithGeneratedAnchor`]: #mapWithGeneratedAnchor

-}
mapWithCustomGeneratedAnchor : (List String -> String) -> (String -> a -> b) -> Validated a -> Validated b
mapWithCustomGeneratedAnchor anchorFromWords f { validate, generatedAnchors, words } =
    let
        anchor =
            anchorFromWords words
    in
    { validate = validate >> Result.map (f anchor)

    -- This results in the anchor list being reversed, which might be confusing
    , generatedAnchors = anchor :: generatedAnchors
    , words = words
    }


{-| Validate given anchor link, to make sure it exists.

    viewValidated : Scaffolded.Block (Validated (Html Msg)) -> Validated (Html Msg)
    viewValidated block =
        case block of
            Scaffolded.Link { destination } ->
                block
                    |> fold
                    |> validateLink destination
                    |> map (Scaffolded.foldHtml [])

            Scaffolded.Heading _ ->
                block
                    |> fold
                    |> mapWithGeneratedAnchor
                        (\anchor -> Scaffolded.foldHtml [ Attr.id anchor ])

            _ ->
                block
                    |> fold
                    |> map (Scaffolded.foldHtml [])

(See also the [docs for `map`](#map) and [docs for `mapWithGeneratedAnchor`](#mapWithGeneratedAnchor).)

-}
validateLink : String -> Validated view -> Validated view
validateLink link { validate, words, generatedAnchors } =
    let
        isValidLink : Anchors -> String -> Bool
        isValidLink allGeneratedAnchors destination =
            not (String.startsWith "#" destination)
                || List.member (String.dropLeft 1 destination) allGeneratedAnchors
    in
    { words = words
    , generatedAnchors = generatedAnchors
    , validate =
        \allGeneratedAnchors ->
            if isValidLink allGeneratedAnchors link then
                validate allGeneratedAnchors

            else
                ResultME.error (InvalidAnchorLink link)
    }



-- "Applicative" interface
-- If you find uses for this, please write an issue or contact the author in any other way


succeed : view -> Validated view
succeed view =
    { words = []
    , generatedAnchors = []
    , validate = \_ -> Ok view
    }


andMap : Validated a -> Validated (a -> b) -> Validated b
andMap a f =
    { generatedAnchors = a.generatedAnchors ++ f.generatedAnchors
    , words = a.words ++ f.words
    , validate =
        \allGeneratedAnchors ->
            f.validate allGeneratedAnchors
                |> Result.andMap (a.validate allGeneratedAnchors)
    }
