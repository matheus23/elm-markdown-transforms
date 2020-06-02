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


{-| -}
type alias Validated view =
    { validate : Anchors -> Result Error view
    , words : List String
    , generatedAnchors : Anchors
    }


{-| -}
type alias Anchors =
    List String


{-| -}
type Error
    = DuplicatedAnchors (List Anchors)
    | InvalidAnchorLink String


{-| -}
resolve : (Error -> error) -> List (Validated a) -> Result error (List a)
resolve handleErrors validations =
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
            |> Result.combine
            |> Result.mapError handleErrors

    else
        groupedAnchors
            |> List.filter (\ls -> List.length ls > 1)
            |> DuplicatedAnchors
            |> handleErrors
            |> Err


{-| -}
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


{-| -}
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


{-| -}
fold : Scaffolded.Block (Validated view) -> Validated (Scaffolded.Block view)
fold block =
    { validate =
        \allGeneratedAnchors ->
            block
                |> Scaffolded.map (\{ validate } -> validate allGeneratedAnchors)
                |> Scaffolded.foldResults
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


{-| -}
map : (a -> b) -> Validated a -> Validated b
map f { validate, generatedAnchors, words } =
    { validate = validate >> Result.map f
    , generatedAnchors = generatedAnchors
    , words = words
    }


{-| -}
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


{-| -}
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


{-| -}
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
                Err (InvalidAnchorLink link)
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
