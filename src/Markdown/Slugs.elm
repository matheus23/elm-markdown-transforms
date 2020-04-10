module Markdown.Slugs exposing (..)

{-| -}

import List.Extra as List
import Markdown.Scaffolded as Scaffolded
import Result.Extra as Result


type alias Rendered view =
    { html : Slugs -> Result Error view
    , words : List String
    , slugs : Slugs
    }


type alias Slugs =
    List String


type Error
    = DuplicatedSlugs (List Slugs)
    | UnknownSlugLink String


resolve : List (Rendered a) -> Result Error (List a)
resolve rendereds =
    let
        globalSlugs =
            List.concatMap .slugs rendereds

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
        groupedSlugs
            |> List.filter (\ls -> List.length ls > 1)
            |> DuplicatedSlugs
            |> Err


errorToString : Error -> String
errorToString error =
    let
        withParens str =
            "[ " ++ str ++ " ]"
    in
    case error of
        DuplicatedSlugs slugs ->
            String.join "\n"
                [ "There are duplicate heading slugs on this page:"
                , ""
                , slugs
                    |> List.map (String.join ", " >> withParens)
                    |> String.join "\n"
                ]

        UnknownSlugLink link ->
            "Invalid slug link: " ++ link


isValidLink : Slugs -> String -> Bool
isValidLink globalSlugs destination =
    not (String.startsWith "#" destination)
        || List.member (String.dropLeft 1 destination) globalSlugs
