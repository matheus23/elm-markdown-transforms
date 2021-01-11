module Markdown.PrettyTables exposing (..)

import Dict exposing (Dict)
import Markdown.Block as Block
import Markdown.Renderer exposing (Renderer)
import Markdown.Scaffolded as Scaffolded
import Maybe.Extra as Maybe


type alias TableInfo view =
    { render : Dict Int ColumnInfo -> view
    , info : Dict Int ColumnInfo
    }


type alias ColumnInfo =
    { size : Int
    , alignment : Maybe Block.Alignment
    }


resolve : TableInfo view -> view
resolve { render, info } =
    render info


map : (a -> b) -> TableInfo a -> TableInfo b
map f { render, info } =
    { render = \actualSizes -> f (render actualSizes)
    , info = info
    }


map2 : (a -> b -> c) -> TableInfo a -> TableInfo b -> TableInfo c
map2 f tableInfo1 tableInfo2 =
    { render =
        \actualInfo ->
            f
                (tableInfo1.render actualInfo)
                (tableInfo2.render actualInfo)
    , info =
        -- merging dictonary by the maximum values
        Dict.merge
            Dict.insert
            (\col info1 info2 ->
                Dict.insert col (combineColumnInfo info1 info2)
            )
            Dict.insert
            tableInfo1.info
            tableInfo2.info
            Dict.empty
    }


combineColumnInfo : ColumnInfo -> ColumnInfo -> ColumnInfo
combineColumnInfo info1 info2 =
    { size = max info1.size info2.size
    , alignment =
        info1.alignment
            |> Maybe.orElse info2.alignment
    }


pure : view -> TableInfo view
pure rendered =
    { render = \_ -> rendered
    , info = Dict.empty
    }


fold : List (TableInfo a) -> TableInfo (List a)
fold list =
    List.foldr (map2 (::)) (pure []) list


type alias TableStyle =
    { renderCell : String -> ColumnInfo -> String
    , renderDelimiter : List ColumnInfo -> String
    , renderRow : List String -> String
    }


defaultStyle : TableStyle
defaultStyle =
    { renderCell =
        \content info ->
            let
                remainingSpace =
                    info.size - String.length content

                padding =
                    case info.alignment |> Maybe.withDefault Block.AlignLeft of
                        Block.AlignLeft ->
                            { left = 0
                            , right = remainingSpace
                            }

                        Block.AlignRight ->
                            { left = remainingSpace
                            , right = 0
                            }

                        Block.AlignCenter ->
                            { left = floor (toFloat remainingSpace / 2)
                            , right = ceiling (toFloat remainingSpace / 2)
                            }
            in
            String.repeat padding.left " "
                ++ content
                ++ String.repeat padding.right " "
    , renderDelimiter =
        \columnInfos ->
            let
                alignCharacters alignment =
                    case alignment of
                        Just Block.AlignLeft ->
                            { left = ":"
                            , right = "-"
                            }

                        Just Block.AlignRight ->
                            { left = "-"
                            , right = ":"
                            }

                        Just Block.AlignCenter ->
                            { left = ":"
                            , right = ":"
                            }

                        Nothing ->
                            { left = "-"
                            , right = "-"
                            }
            in
            columnInfos
                |> List.map
                    (\columnInfo ->
                        (alignCharacters columnInfo.alignment).left
                            ++ String.repeat columnInfo.size "-"
                            ++ (alignCharacters columnInfo.alignment).right
                    )
                |> String.join "|"
                |> (\str -> "|" ++ str ++ "|")
    , renderRow =
        String.join " | " >> (\str -> "| " ++ str ++ " |")
    }


compactStyle : TableStyle
compactStyle =
    { renderCell =
        \content _ ->
            content
    , renderDelimiter =
        let
            alignCharacters alignment =
                case alignment of
                    Just Block.AlignLeft ->
                        { left = ":"
                        , right = "-"
                        }

                    Just Block.AlignRight ->
                        { left = "-"
                        , right = ":"
                        }

                    Just Block.AlignCenter ->
                        { left = ":"
                        , right = ":"
                        }

                    Nothing ->
                        { left = "-"
                        , right = "-"
                        }
        in
        List.map
            (\columnInfo ->
                (alignCharacters columnInfo.alignment).left
                    ++ "-"
                    ++ (alignCharacters columnInfo.alignment).right
            )
            >> String.join " | "
    , renderRow = String.join " | "
    }


{-| Convert a block of markdown back to markdown text.
(See the 'Formatting Markdown' test in the test suite.)

This just renders one particular style of markdown. Your use-case might need something
completely different. I recommend taking a look at the source code and adapting it to
your needs.

-}
reducePrettyTable : TableStyle -> Scaffolded.Block (Int -> TableInfo String) -> Int -> TableInfo String
reducePrettyTable style block column =
    let
        tableCell maybeAlignment children =
            let
                rendered =
                    children
                        |> List.map ((|>) 0 >> resolve)
                        |> String.concat

                columnInfo =
                    { size = String.length rendered
                    , alignment = maybeAlignment
                    }
            in
            { info =
                Dict.singleton column columnInfo
            , render =
                \info ->
                    info
                        |> Dict.get column
                        |> Maybe.withDefault columnInfo
                        |> style.renderCell rendered
            }
    in
    case block of
        Scaffolded.Table children ->
            children
                -- No column info, so we just say they're all in column 0
                |> List.map ((|>) 0)
                -- make them all coordinate on the column size
                |> fold
                |> (\{ render, info } ->
                        let
                            rendereds =
                                render info

                            headingDelimiter =
                                info
                                    |> grabFrom 0
                                    |> style.renderDelimiter
                        in
                        rendereds
                            |> String.join ("\n" ++ headingDelimiter ++ "\n")
                   )
                |> pure

        Scaffolded.TableHeader children ->
            children
                |> List.map ((|>) 0)
                |> fold
                |> map (String.join "\n")

        Scaffolded.TableBody children ->
            children
                |> List.map ((|>) 0)
                |> fold
                |> map (String.join "\n")

        Scaffolded.TableRow children ->
            children
                |> List.indexedMap (\index expectsColumnIndex -> expectsColumnIndex index)
                |> fold
                |> map style.renderRow

        Scaffolded.TableHeaderCell maybeAlignment children ->
            tableCell maybeAlignment children

        Scaffolded.TableCell maybeAlignment children ->
            tableCell maybeAlignment children

        other ->
            other
                |> Scaffolded.map ((|>) 0 >> resolve)
                |> Scaffolded.reducePretty
                |> pure



-- UTILITIES


grabFrom : Int -> Dict Int a -> List a
grabFrom index dict =
    case Dict.get index dict of
        Just sth ->
            sth :: grabFrom (index + 1) dict

        Nothing ->
            []
