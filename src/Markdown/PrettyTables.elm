module Markdown.PrettyTables exposing
    ( reducePrettyTable, finishReduction
    , TableStyle, defaultStyle, compactStyle
    , TableInfo, ColumnInfo
    , resolve, pure
    , map, map2, fold
    , combineColumnInfo
    )

{-|


# Pretty-printing Markdown files with Tables

(Tables defined by the github flavoured markdown (GFM) spec).

Elm-markdown supports GFM tables. The function `reducePretty`
in the `Markdown.Scaffolded` module doesn't support pretty
printing these.

This module includes the function `reducePrettyTable`, which
supports pretty-printing markdown tables.

Due to this being slightly complicated, this functionality was
moved into this module together with its helpers.

@docs reducePrettyTable, finishReduction


## Rendering styles

@docs TableStyle, defaultStyle, compactStyle


## Internal datatypes

@docs TableInfo, ColumnInfo
@docs resolve, pure
@docs map, map2, fold
@docs combineColumnInfo

-}

import Dict exposing (Dict)
import Markdown.Block as Block
import Markdown.Renderer exposing (Renderer)
import Markdown.Scaffolded as Scaffolded
import Maybe.Extra as Maybe


{-| The main datatype that drives pretty-printing.

This allows the pretty-printing to work in a two-step process:

1.  Collect all the pretty-printed cells and their information `ColumnInfo`
2.  Use the maximum cell sizes as column sizes to add padding around cells

-}
type alias TableInfo view =
    { render : Dict Int ColumnInfo -> view
    , info : Dict Int ColumnInfo
    }


{-| All additional column information needed to support pretty-printing.
-}
type alias ColumnInfo =
    { size : Int
    , alignment : Maybe Block.Alignment
    }


{-| Resolve the two-step process that a `TableInfo` encodes by running the steps.
-}
resolve : TableInfo view -> view
resolve { render, info } =
    render info


{-| Make anything a `TableInfo` without adding column information.
-}
pure : view -> TableInfo view
pure rendered =
    { render = \_ -> rendered
    , info = Dict.empty
    }


{-| Transform the value that would be generated in the first step of the `TableInfo`
process by applying a function.
-}
map : (a -> b) -> TableInfo a -> TableInfo b
map f { render, info } =
    { render = \actualSizes -> f (render actualSizes)
    , info = info
    }


{-| Combine two `TableInfo` values using given function.
This does two things:

1.  It combines what the `TableInfo`s render to in their first step using the function
2.  It merges the collected column information from both

-}
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


{-| A helper function for combining two column infos form two cells of the same column.
-}
combineColumnInfo : ColumnInfo -> ColumnInfo -> ColumnInfo
combineColumnInfo info1 info2 =
    { size = max info1.size info2.size
    , alignment =
        info1.alignment
            |> Maybe.orElse info2.alignment
    }


{-| Combine all column information inside a `TableInfo` from a whole list.
-}
fold : List (TableInfo a) -> TableInfo (List a)
fold list =
    List.foldr (map2 (::)) (pure []) list


{-| The style to render tables to. There are two values for this provided for your
convenience (defaultStyle, compactStyle), so check those out before creating a value of
your own.

In case you still want to, you need to provide:

1.  `renderCell`: A function that renders a cell content and applies appropriate padding
    using the given `ColumnInfo`.
2.  `renderDelimiter`: A function that renders the horizontal line between the table header
    and table body. Remember that to write valid markdown, you need to include a pipe (|)
    for each delimiter between columns.
3.  `renderRow`: A function for combining multiple cells in a row. Remember that to write
    valid markdown, you'll need to include a pipe (|) between each cell.

-}
type alias TableStyle =
    { renderCell : String -> ColumnInfo -> String
    , renderDelimiter : List ColumnInfo -> String
    , renderRow : List String -> String
    }


{-| A sensible default for pretty-printing tables.

It will:

1.  Make all cells in a column have uniform width
2.  Left/Center/Right-align cell contents (left by default)
3.  Add a border (|) to the left and right side of your table

-}
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


{-| This style will try to produce valid markdown tables without padding or additional
borders (unlike `defaultStyle`).

However, it's not the minimal amount of characters needed to get a parsing table.

-}
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

See the 'Formatting Markdown Tables' test in the `example/` folder:

1.  It will show you how pretty-printed tables will look like
2.  It shows you how to transform the result of this function to a string
    (See `finishReduction`).

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


{-| Transform the result of `reducePrettyTable` after being put through
`Markdown.render` to a string.

Use it like this:

    markdownBlocks
        |> Markdown.render
            (Scaffolded.toRenderer
                { renderHtml = Markdown.Html.oneOf []
                , renderMarkdown = Tables.reducePrettyTable style
                }
            )
        |> Result.map Tables.finishReduction

-}
finishReduction : List (Int -> TableInfo String) -> String
finishReduction list =
    list
        |> List.map ((|>) 0)
        |> fold
        |> resolve
        |> String.join "\n\n"



-- UTILITIES


grabFrom : Int -> Dict Int a -> List a
grabFrom index dict =
    case Dict.get index dict of
        Just sth ->
            sth :: grabFrom (index + 1) dict

        Nothing ->
            []
