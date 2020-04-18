module Markdown.Scaffolded exposing
    ( Block(..)
    , map
    , parameterized, validating, withStaticHttpRequests
    , foldHtml, foldFunction, foldWords, foldResults, foldStaticHttpRequests
    , reduce
    , fromRenderer, toRenderer
    , bumpHeadings
    )

{-|


# Rendering Markdown with Scaffolds and Folds

(This is called recursion-schemes in other languages, but don't worry, you don't have to
write recursive functions!)

This is module provides a more **complicated**, but also **more powerful and
composable** way of rendering markdown than the built-in elm-markdown
[`Renderer`](https://package.elm-lang.org/packages/dillonkearns/elm-markdown/latest/Markdown-Renderer).

If you feel a little overwhelmed with this module at first, I recommend taking a look at
the [What are folds?](#what-are-folds-) section.


# Main Datastructure

@docs Block

@docs map


# High-level Transformations

These functions are not as composable as [fold building blocks](#fold-building-blocks),
but might suffice for your use case. Take a look at the other section if you find you need
something better.

@docs parameterized, validating, withStaticHttpRequests


# Fold Building Blocks

@docs foldHtml, foldFunction, foldWords, foldResults, foldStaticHttpRequests
@docs reduce


### What are folds?

Often, we're working with functions of the type `Block view -> view`, where `view` might
be something like `Html Msg` or `String`, etc. or, generally, functions of structure
`Block a -> b`.

I refer to functions of that structure as 'folds'. (This is somewhat different to the
'real' terminology, but I feel like they capture the nature of 'folding once' very well.)

If you know `List.foldr` you already know an example for a fold!
The folds in this module are no different, we just write them in different ways.

We can do the same thing we did for this library for lists:

    type ListScaffold elem a
        = Empty
        | Cons elem a

    foldEmpty = 0

    foldCons a b = a + b

    handler listElement =
        case listElement of
            Empty ->
                foldEmpty

            Cons elem accumulated ->
                foldCons elem accumulated

    foldl : (ListScaffold a b -> b) -> List a -> b
    foldl handle list =
        case list of
            [] -> handle Empty
            (x:xs) -> handle (Cons x xs)

    foldl handler == List.foldl foldCons foldEmpty

The last line illustrates how differnt ways of writing these folds relate: For
`List.foldl` we simply provide the cases (empty or cons) as different arguments,
for folds in this library, we create a custom type case for empty and cons.


### Combining Folds

You can combine multiple 'folds' into one. There's no function for doing this, but a
pattern you might want to follow.

Let's say you want to accumulate both all the words in your markdown and the `Html` you
want it to render to, then you can do this:

    type alias Rendered =
        { html : Html Msg
        , words : List String
        }

    foldRendered : Block Rendered -> Rendered
    foldRendered block =
        { html = block |> map .html |> foldHtml
        , words = block |> map .words |> foldWords
        }

If you want to render to more things, just add another parameter to the record type and
follow the pattern. It is even possible to let the rendered html to depend on the words
inside itself (or maybe something else you're additionally folding to).


# Conversions

Did you already start to write a custom elm-markdown `Renderer`, but want to use this
library? Don't worry. They're compatible. You can convert between them!

@docs fromRenderer, toRenderer


# Utilities

I mean to aggregate utilites for transforming Blocks in this section.

@docs bumpHeadings

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Markdown.Block as Block
import Markdown.Html
import Markdown.Renderer exposing (Renderer)
import Pages.StaticHttp as StaticHttp
import Regex
import Result.Extra as Result



-- EXPOSED DEFINITIONS


{-| A datatype that enumerates all possible ways markdown could wrap some children.

Kind of like a 'Scaffold' around something that's already built, which will get torn down
after building is finished.

This does not include Html tags.

If you look at the left hand sides of all of the functions in the elm-markdown
[`Renderer`](https://package.elm-lang.org/packages/dillonkearns/elm-markdown/latest/Markdown-Renderer),
you'll notice a similarity to this custom type, except it's missing a type for 'html'.

Defining this data structure has some advantages in composing multiple Renderers.

It has a type parameter `children`, which is supposed to be filled with `String`,
`Html msg` or similar. Take a look at some [folds](#fold-building-blocks) for examples of this.

There are some neat tricks you can do with this data structure, for example, `Block Never`
represents only non-nested blocks of markdown.

-}
type Block children
    = Heading { level : Block.HeadingLevel, rawText : String, children : List children }
    | Paragraph (List children)
    | BlockQuote (List children)
    | Text String
    | CodeSpan String
    | Strong (List children)
    | Emphasis (List children)
    | Link { title : Maybe String, destination : String, children : List children }
    | Image { alt : String, src : String, title : Maybe String }
    | UnorderedList { items : List (Block.ListItem children) }
    | OrderedList { startingIndex : Int, items : List (List children) }
    | CodeBlock { body : String, language : Maybe String }
    | HardLineBreak
    | ThematicBreak
    | Table (List children)
    | TableHeader (List children)
    | TableBody (List children)
    | TableRow (List children)
    | TableCell (List children)
    | TableHeaderCell (Maybe Block.Alignment) (List children)


{-| Transform each child of a `Block` using the given function.

For example, we can transform the lists of words inside each block into concatenated
Strings:

    wordsToWordlist : Block (List String) -> Block String
    wordsToWordlist block =
        map (\listOfWords -> String.join ", " listOfWords)
            block

The ability to define this function is one of the reasons for our `Block` definition. If
you try defining `map` for elm-markdown's `Renderer` you'll find out it doesn't work.

-}
map : (a -> b) -> Block a -> Block b
map f markdown =
    case markdown of
        Heading { level, rawText, children } ->
            Heading { level = level, rawText = rawText, children = List.map f children }

        Paragraph children ->
            Paragraph (List.map f children)

        BlockQuote children ->
            BlockQuote (List.map f children)

        Text content ->
            Text content

        CodeSpan content ->
            CodeSpan content

        Strong children ->
            Strong (List.map f children)

        Emphasis children ->
            Emphasis (List.map f children)

        Link { title, destination, children } ->
            Link { title = title, destination = destination, children = List.map f children }

        Image imageInfo ->
            Image imageInfo

        UnorderedList { items } ->
            UnorderedList
                { items =
                    List.map
                        (\(Block.ListItem task children) ->
                            Block.ListItem task (List.map f children)
                        )
                        items
                }

        OrderedList { startingIndex, items } ->
            OrderedList { startingIndex = startingIndex, items = List.map (List.map f) items }

        CodeBlock codeBlockInfo ->
            CodeBlock codeBlockInfo

        HardLineBreak ->
            HardLineBreak

        ThematicBreak ->
            ThematicBreak

        Table children ->
            Table (List.map f children)

        TableHeader children ->
            TableHeader (List.map f children)

        TableBody children ->
            TableBody (List.map f children)

        TableRow children ->
            TableRow (List.map f children)

        TableCell children ->
            TableCell (List.map f children)

        TableHeaderCell alignment children ->
            TableHeaderCell alignment (List.map f children)


{-| Use this function if you want to parameterize your view by an environment.

Another way of thinking about this use-case is: use this if you want to 'render to
functions'.

Examples for what the `environment` type variable can be:

  - A `Model`, for rendering to `Model -> Html Msg` for `view`.
  - Templating information, in case you want to use markdown as templates and want to
    render to a function that expects templating parameters.

Usually, for the above usecases you would have to define a function of type

    foldTemplate :
        Block (TemplateInfo -> Html msg)
        -> (TemplateInfo -> Html msg)

for example, so that you can turn it back into a `Renderer (Template Info -> Html msg)`
for elm-markdown.

If you were to define such a function, you would have to pass around the `TemplateInfo`
parameter a lot. This function will take care of that for you.


### Anti use-cases

In some cases using this function would be overkill. The alternative to this function is
to simply parameterize your whole renderer (and not use this library):

    renderMarkdown : List String -> Block (Html Msg) -> Html Msg
    renderMarkdown censoredWords markdown =
        ...

    renderer : List String -> Markdown.Renderer (Html Msg)
    renderer censoredWords =
        toRenderer
            { renderHtml = ...
            , renderMarkdown = renderMarkdown censoredWords
            }

In this example you can see how we pass through the 'censored words'. It behaves kind of
like some global context in which we create our renderer.

It is hard to convey the abstract notion of when to use `parameterized` and when not to.
I'll give it a try: If you want to parse your markdown once and need to quickly render
different versions of it (for example with different `Model`s or different
`TemplateInfo`s), then use this. In other cases, if you probably only want to de-couple
some variable out of your renderer that is pretty static in general (for example censored
words), don't use this.


### `parameterized` over multiple Parameters

If you want to parameterize your renderer over multiple variables, there are two options:

1.  Add a field to the `environment` type used in this function
2.  Take another parameter in curried form

Although both are possible, I highly recommend the first option, as it is by far easier
to deal with only one call to `parameterized`, not with two calls that would be required
for option 2.


### Missing Functionality

If this function doesn't quite do what you want, just try to re-create what you need by
using `map` directly. `parameterized` basically just documents a pattern that is really
easy to re-create: Its implementation is just 1 line of code.

-}
parameterized :
    (Block view -> environment -> view)
    -> (Block (environment -> view) -> (environment -> view))
parameterized fold markdown env =
    fold (map (\expectingEnv -> expectingEnv env) markdown) env


{-| This transform enables validating the content of your `Block` before
rendering.

This function's most prominent usecases are linting markdown files, so for example:

  - Make sure all your code snippets are specified only with valid languages
    ('elm', 'javascript', 'js', 'html' etc.)
  - Make sure all your links are `https://` links
  - Generate errors/warnings on typos or words not contained in a dictionary
  - Disallow `h1` (alternatively, consider bumping the heading level)

But it might also be possible that your `view` type can't _always_ be folded from a
`Block view` to a `view`, so you need to generate an error in these cases.


### Missing Functionality

If this function doesn't quite do what you need to do, try using `foldResults`.
The `validating` definition basically just documents a common pattern. Its implementation
is just 1 line of code.

-}
validating :
    (Block view -> Result error view)
    -> (Block (Result error view) -> Result error view)
validating fold markdown =
    markdown |> foldResults |> Result.andThen fold


{-| This transform allows you to perform elm-pages' StaticHttp requests without having to
think about how to thread these through your renderer.

Some applications that can be realized like this:

  - Verifying that all links in your markdown do resolve at page build-time
    (Note: This currently needs some change in elm-pages, so it's not possible _yet_)
  - Giving custom elm-markdown HTML elements the ability to perform StaticHttp requests


### Missing Functionality

If this function doesn't quite do what you need to do, try using `foldStaticHttpRequests`.
The `wihtStaticHttpRequests` definition basically just documents a common pattern.
Its implementation is just 1 line of code.

-}
withStaticHttpRequests :
    (Block view -> StaticHttp.Request view)
    -> (Block (StaticHttp.Request view) -> StaticHttp.Request view)
withStaticHttpRequests fold markdown =
    markdown |> foldStaticHttpRequests |> StaticHttp.andThen fold


{-| This will fold a `Block` to `Html` similar to what the
[`defaultHtmlRenderer` in elm-markdown](https://package.elm-lang.org/packages/dillonkearns/elm-markdown/latest/Markdown-Renderer#defaultHtmlRenderer)
does. That is, it renders similar to what the CommonMark spec expects.

It also takes a list of attributes for convenience, so if you want to attach styles,
id's, classes or events, you can use this.

However, **the attributes parameter is ignored for `Text` nodes**.

-}
foldHtml : List (Html.Attribute msg) -> Block (Html msg) -> Html msg
foldHtml attributes markdown =
    case markdown of
        Heading { level, children } ->
            case level of
                Block.H1 ->
                    Html.h1 attributes children

                Block.H2 ->
                    Html.h2 attributes children

                Block.H3 ->
                    Html.h3 attributes children

                Block.H4 ->
                    Html.h4 attributes children

                Block.H5 ->
                    Html.h5 attributes children

                Block.H6 ->
                    Html.h6 attributes children

        Paragraph children ->
            Html.p attributes children

        BlockQuote children ->
            Html.blockquote attributes children

        Text content ->
            Html.text content

        CodeSpan content ->
            Html.code attributes [ Html.text content ]

        Strong children ->
            Html.strong attributes children

        Emphasis children ->
            Html.em attributes children

        Link link ->
            case link.title of
                Just title ->
                    Html.a
                        (Attr.href link.destination
                            :: Attr.title title
                            :: attributes
                        )
                        link.children

                Nothing ->
                    Html.a (Attr.href link.destination :: attributes) link.children

        Image imageInfo ->
            case imageInfo.title of
                Just title ->
                    Html.img
                        (Attr.src imageInfo.src
                            :: Attr.alt imageInfo.alt
                            :: Attr.title title
                            :: attributes
                        )
                        []

                Nothing ->
                    Html.img
                        (Attr.src imageInfo.src
                            :: Attr.alt imageInfo.alt
                            :: attributes
                        )
                        []

        UnorderedList { items } ->
            Html.ul attributes
                (items
                    |> List.map
                        (\item ->
                            case item of
                                Block.ListItem task children ->
                                    let
                                        checkbox =
                                            case task of
                                                Block.NoTask ->
                                                    Html.text ""

                                                Block.IncompleteTask ->
                                                    Html.input
                                                        [ Attr.disabled True
                                                        , Attr.checked False
                                                        , Attr.type_ "checkbox"
                                                        ]
                                                        []

                                                Block.CompletedTask ->
                                                    Html.input
                                                        [ Attr.disabled True
                                                        , Attr.checked True
                                                        , Attr.type_ "checkbox"
                                                        ]
                                                        []
                                    in
                                    Html.li [] (checkbox :: children)
                        )
                )

        OrderedList { startingIndex, items } ->
            Html.ol
                (case startingIndex of
                    1 ->
                        Attr.start startingIndex :: attributes

                    _ ->
                        attributes
                )
                (items
                    |> List.map
                        (\itemBlocks ->
                            Html.li []
                                itemBlocks
                        )
                )

        CodeBlock { body } ->
            Html.pre attributes
                [ Html.code []
                    [ Html.text body
                    ]
                ]

        HardLineBreak ->
            Html.br attributes []

        ThematicBreak ->
            Html.hr attributes []

        Table children ->
            Html.table attributes children

        TableHeader children ->
            Html.thead attributes children

        TableBody children ->
            Html.tbody attributes children

        TableRow children ->
            Html.tr attributes children

        TableHeaderCell maybeAlignment children ->
            let
                attrs =
                    case maybeAlignment of
                        Just Block.AlignLeft ->
                            Attr.align "left" :: attributes

                        Just Block.AlignCenter ->
                            Attr.align "center" :: attributes

                        Just Block.AlignRight ->
                            Attr.align "right" :: attributes

                        Nothing ->
                            attributes
            in
            Html.th attrs children

        TableCell children ->
            Html.td attributes children


{-| Transform a block that contains functions into a function that produces blocks.

One really common use-case is having access to a `Model` inside your html renderers.
In these cases you want your markdown to be 'rendered to a function'.

So let's say you've got a
[`Markdown.Html.Renderer`]()
like so:

    renderHtml :
        Markdown.Html.Renderer
            (List (Model -> Html Msg)
             -> (Model -> Html Msg)
            )

It has this type to be able to depend on the `Model`. Eventually you'll want to render to
`Model -> Html Msg`.

So now you can define your
[`Markdown.Renderer.Renderer`]()
like so:


    renderer : Markdown.Renderer.Renderer (Model -> Html Msg)
    renderer =
        toRenderer
            { renderHtml = renderHtml
            , renderMarkdown = renderMarkdown
            }

    renderMarkdown :
        Block (Model -> Html Msg)
        -> (Model -> Html Msg)
    renderMarkdown block model =
        foldFunction block
            -- ^ result : Model -> Block (Html Msg)
            model
            -- ^ result : Block (Html Msg)
            |> foldHtml

    -- ^ result : Html Msg

-}
foldFunction : Block (environment -> view) -> (environment -> Block view)
foldFunction markdown environment =
    markdown |> map ((|>) environment)


{-| Extracts all words from the blocks and inlines. Excludes any markup characters, if
they had an effect on the markup.

The words are split according to the `\s` javascript regular expression (regex).

Inline code spans are split, but **code blocks fragments are ignored** (code spans are
included).

If you need something more specific, I highly recommend rolling your own function for
this.

This is useful if you need to e.g. create header slugs.

-}
foldWords : Block (List String) -> List String
foldWords =
    let
        whitespace =
            Regex.fromStringWith { caseInsensitive = True, multiline = True } "\\s"
                |> Maybe.withDefault Regex.never

        words =
            Regex.split whitespace

        extractWords block =
            case block of
                Text content ->
                    words content

                CodeSpan content ->
                    words content

                _ ->
                    []
    in
    reduce
        { extract = extractWords
        , accumulate = List.concat
        }


{-| Thread results through your Blocks.

The input is a block that contains possibly failed views. The output becomes `Err`, if
any of the input block's children had an error (then it's the first error).
If all of the block's children were `Ok`, then the result is going to be `Ok`.

-}
foldResults : Block (Result error view) -> Result error (Block view)
foldResults markdown =
    case markdown of
        Heading { level, rawText, children } ->
            children
                |> Result.combine
                |> Result.map
                    (\chdr ->
                        Heading { level = level, rawText = rawText, children = chdr }
                    )

        Paragraph children ->
            children
                |> Result.combine
                |> Result.map Paragraph

        BlockQuote children ->
            children
                |> Result.combine
                |> Result.map BlockQuote

        Text content ->
            Text content
                |> Ok

        CodeSpan content ->
            CodeSpan content
                |> Ok

        Strong children ->
            children
                |> Result.combine
                |> Result.map Strong

        Emphasis children ->
            children
                |> Result.combine
                |> Result.map Emphasis

        Link { title, destination, children } ->
            children
                |> Result.combine
                |> Result.map
                    (\chdr ->
                        Link { title = title, destination = destination, children = chdr }
                    )

        Image imageInfo ->
            Image imageInfo
                |> Ok

        UnorderedList { items } ->
            items
                |> List.map
                    (\(Block.ListItem task children) ->
                        children
                            |> Result.combine
                            |> Result.map (Block.ListItem task)
                    )
                |> Result.combine
                |> Result.map (\itms -> UnorderedList { items = itms })

        OrderedList { startingIndex, items } ->
            items
                |> List.map Result.combine
                |> Result.combine
                |> Result.map
                    (\itms ->
                        OrderedList { startingIndex = startingIndex, items = itms }
                    )

        CodeBlock codeBlockInfo ->
            CodeBlock codeBlockInfo
                |> Ok

        HardLineBreak ->
            HardLineBreak
                |> Ok

        ThematicBreak ->
            ThematicBreak
                |> Ok

        Table children ->
            children
                |> Result.combine
                |> Result.map Table

        TableHeader children ->
            children
                |> Result.combine
                |> Result.map TableHeader

        TableBody children ->
            children
                |> Result.combine
                |> Result.map TableBody

        TableRow children ->
            children
                |> Result.combine
                |> Result.map TableRow

        TableHeaderCell maybeAlignment children ->
            children
                |> Result.combine
                |> Result.map (TableHeaderCell maybeAlignment)

        TableCell children ->
            children
                |> Result.combine
                |> Result.map TableCell


{-| Accumulate elm-page's
[`StaticHttp.Request`](https://package.elm-lang.org/packages/dillonkearns/elm-pages/latest/Pages-StaticHttp#Request)s
over blocks.

Using this, it is possible to write folds that produce views as a result of performing
static http requests.

-}
foldStaticHttpRequests : Block (StaticHttp.Request view) -> StaticHttp.Request (Block view)
foldStaticHttpRequests markdown =
    case markdown of
        Heading { level, rawText, children } ->
            children
                |> allStaticHttp
                |> StaticHttp.map
                    (\chdr ->
                        Heading { level = level, rawText = rawText, children = chdr }
                    )

        Paragraph children ->
            children
                |> allStaticHttp
                |> StaticHttp.map Paragraph

        BlockQuote children ->
            children
                |> allStaticHttp
                |> StaticHttp.map BlockQuote

        Text content ->
            Text content
                |> StaticHttp.succeed

        CodeSpan content ->
            CodeSpan content
                |> StaticHttp.succeed

        Strong children ->
            children
                |> allStaticHttp
                |> StaticHttp.map Strong

        Emphasis children ->
            children
                |> allStaticHttp
                |> StaticHttp.map Emphasis

        Link { title, destination, children } ->
            children
                |> allStaticHttp
                |> StaticHttp.map
                    (\chdr ->
                        Link { title = title, destination = destination, children = chdr }
                    )

        Image imageInfo ->
            Image imageInfo
                |> StaticHttp.succeed

        UnorderedList { items } ->
            items
                |> List.map
                    (\(Block.ListItem task children) ->
                        children
                            |> allStaticHttp
                            |> StaticHttp.map (Block.ListItem task)
                    )
                |> allStaticHttp
                |> StaticHttp.map (\itms -> UnorderedList { items = itms })

        OrderedList { startingIndex, items } ->
            items
                |> List.map allStaticHttp
                |> allStaticHttp
                |> StaticHttp.map
                    (\itms ->
                        OrderedList { startingIndex = startingIndex, items = itms }
                    )

        CodeBlock codeBlockInfo ->
            CodeBlock codeBlockInfo
                |> StaticHttp.succeed

        HardLineBreak ->
            HardLineBreak
                |> StaticHttp.succeed

        ThematicBreak ->
            ThematicBreak
                |> StaticHttp.succeed

        Table children ->
            children
                |> allStaticHttp
                |> StaticHttp.map Table

        TableHeader children ->
            children
                |> allStaticHttp
                |> StaticHttp.map TableHeader

        TableBody children ->
            children
                |> allStaticHttp
                |> StaticHttp.map TableBody

        TableRow children ->
            children
                |> allStaticHttp
                |> StaticHttp.map TableRow

        TableHeaderCell maybeAlignment children ->
            children
                |> allStaticHttp
                |> StaticHttp.map (TableHeaderCell maybeAlignment)

        TableCell children ->
            children
                |> allStaticHttp
                |> StaticHttp.map TableCell


{-| Reduces a block down to anything that can be accumulated.

You provide two functions

  - `accumulate`: Describe how values of type `a` are combined. Examples: `List.concat`,
    `List.sum`, etc.
  - `extract`: Descibe how a blocks generate values that are supposed to be accumulated.

For example, this can count the amount of headings in a markdown document:

    reduce
        { accumulate = List.sum
        , extract =
            \block ->
                case block of
                    Heading _ ->
                        1

                    _ ->
                        0
        }

Or this extracts code blocks:

    reduce
        { accumulate = List.concat
        , extract =
            \block ->
                case block of
                    CodeBlock codeBlock ->
                        [ codeBlock ]

                    _ ->
                        []
        }

The special thing about this function is how you don't have to worry about accumulating
the other generated values recursively.

-}
reduce : { accumulate : List a -> a, extract : Block a -> a } -> Block a -> a
reduce { extract, accumulate } block =
    let
        append a b =
            accumulate [ a, b ]
    in
    case block of
        Heading { children } ->
            accumulate children
                |> append (extract block)

        Paragraph children ->
            accumulate children
                |> append (extract block)

        BlockQuote children ->
            accumulate children
                |> append (extract block)

        Text _ ->
            extract block

        CodeSpan _ ->
            extract block

        Strong children ->
            accumulate children
                |> append (extract block)

        Emphasis children ->
            accumulate children
                |> append (extract block)

        Link link ->
            accumulate link.children
                |> append (extract block)

        Image _ ->
            extract block

        UnorderedList { items } ->
            items
                |> List.concatMap
                    (\(Block.ListItem _ child) -> child)
                |> accumulate
                |> append (extract block)

        OrderedList { items } ->
            items
                |> List.concat
                |> accumulate
                |> append (extract block)

        CodeBlock _ ->
            extract block

        HardLineBreak ->
            extract block

        ThematicBreak ->
            extract block

        Table children ->
            accumulate children
                |> append (extract block)

        TableHeader children ->
            accumulate children
                |> append (extract block)

        TableBody children ->
            accumulate children
                |> append (extract block)

        TableRow children ->
            accumulate children
                |> append (extract block)

        TableHeaderCell _ children ->
            accumulate children
                |> append (extract block)

        TableCell children ->
            accumulate children
                |> append (extract block)


{-| There are two ways of thinking about this function:

1.  Render a `Block` using the given elm-markdown `Renderer`.
2.  Extract a function of type `(Block view -> view)` out of
    the elm-markdown `Renderer`. This is useful if you want to make use
    of the utilities present in this library.

-}
fromRenderer : Renderer view -> Block view -> view
fromRenderer renderer markdown =
    case markdown of
        Heading info ->
            renderer.heading info

        Paragraph children ->
            renderer.paragraph children

        BlockQuote children ->
            renderer.blockQuote children

        Text content ->
            renderer.text content

        CodeSpan content ->
            renderer.codeSpan content

        Strong children ->
            renderer.strong children

        Emphasis children ->
            renderer.emphasis children

        Link { title, destination, children } ->
            renderer.link { title = title, destination = destination } children

        Image imageInfo ->
            renderer.image imageInfo

        UnorderedList { items } ->
            renderer.unorderedList items

        OrderedList { startingIndex, items } ->
            renderer.orderedList startingIndex items

        CodeBlock info ->
            renderer.codeBlock info

        HardLineBreak ->
            renderer.hardLineBreak

        ThematicBreak ->
            renderer.thematicBreak

        Table children ->
            renderer.table children

        TableHeader children ->
            renderer.tableHeader children

        TableBody children ->
            renderer.tableBody children

        TableRow children ->
            renderer.tableRow children

        TableHeaderCell maybeAlignment children ->
            renderer.tableHeaderCell maybeAlignment children

        TableCell children ->
            renderer.tableCell children


{-| Convert a function that works with `Block` to a `Renderer` for use with
elm-markdown.

(The second parameter is a [`Markdown.Html.Renderer`](/packages/dillonkearns/elm-markdown/3.0.0/Markdown-Html#Renderer))

-}
toRenderer :
    { renderMarkdown : Block view -> view
    , renderHtml : Markdown.Html.Renderer (List view -> view)
    }
    -> Renderer view
toRenderer { renderMarkdown, renderHtml } =
    { heading = Heading >> renderMarkdown
    , paragraph = Paragraph >> renderMarkdown
    , blockQuote = BlockQuote >> renderMarkdown
    , html = renderHtml
    , text = Text >> renderMarkdown
    , codeSpan = CodeSpan >> renderMarkdown
    , strong = Strong >> renderMarkdown
    , emphasis = Emphasis >> renderMarkdown
    , hardLineBreak = HardLineBreak |> renderMarkdown
    , link =
        \{ title, destination } children ->
            Link { title = title, destination = destination, children = children }
                |> renderMarkdown
    , image = Image >> renderMarkdown
    , unorderedList =
        \items ->
            UnorderedList { items = items }
                |> renderMarkdown
    , orderedList =
        \startingIndex items ->
            OrderedList { startingIndex = startingIndex, items = items }
                |> renderMarkdown
    , codeBlock = CodeBlock >> renderMarkdown
    , thematicBreak = ThematicBreak |> renderMarkdown
    , table = Table >> renderMarkdown
    , tableHeader = TableHeader >> renderMarkdown
    , tableBody = TableBody >> renderMarkdown
    , tableRow = TableRow >> renderMarkdown
    , tableHeaderCell = \maybeAlignment -> TableHeaderCell maybeAlignment >> renderMarkdown
    , tableCell = TableCell >> renderMarkdown
    }


{-| Bump all `Heading` elements by given positive amount of levels.

    import Markdown.Block as Block

    bumpHeadings 2
        (Heading
            { level = Block.H1
            , rawText = ""
            , children = []
            }
        )
    --> Heading
    -->     { level = Block.H3
    -->     , rawText = ""
    -->     , children = []
    -->     }

    bumpHeadings 1
        (Heading
            { level = Block.H6
            , rawText = ""
            , children = []
            }
        )
    --> Heading
    -->     { level = Block.H6
    -->     , rawText = ""
    -->     , children = []
    -->     }

    bumpHeadings -1
        (Heading
            { level = Block.H2
            , rawText = ""
            , children = []
            }
        )
    --> Heading
    -->     { level = Block.H2
    -->     , rawText = ""
    -->     , children = []
    -->     }

-}
bumpHeadings : Int -> Block view -> Block view
bumpHeadings by markdown =
    let
        -- vendored from elm-loop
        for : Int -> (a -> a) -> a -> a
        for =
            let
                for_ : Int -> Int -> (a -> a) -> a -> a
                for_ i n f v =
                    if i < n then
                        for_ (i + 1) n f (f v)

                    else
                        v
            in
            for_ 0
    in
    case markdown of
        Heading info ->
            Heading { info | level = for by bumpHeadingLevel info.level }

        other ->
            other



-- LOCAL DEFINITIONS


allStaticHttp : List (StaticHttp.Request a) -> StaticHttp.Request (List a)
allStaticHttp =
    List.foldl (StaticHttp.map2 (::)) (StaticHttp.succeed [])


bumpHeadingLevel : Block.HeadingLevel -> Block.HeadingLevel
bumpHeadingLevel level =
    case level of
        Block.H1 ->
            Block.H2

        Block.H2 ->
            Block.H3

        Block.H3 ->
            Block.H4

        Block.H4 ->
            Block.H5

        Block.H5 ->
            Block.H6

        Block.H6 ->
            Block.H6
