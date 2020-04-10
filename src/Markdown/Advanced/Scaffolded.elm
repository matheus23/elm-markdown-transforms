module Markdown.Advanced.Scaffolded exposing (..)

{-| Things to think about:

  - I need to make my own Html renderer now?
  - Do I need to bundle block and inline algebras together now?

For now I'll keep this module non-exposed. Things have to be figured out first!
I can build this API while I depend on this via a

    "source-directories": "../elm-markdown-transforms/src"

field.

-}

import Markdown.Block exposing (Alignment(..), HeadingLevel(..), Html(..), ListItem(..))


type Block block inline
    = -- Container Blocks
      HtmlBlock (Html block)
    | UnorderedList (List (ListItem inline))
    | OrderedList Int (List (List inline))
    | BlockQuote (List block)
      -- Leaf Blocks With Inlines
    | Heading HeadingLevel (List inline)
    | Paragraph (List inline)
    | Table (List { label : List inline, alignment : Maybe Alignment }) (List (List inline))
      -- Leaf Blocks Without Inlines
    | CodeBlock { body : String, language : Maybe String }
    | ThematicBreak


type Inline block inline
    = HtmlInline (Html block)
    | Link String (Maybe String) (List inline)
    | Image String (Maybe String) (List inline)
    | Emphasis (List inline)
    | Strong (List inline)
      -- Strikethrough TODO  https://github.github.com/gfm/#strikethrough-extension-
    | CodeSpan String
    | Text String
    | HardLineBreak


type DeepBlock
    = DeepBlock (Block DeepBlock DeepInline)


type DeepInline
    = DeepInline (Inline DeepBlock DeepInline)
