-- | Plate.Html
-- |
-- | A markup value, and one function that turns it into bytes.
-- |
-- | Small on purpose. A page this size assembled by string concatenation gets
-- | an unclosed tag or an unescaped `<` in it eventually, and both fail
-- | silently — the browser guesses, and the guess is usually *nearly* right,
-- | which is the worst kind of wrong for a document whose whole claim is that
-- | you can check it by looking. Escaping happens in exactly one place here
-- | (`Text`), and closing happens in exactly one place (`render`), so neither
-- | can be forgotten at a call site.
-- |
-- | `Raw` is the escape hatch and it is used for three things only: the
-- | stylesheet, the script, and inline `<code>` runs lifted out of an author's
-- | prose. Nothing derived from a machine's identifiers goes through it.
module Plate.Html
  ( Node(..)
  , Attr
  , render
  , el
  , el_
  , text
  , raw
  , empty
  , attr
  , cls
  , when_
  , foldNodes
  ) where

import Prelude

import Data.Array (elem, foldMap, null)
import Data.String (joinWith)
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Tuple (Tuple(..))

type Attr = Tuple String String

data Node
  = El String (Array Attr) (Array Node)
  | Text String
  | Raw String
  | Empty

el :: String -> Array Attr -> Array Node -> Node
el = El

-- | An element with no attributes, which is most of them.
el_ :: String -> Array Node -> Node
el_ name = El name []

text :: String -> Node
text = Text

raw :: String -> Node
raw = Raw

empty :: Node
empty = Empty

attr :: String -> String -> Attr
attr = Tuple

cls :: String -> Attr
cls = Tuple "class"

-- | Emit a node only when a condition holds — the alternative being `Empty`,
-- | which renders to nothing at all rather than to an empty element.
when_ :: Boolean -> Node -> Node
when_ b node = if b then node else Empty

foldNodes :: Array Node -> Node
foldNodes children = El "" [] children

render :: Node -> String
render = case _ of
  Empty -> ""
  Text s -> escape s
  Raw s -> s
  -- The anonymous element is a fragment: children, no tags.
  El "" _ children -> foldMap render children
  El name attrs children
    | name `elem` voids -> "<" <> name <> renderAttrs attrs <> ">"
    | otherwise ->
        "<" <> name <> renderAttrs attrs <> ">"
          <> foldMap render children
          <> "</" <> name <> ">"

voids :: Array String
voids = [ "br", "hr", "img", "input", "link", "meta" ]

renderAttrs :: Array Attr -> String
renderAttrs attrs
  | null attrs = ""
  | otherwise = " " <> joinWith " " (map one attrs)
      where
      one (Tuple k v) = k <> "=\"" <> escapeAttr v <> "\""

escape :: String -> String
escape =
  replaceAll (Pattern "&") (Replacement "&amp;")
    >>> replaceAll (Pattern "<") (Replacement "&lt;")
    >>> replaceAll (Pattern ">") (Replacement "&gt;")

escapeAttr :: String -> String
escapeAttr =
  escape >>> replaceAll (Pattern "\"") (Replacement "&quot;")
