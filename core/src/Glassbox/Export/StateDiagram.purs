-- | Glassbox.Export.StateDiagram
-- |
-- | Emit a machine as state-diagram text.
-- |
-- | **Nothing is imported to do this.** The dialect is Mermaid's
-- | `stateDiagram-v2`, because GitHub and a hundred other things render it for
-- | free, but Mermaid is a *paste target* and not a dependency: what follows is
-- | string concatenation over our own value, and no external package is pulled
-- | in to produce it. The picture on screen is ours too — derived from the same
-- | edge set, laid out by `glassbox-draw`, and put on the page by HATS. This
-- | module is a convenience for getting a machine into somebody else's
-- | document, not a way of getting out of drawing it.
-- |
-- | It lives under `Export` rather than `Codec` because it only goes one way. A
-- | codec round-trips; this cannot be read back, and filing it beside
-- | `Glassbox.Codec.JSON` implied a symmetry it does not have.
-- |
-- | Its cheap value is a second opinion: if the picture and this text disagree,
-- | one of the two readings is wrong, and finding that out costs a glance.
module Glassbox.Export.StateDiagram
  ( toStateDiagram
  ) where

import Prelude

import Data.Array (filter, find)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Glassbox.Edges (Edge, EdgeKind(..))
import Glassbox.Spec (Spec, StateId(..), labelOfEvent)

toStateDiagram :: Spec -> Array Edge -> String
toStateDiagram spec edges =
  joinWith "\n" $
    [ "stateDiagram-v2" ]
      <> map stateLine spec.states
      <> initialLine
      <> map edgeLine (filter drawn edges)
  where
  -- Refusals are self-loops that would treble the edge count without telling a
  -- reader anything the drawing's refusal styling does not already say.
  drawn edge = edge.kind /= OnRefusal

  raw (StateId s) = s

  stateLine decl = "    " <> raw decl.id <> " : " <> clean decl.label

  initialLine = case find (\d -> d.id == spec.initial) spec.states of
    Just initial -> [ "    [*] --> " <> raw initial.id ]
    Nothing -> []

  edgeLine edge =
    "    " <> raw edge.from <> " --> " <> raw edge.to
      <> " : " <> marker edge.kind
      <> joinWith ", " (map (clean <<< labelOfEvent spec) edge.events)

  marker = case _ of
    OnDeadline -> "after "
    _ -> ""

  -- A colon separates a transition from its label in this notation, so it
  -- cannot appear inside one.
  clean = replaceAll (Pattern ":") (Replacement " ")
