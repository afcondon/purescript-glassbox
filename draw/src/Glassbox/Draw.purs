-- | Glassbox.Draw
-- |
-- | The adapter between a machine's edge set and Hylograph's layout.
-- |
-- | This is a separate package from `glassbox` on purpose. The core — the data
-- | model, the runtime, the codec, the edge set — has no dependency on any
-- | drawing library, because two of its intended consumers have no business
-- | pulling one in: a headless conformance runner comparing two
-- | implementations of the format, and any host on a backend where a layout
-- | library is not available or not wanted. Running a machine and drawing one
-- | are different jobs, and the project's own claim is that the same machine
-- | runs under an arbitrary UI.
-- |
-- | What crosses the boundary is `Glassbox.Edges.Edge`, which carries
-- | identifiers rather than labels or geometry. Everything here is the
-- | translation of that into `DataViz.Layout.StateMachine`'s vocabulary.
module Glassbox.Draw
  ( EdgeExtra
  , describe
  , annotate
  , edgePairs
  , inducedFrom
  ) where

import Prelude

import Data.Graph.Algorithms (mkSimpleGraph)
import Data.Graph.InducedTree (EdgeClass, Induced, classOf, induce)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import DataViz.Layout.StateMachine (StateMachine)
import Glassbox.Edges (Edge, EdgeKind)
import Glassbox.Spec (Spec, StateId(..), labelOfEvent, textOfRefusal)

-- | What rides along on a drawn transition.
-- |
-- | This is what the `extra` parameter on `DataViz.Layout.StateMachine`'s
-- | `Transition` is for: without it a renderer would need a side table keyed by
-- | from/to/label to know that an edge is a refusal rather than a move, or a
-- | shortcut rather than a spine.
type EdgeExtra =
  { kind :: EdgeKind
  , reason :: Maybe String
  , role :: Maybe EdgeClass
  }

-- | Turn a machine and its edge set into something the layout can lay out.
-- |
-- | Labels are resolved here and not before, because which of an event's names
-- | to show is a question for whoever is showing it.
describe :: Spec -> Array Edge -> StateMachine Unit EdgeExtra
describe spec edges =
  { states: map toState spec.states
  , transitions: map toTransition edges
  }
  where
  raw (StateId s) = s

  toState decl =
    { id: raw decl.id
    , label: decl.label
    , isInitial: decl.id == spec.initial
    , isFinal: decl.final
    , extra: unit
    }

  toTransition edge =
    { from: raw edge.from
    , to: raw edge.to
    , label: joinWith ", " (map (labelOfEvent spec) edge.events)
    , extra:
        { kind: edge.kind
        , reason: map (textOfRefusal spec) edge.reason
        , role: Nothing
        }
    }

-- | Attach each edge's position relative to the tree a navigator is forced to
-- | build. A second, independent annotation on the same edges: `describe` says
-- | how an edge came to exist, `annotate` says what it means to someone
-- | navigating.
annotate :: Induced String -> StateMachine Unit EdgeExtra -> StateMachine Unit EdgeExtra
annotate induced machine = machine
  { transitions = map mark machine.transitions }
  where
  mark edge = edge { extra = edge.extra { role = classOf induced edge.from edge.to } }

-- | The bare directed edge set, for analysis that does not care about labels.
edgePairs :: forall se te. StateMachine se te -> Array (Tuple String String)
edgePairs machine = map (\e -> Tuple e.from e.to) machine.transitions

-- | The induced tree of a described machine, rooted at its initial state.
-- |
-- | A convenience: `induce` wants a `SimpleGraph`, and every caller was
-- | otherwise writing the same three lines to build one.
inducedFrom :: StateMachine Unit EdgeExtra -> String -> Induced String
inducedFrom machine root =
  induce root (mkSimpleGraph (map _.id machine.states) (edgePairs machine))
