-- | Glassbox.Edges
-- |
-- | The edge set of a machine, derived by asking it.
-- |
-- | Every edge here is produced by actually resolving a rule under a world, so
-- | there is no second description that could drift from the first. This is the
-- | property the whole library exists to protect, and it is why there is no
-- | `edges` field on `Spec` for an author to write by hand.
-- |
-- | Because the edge set is computed **under a world**, configuration genuinely
-- | reshapes it: set a looper's count-in to zero and its Armed state loses its
-- | only inbound edge. That is the mechanical test the format uses to sort
-- | configuration from parameter — *does the diagram change when you change
-- | it?* — and it only works because the edge set is derived rather than
-- | declared.
-- |
-- | This module is deliberately free of any drawing concern. It yields
-- | identifiers, not labels, and knows nothing about geometry: a headless
-- | conformance runner comparing two implementations needs exactly this and
-- | must not have to pull in a layout library to get it. Turning edges into a
-- | picture is `glassbox-draw`'s job; turning them into text is
-- | `Glassbox.Export.StateDiagram`'s.
module Glassbox.Edges
  ( EdgeKind(..)
  , Edge
  , Options
  , defaultOptions
  , edgesOf
  , merge
  , inboundTo
  , reachableStates
  , unreachableStates
  ) where

import Prelude

import Data.Array (concatMap, elem, filter, nub, nubEq, sort)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Glassbox.Run (World, resolve)
import Glassbox.Spec
  ( EventId
  , Outcome(..)
  , RefusalId
  , Spec
  , StateId
  , deadlineFor
  )

-- | How an edge came to exist.
-- |
-- | A refusal is an edge because it is a thing that happens when a person
-- | presses something, and a display that cannot show it is the failure the
-- | `Stay`/`Refuse` distinction exists to prevent. It is drawn as a self-loop
-- | but it is not a transition.
data EdgeKind
  = OnEvent      -- ^ an event moved the machine
  | OnDeadline   -- ^ it will fire with no event from anyone
  | OnRefusal    -- ^ the event was refused, with a reason

derive instance eqEdgeKind :: Eq EdgeKind
derive instance ordEdgeKind :: Ord EdgeKind

-- | One edge.
-- |
-- | `events` is an array because `merge` collapses parallel edges that share a
-- | source, target and kind — three events that all lead one way are one arrow.
-- | It carries identifiers rather than rendered labels, because which of an
-- | event's names to show is a decision for whoever is doing the showing.
type Edge =
  { from :: StateId
  , to :: StateId
  , events :: Array EventId
  , kind :: EdgeKind
  , reason :: Maybe RefusalId
  }

type Options =
  { showRefusals :: Boolean
  }

defaultOptions :: Options
defaultOptions = { showRefusals: true }

-- | Resolve every (state, event) pair under one world.
edgesOf :: Options -> World -> Spec -> Array Edge
edgesOf options world spec =
  merge (filter keep (eventEdges <> deadlineEdges))
  where
  keep edge = options.showRefusals || edge.kind /= OnRefusal

  eventEdges = concatMap probeState spec.states

  -- An event that only ever arrives because a deadline delivered it is drawn as
  -- a deadline edge below. Probing it here as well would produce the same
  -- transition twice, once truthfully and once misleadingly — as though a
  -- person could cause it.
  probeState decl =
    Array.mapMaybe (probe decl.id)
      (filter (not <<< deliveredByDeadline decl.id) (map _.id spec.events))

  deliveredByDeadline sid event = case deadlineFor spec sid of
    Just d -> d.fires == event
    Nothing -> false

  probe from event = case resolve spec world from event of
    Move to -> Just { from, to, events: [ event ], kind: OnEvent, reason: Nothing }
    Refuse rid -> Just { from, to: from, events: [ event ], kind: OnRefusal, reason: Just rid }
    Stay -> Nothing

  -- A state that resolves by itself. The target comes from asking the rules
  -- what the pending event does, never from trusting the deadline to say where
  -- it goes — which is why a `Deadline` names an event rather than a state.
  deadlineEdges = Array.mapMaybe deadlineEdge spec.states

  deadlineEdge decl = case deadlineFor spec decl.id of
    Nothing -> Nothing
    Just d -> case resolve spec world decl.id d.fires of
      Move to -> Just
        { from: decl.id, to, events: [ d.fires ], kind: OnDeadline, reason: Nothing }
      _ -> Nothing

-- | Collapse parallel edges of the same kind between the same pair.
merge :: Array Edge -> Array Edge
merge edges = Array.mapMaybe rebuild (nub (map key edges))
  where
  key edge = Tuple (Tuple edge.from edge.to) edge.kind

  rebuild k@(Tuple (Tuple from to) kind) = case Array.head group of
    Nothing -> Nothing
    Just _ -> Just
      { from
      , to
      , kind
      , events: nubEq (sort (concatMap _.events group))
      , reason: Array.head (Array.mapMaybe _.reason group)
      }
    where
    group = filter (\e -> key e == k) edges

-- | Edges arriving at a state from somewhere else. A self-loop is not an
-- | arrival, which is what makes this the reachability question rather than the
-- | degree question.
inboundTo :: StateId -> Array Edge -> Array Edge
inboundTo target = filter (\e -> e.to == target && e.from /= target)

-- | Every state something can actually get to, plus the initial state.
reachableStates :: Spec -> Array Edge -> Array StateId
reachableStates spec edges =
  nub ([ spec.initial ] <> map _.to (filter (\e -> e.from /= e.to) edges))

-- | States the machine declares and cannot arrive at under this world.
-- |
-- | A warning rather than an error: a count-in of zero legitimately strands the
-- | Armed state, and an artifact is entitled to describe a machine whose shape
-- | depends on how it is configured.
unreachableStates :: Spec -> Array Edge -> Array StateId
unreachableStates spec edges =
  filter (\s -> not (elem s reached)) (map _.id spec.states)
  where
  reached = reachableStates spec edges
