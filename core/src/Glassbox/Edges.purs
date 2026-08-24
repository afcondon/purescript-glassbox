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
  , assignments
  , merge
  , inboundTo
  , reachableStates
  , unreachableStates
  ) where

import Prelude

import Data.Array (concatMap, elem, filter, nub, nubEq, sort)
import Data.Foldable (foldl)
import Data.Set as Set
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Glassbox.Run (World, resolve, setFact)
import Glassbox.Spec
  ( EventId
  , FactId
  , Outcome(..)
  , Value(..)
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
  -- | Draw what the machine CAN do, rather than what it would do if every fact
  -- | the host has not reported were false.
  -- |
  -- | This distinction cost a real transition. `Bosun`'s supervisor guards
  -- | `raised -restart-> restarting` on a fact — is the named service in this
  -- | group — and under a bare world that fact reads false, so the guarded case
  -- | lost to the refusal beneath it and the edge simply was not in the
  -- | picture. A diagram that silently omits a transition is worse than no
  -- | diagram, because the whole justification for drawing one is that it is a
  -- | second opinion.
  -- |
  -- | Config is a different matter and is deliberately NOT enumerated: it is
  -- | *declared* by the artifact, with a default, and it genuinely reshapes the
  -- | machine. Set a looper's count-in to zero and `Armed` really does lose its
  -- | only inbound edge. Facts describe a world that changes under the machine's
  -- | feet; config describes which machine this is.
  , factsUnknown :: Boolean
  }

-- | `factsUnknown` is on, because the ordinary question about a machine is what
-- | it can do. A caller wanting the edges of one particular moment turns it off
-- | and supplies the facts of that moment.
defaultOptions :: Options
defaultOptions = { showRefusals: true, factsUnknown: true }

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

  -- Every world the unreported facts could put the machine in. Enumerated
  -- rather than reasoned about, so this cannot drift from `resolve`: whatever
  -- the runtime would do under some assignment is what gets drawn, by asking it.
  -- EVERY declared fact, not just the ones this world has yet to report. A fact
  -- the host happens to have set is still transient — it is true now and may be
  -- false in a moment — so letting it prune the picture would mean the same
  -- machine drew differently depending on when you asked. Config is what decides
  -- which machine this is; facts are not.
  worlds =
    if not options.factsUnknown then [ world ]
    else assignments world (map _.id spec.facts)

  probe from event =
    let
      outcomes = map (\w -> resolve spec w from event) worlds
    in
      case Array.head (Array.filter isMove outcomes) of
      -- A pair that can move under SOME assignment is drawn as moving, and the
      -- refusal it also admits is drawn beside it by the pass below.
        Just (Move to) -> Just { from, to, events: [ event ], kind: OnEvent, reason: Nothing }
        _ -> case Array.head (Array.filter isRefusal' outcomes) of
          Just (Refuse rid) -> Just { from, to: from, events: [ event ], kind: OnRefusal, reason: Just rid }
          _ -> Nothing

  isMove = case _ of
    Move _ -> true
    _ -> false

  isRefusal' = case _ of
    Refuse _ -> true
    _ -> false

  -- A state that resolves by itself. The target comes from asking the rules
  -- what the pending event does, never from trusting the deadline to say where
  -- it goes — which is why a `Deadline` names an event rather than a state.
  deadlineEdges = Array.mapMaybe deadlineEdge spec.states

  deadlineEdge decl = case deadlineFor spec decl.id of
    Nothing -> Nothing
    Just d -> case Array.head (Array.filter isMove (map (\w -> resolve spec w decl.id d.fires) worlds)) of
      Just (Move to) -> Just
        { from: decl.id, to, events: [ d.fires ], kind: OnDeadline, reason: Nothing }
      _ -> Nothing

-- | Every way the listed facts could be reported, as worlds.
-- |
-- | Exported because it is the conformance criterion the plan states — *every
-- | (state x event x guard-assignment) the artifact admits* — and a runner
-- | driving two implementations through all of them needs exactly this list.
-- |
-- | Exponential, and capped for that reason: past a dozen unreported facts the
-- | enumeration is the wrong tool and the honest answer is the world we were
-- | given. No machine in any corpus is near the cap, and a machine that reaches
-- | it has a modelling problem the drawing cannot fix.
assignments :: World -> Array FactId -> Array World
assignments world facts
  | Array.length facts > 12 = [ world ]
  | otherwise = foldl spread [ world ] facts
      where
      spread ws fid = do
        w <- ws
        v <- [ VBoolean false, VBoolean true ]
        pure (setFact fid v w)

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

-- | Every state actually reachable from the initial state, by any route.
-- |
-- | This is a transitive closure and it has to be. An earlier version was
-- | `initial <> every non-self edge target`, which is in-degree rather than
-- | reachability: a stranded island `c --go--> d` reported `d` as reachable
-- | purely because `c` pointed at it, so `unreachableStates` only ever caught
-- | the *head* of a stranded component and quietly passed the rest.
-- |
-- | Self-loops are excluded from the walk: refusing an event where you already
-- | are is not a way of getting anywhere.
reachableStates :: Spec -> Array Edge -> Array StateId
reachableStates spec edges = Set.toUnfoldable (walk (Set.singleton spec.initial) [ spec.initial ])
  where
  walk seen frontier = case Array.uncons frontier of
    Nothing -> seen
    Just { head, tail } ->
      let
        found = filter (\s -> not (Set.member s seen)) (successorsOf head)
        seen' = foldl (flip Set.insert) seen found
      in
        walk seen' (tail <> found)

  successorsOf from =
    nub (map _.to (filter (\e -> e.from == from && e.to /= e.from) edges))

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
