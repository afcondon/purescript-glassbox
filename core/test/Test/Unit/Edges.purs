-- | Test.Unit.Edges
-- |
-- | `Glassbox.Edges`, on hand-built edge arrays rather than through a machine.
-- |
-- | `merge` and the reachability helpers are the only place in the library where
-- | a *set* is computed rather than a value looked up, and set operations are
-- | where identity bugs live: merging across a boundary that should have
-- | separated two arrows, or losing the refusal reason that was the only thing
-- | distinguishing them. Driving those from a real machine makes the interesting
-- | cases hard to reach on purpose.
module Test.Unit.Edges
  ( edgeUnits
  ) where

import Prelude

import Data.Array (elem, filter, length, nub, sort)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Glassbox.Edges
  ( Edge
  , EdgeKind(..)
  , defaultOptions
  , edgesOf
  , inboundTo
  , merge
  , reachableStates
  , unreachableStates
  )
import Glassbox.Run (worldFrom)
import Glassbox.Spec (Expr(..), RefusalId(..), Spec, StateId(..), Value(..))
import Test.Fixture (always, blank, edge, eid, ev, refusal, refuse, sid, st, stay, to)
import Test.Harness (check, section)

-- =============================================================================
-- merge
-- =============================================================================

mixed :: Array Edge
mixed =
  [ edge "a" "b" [ "x" ] OnEvent Nothing
  , edge "a" "b" [ "y" ] OnEvent Nothing
  , edge "a" "a" [ "z" ] OnRefusal (Just "busy")
  , edge "b" "c" [ "t" ] OnDeadline Nothing
  , edge "a" "b" [ "x" ] OnDeadline Nothing
  ]

-- | The (from, to, kind) identity `merge` groups by, rendered so it can be
-- | compared as a set without needing an `Ord` instance on `Edge`.
triples :: Array Edge -> Array String
triples = sort <<< nub <<< map key
  where
  key e = raw e.from <> " -> " <> raw e.to <> " / " <> kindName e.kind
  raw (StateId s) = s
  kindName = case _ of
    OnEvent -> "event"
    OnDeadline -> "deadline"
    OnRefusal -> "refusal"

mergeUnits :: Effect (Array Boolean)
mergeUnits = do
  section "Edges.merge — collapse parallel arrows, and nothing else"

  let onEvent from target events = edge from target events OnEvent Nothing

  a <- check "three events one way become one arrow carrying three events"
    ( merge [ onEvent "a" "b" [ "x" ], onEvent "a" "b" [ "y" ], onEvent "a" "b" [ "z" ] ]
        == [ onEvent "a" "b" [ "x", "y", "z" ] ]
    )
  b <- check "events end up sorted, whatever order they arrived in"
    ( merge [ onEvent "a" "b" [ "z" ], onEvent "a" "b" [ "x" ], onEvent "a" "b" [ "m" ] ]
        == [ onEvent "a" "b" [ "m", "x", "z" ] ]
    )
  c <- check "and deduplicated"
    ( merge [ onEvent "a" "b" [ "x", "y" ], onEvent "a" "b" [ "y", "x" ] ]
        == [ onEvent "a" "b" [ "x", "y" ] ]
    )

  -- The three boundaries. Merging across any of them would draw an arrow the
  -- machine does not have.
  d <- check "a different target is a different arrow"
    (length (merge [ onEvent "a" "b" [ "x" ], onEvent "a" "c" [ "x" ] ]) == 2)
  e <- check "a different source is a different arrow"
    (length (merge [ onEvent "a" "c" [ "x" ], onEvent "b" "c" [ "x" ] ]) == 2)
  f <- check "a different kind is a different arrow, even between the same pair"
    ( length
        ( merge
            [ onEvent "a" "b" [ "x" ]
            , edge "a" "b" [ "x" ] OnDeadline Nothing
            , edge "a" "b" [ "x" ] OnRefusal (Just "r")
            ]
        ) == 3
    )

  g <- check "an empty array merges to an empty array" (merge [] == [])
  h <- check "a single edge survives unchanged, events still sorted"
    (merge [ onEvent "a" "b" [ "y", "x" ] ] == [ onEvent "a" "b" [ "x", "y" ] ])

  -- A refusal is the one edge kind carrying a payload beyond its endpoints.
  i <- check "a refusal reason survives the merge"
    ( merge [ edge "a" "a" [ "x" ] OnRefusal (Just "busy") ]
        == [ edge "a" "a" [ "x" ] OnRefusal (Just "busy") ]
    )
  j <- check "merging two refusals keeps the reason and both events"
    ( merge [ edge "a" "a" [ "x" ] OnRefusal (Just "busy"), edge "a" "a" [ "y" ] OnRefusal (Just "busy") ]
        == [ edge "a" "a" [ "x", "y" ] OnRefusal (Just "busy") ]
    )
  k <- check "a reason on only one of the group still survives"
    ( merge [ edge "a" "a" [ "x" ] OnRefusal Nothing, edge "a" "a" [ "y" ] OnRefusal (Just "busy") ]
        == [ edge "a" "a" [ "x", "y" ] OnRefusal (Just "busy") ]
    )

  -- Two refusals between the same pair with DIFFERENT tags collapse into one
  -- arrow carrying the first, and the second tag is gone. `reason` is a
  -- `Maybe RefusalId` rather than an array, so no display can say "these two
  -- presses are both refused, for these two different reasons". Recorded as
  -- present behaviour, not endorsed — see the report.
  l <- check "two DIFFERENT refusal tags collapse, keeping only the first (lossy, documented)"
    ( merge [ edge "a" "a" [ "x" ] OnRefusal (Just "busy"), edge "a" "a" [ "y" ] OnRefusal (Just "locked") ]
        == [ edge "a" "a" [ "x", "y" ] OnRefusal (Just "busy") ]
    )

  m <- check "merge is idempotent on a mixed array"
    (let once = merge mixed in merge once == once)
  n <- check "merge preserves the set of (from, to, kind) triples"
    (triples (merge mixed) == triples mixed)
  o <- check "merge of a mixed array has one arrow per distinct triple"
    (length (merge mixed) == length (triples mixed))

  pure [ a, b, c, d, e, f, g, h, i, j, k, l, m, n, o ]

-- =============================================================================
-- inboundTo
-- =============================================================================

inboundUnits :: Effect (Array Boolean)
inboundUnits = do
  section "Edges.inboundTo — a self-loop is not an arrival"

  let
    es =
      [ edge "a" "b" [ "x" ] OnEvent Nothing
      , edge "c" "b" [ "y" ] OnEvent Nothing
      , edge "b" "b" [ "z" ] OnEvent Nothing
      , edge "b" "b" [ "w" ] OnRefusal (Just "busy")
      , edge "b" "c" [ "v" ] OnEvent Nothing
      ]

  a <- check "arrivals from elsewhere are counted"
    (length (inboundTo (sid "b") es) == 2)
  b <- check "a self-loop is not an arrival"
    (map _.from (inboundTo (sid "b") es) == [ sid "a", sid "c" ])
  c <- check "a refusal self-loop is not an arrival either"
    (inboundTo (sid "b") [ edge "b" "b" [ "w" ] OnRefusal (Just "busy") ] == [])
  d <- check "a state nothing points at has no inbound edges"
    (inboundTo (sid "zzz") es == [])
  e <- check "outbound edges are not counted as inbound"
    (length (inboundTo (sid "c") es) == 1)

  pure [ a, b, c, d, e ]

-- =============================================================================
-- Reachability
-- =============================================================================

-- | `a --go--> b`, plus an island `c --go--> d` that nothing can arrive at.
island :: Spec
island = blank
  { initial = sid "a"
  , states = [ st "a", st "b", st "c", st "d" ]
  , events = [ ev "go" ]
  , refusals = [ refusal "no" "no" ]
  , rules =
      [ { from: sid "a", on: eid "go", cases: [ to always "b" ] }
      , { from: sid "c", on: eid "go", cases: [ to always "d" ] }
      , { from: sid "b", on: eid "go", cases: [ refuse always "no" ] }
      ]
  }

-- | One state whose only edge is a refusal on itself.
lonely :: Spec
lonely = blank
  { initial = sid "a"
  , states = [ st "a", st "b" ]
  , events = [ ev "go" ]
  , refusals = [ refusal "no" "no" ]
  , rules = [ { from: sid "b", on: eid "go", cases: [ refuse always "no" ] } ]
  }

reachabilityUnits :: Effect (Array Boolean)
reachabilityUnits = do
  section "Edges.reachableStates and unreachableStates"

  let es = edgesOf defaultOptions (worldFrom island) island

  a <- check "the initial state is always reachable, edges or not"
    (reachableStates blank [] == [ sid "a" ])
  b <- check "a state something moves to is reachable"
    (sort (reachableStates island es) == sort [ sid "a", sid "b" ])
  c <- check "a stranded island is reported unreachable, all of it"
    (sort (unreachableStates island es) == sort [ sid "c", sid "d" ])
  d <- check "reachable and unreachable together are exactly the declared states"
    ( sort (reachableStates island es <> unreachableStates island es)
        == sort (map _.id island.states)
    )

  -- A refusal is drawn as a self-loop, and a self-loop must not make its own
  -- state look reachable — otherwise every state carrying a refusal would
  -- silence the warning.
  e <- check "a refusal self-loop does not make its state reachable"
    (unreachableStates lonely (edgesOf defaultOptions (worldFrom lonely) lonely) == [ sid "b" ])

  -- This was a real defect, found by these tests and fixed on 2026-08-23.
  -- `reachableStates` was `initial <> every non-self edge target` — in-degree,
  -- not reachability — so `d` counted as reachable purely because the stranded
  -- `c` pointed at it, and only the *head* of a stranded component was ever
  -- warned about. It is a transitive closure now, and these two pin that.
  f <- check "transitive: reached only from an unreachable state does not count"
    (not (elem (sid "d") (reachableStates island es)))
  g <- check "the whole stranded component is warned about, not just its head"
    (sort (unreachableStates island es) == sort [ sid "c", sid "d" ])

  pure [ a, b, c, d, e, f, g ]

-- =============================================================================
-- edgesOf
-- =============================================================================

-- | `a` resolves by itself after two ticks; `b` swallows a press; `c` refuses.
timed :: Spec
timed = blank
  { initial = sid "a"
  , states = [ st "a", st "b", st "c" ]
  , events = [ ev "tick", ev "press" ]
  , refusals = [ refusal "no" "no" ]
  , rules =
      [ { from: sid "a", on: eid "tick", cases: [ to always "b" ] }
      , { from: sid "b", on: eid "press", cases: [ stay always ] }
      , { from: sid "c", on: eid "press", cases: [ refuse always "no" ] }
      ]
  , deadlines = [ { inState: sid "a", fires: eid "tick", after: Lit (VNumber 2.0) } ]
  }

edgesOfUnits :: Effect (Array Boolean)
edgesOfUnits = do
  section "Edges.edgesOf — what a machine's arrows actually are"

  let world = worldFrom timed
  let shown = edgesOf defaultOptions world timed
  let hidden = edgesOf { showRefusals: false } world timed
  let ofKind k = filter (\e -> e.kind == k)
  let from s = filter (\e -> e.from == s)

  a <- check "showRefusals false drops exactly the refusal arrows"
    (length shown - length hidden == length (ofKind OnRefusal shown) && ofKind OnRefusal hidden == [])
  b <- check "showRefusals true is the default, and there is one refusal here"
    (length (ofKind OnRefusal shown) == 1)
  c <- check "a Stay produces no arrow at all"
    (from (sid "b") shown == [])
  d <- check "a deadline produces an arrow of its own kind"
    (map _.from (ofKind OnDeadline shown) == [ sid "a" ])
  e <- check "the deadline's target comes from the rules, not from the deadline"
    (map _.to (ofKind OnDeadline shown) == [ sid "b" ])

  -- The event a deadline delivers is not also probed as though a person could
  -- press it, or the same transition would be drawn twice — once truthfully and
  -- once as a button nobody has.
  f <- check "an event delivered by a deadline is not ALSO drawn as a user arrow"
    (ofKind OnEvent (from (sid "a") shown) == [])

  g <- check "a refusal is a self-loop carrying its tag"
    ( map _.reason (ofKind OnRefusal shown) == [ Just (RefusalId "no") ]
        && map (\e -> e.from == e.to) (ofKind OnRefusal shown) == [ true ]
    )

  pure [ a, b, c, d, e, f, g ]

edgeUnits :: Effect (Array Boolean)
edgeUnits = do
  a <- mergeUnits
  b <- inboundUnits
  c <- reachabilityUnits
  d <- edgesOfUnits
  pure (a <> b <> c <> d)
