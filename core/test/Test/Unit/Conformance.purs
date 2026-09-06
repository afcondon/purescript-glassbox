-- | Test.Unit.Conformance
-- |
-- | `Glassbox.Conformance` is the table another runtime is held to, so what
-- | matters about it is coverage and honesty: every pair is in it, every
-- | branch a guard can take is in it, nothing that cannot change an answer
-- | pads it out, and each line says what `step` says.
module Test.Unit.Conformance
  ( conformanceUnits
  ) where

import Prelude

import Data.Array (all, any, filter, foldl, length, nub)
import Data.Maybe (Maybe(..))
import Data.String (contains, split)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Glassbox.Conformance (Vector, candidatesFor, printSuite, suiteFor)
import Glassbox.Run (World, setConfig, setFact, step, worldFrom)
import Glassbox.Spec (CmpOp(..), ConfigId(..), Expr(..), FactId(..), Guard(..), Outcome(..), RefusalId(..), Spec, Value(..))
import Test.Fixture (always, blank, cfg, command, doing, eid, ev, fact, refusal, refuse, sid, st, stay, to)
import Test.Harness (check, section)

-- | Two facts and a numeric config, but only one rule reads any of them —
-- | which is the point: the enumeration should be wide where it has to be and
-- | one line everywhere else.
machine :: Spec
machine = blank
  { id = "table"
  , initial = sid "a"
  , states = [ st "a", doing "b" [ "enter-b" ] [ "leave-b" ] ]
  , events = [ ev "go", ev "back" ]
  , config = [ cfg "limit" (VNumber 3.0) ]
  , facts = [ fact "held", fact "idle" ]
  , commands = [ command "enter-b", command "leave-b" ]
  , refusals = [ refusal "busy" "held elsewhere" ]
  , rules =
      [ { from: sid "a"
        , on: eid "go"
        , cases:
            [ refuse (Just (Holds (FactOf (FactId "held")))) "busy"
            , to (Just (Cmp OpGt (ConfigOf (ConfigId "limit")) (Lit (VNumber 0.0)))) "b"
            , stay always
            ]
        }
      , { from: sid "a", on: eid "back", cases: [ stay always ] }
      , { from: sid "b", on: eid "go", cases: [ stay always ] }
      , { from: sid "b", on: eid "back", cases: [ to always "a" ] }
      ]
  }

conformanceUnits :: Effect (Array Boolean)
conformanceUnits = do
  section "Conformance — the table another runtime is held to"
  let suite = suiteFor machine
  let pairs = nub (map (\v -> Tuple v.from v.event) suite.vectors)

  a <- check "every (state x event) pair appears at least once"
    (length pairs == 4)

  -- `a x go` reads one fact and one config with two candidate values (its
  -- default 3 and the 0 it is compared against): four lines. The other three
  -- pairs read nothing and get one line each.
  b <- check "a guarded pair is enumerated over what it reads, and only that"
    (length (filter (\v -> v.from == sid "a" && v.event == eid "go") suite.vectors) == 4)
  c <- check "an unguarded pair is one line"
    (length suite.vectors == 7)
  d <- check "the fact the other rules never read is absent from their lines"
    ( all (\v -> v.assignment.facts == [])
        (filter (\v -> not (v.from == sid "a" && v.event == eid "go")) suite.vectors)
    )

  e <- check "config candidates are the default and the literals it is compared against"
    (candidatesFor machine (ConfigId "limit") == [ VNumber 3.0, VNumber 0.0 ])

  f <- check "every line says what step says under its own world"
    ( all
        ( \v ->
            let
              world = applied v
              Tuple current record = step machine world v.from v.event
            in
              current == v.current && record.outcome == v.outcome && record.commands == v.commands
        )
        suite.vectors
    )

  g <- check "the refusal branch, the move and the fall-through are all present"
    ( any (\v -> v.outcome == Refuse (RefusalId "busy")) suite.vectors
        && any (\v -> v.outcome == Move (sid "b")) suite.vectors
        && any (\v -> v.from == sid "a" && v.event == eid "go" && v.outcome == Stay) suite.vectors
    )

  h <- check "the canonical text rides along, and the start commands"
    (contains (Pattern "glassbox-canonical 1") suite.canonical && suite.start == [])

  let text = printSuite suite
  i <- check "the printed file is one vector per line, with the suite version first"
    ( contains (Pattern "\"glassbox-vectors\": 1,") text
        && length (filter (contains (Pattern "\"outcome\"")) (split (Pattern "\n") text)) == 7
    )

  pure [ a, b, c, d, e, f, g, h, i ]

-- | The world a vector describes, rebuilt the way a replayer would: defaults,
-- | then the config it names, then the facts it names.
applied :: Vector -> World
applied v =
  foldl (\w (Tuple k val) -> setFact k val w)
    (foldl (\w (Tuple k val) -> setConfig k val w) (worldFrom machine) v.assignment.config)
    v.assignment.facts
