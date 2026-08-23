-- | The Blocks syntax, printed and read back.
-- |
-- | The acceptance test is the same one the JSON codec has, with one clause
-- | added that we learned the hard way: **prose must survive too**. The
-- | behavioural round-trip alone passed for a whole day while the codec was
-- | silently discarding seventy lines of design commentary, because comparing
-- | outcomes cannot see content that no outcome depends on.
module Test.Unit.DSL (dslUnits, roundTripsMachine) where

import Prelude

import Data.Array (all, length)
import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Glassbox.DSL.Parse (parseBlocks)
import Glassbox.DSL.Print (Syntax(..), print)
import Glassbox.Run (resolve, worldFrom)
import Glassbox.Spec
  ( EventId(..)
  , Guard(..)
  , Outcome(..)
  , Spec
  , StateId
  , commandIds
  , eventIds
  , stateIds
  )
import Test.Fixture (blank, ev, sid, st)
import Test.Harness (check, section)

-- =============================================================================
-- Round trip over the corpus
-- =============================================================================

-- | Behaviour, prose, and stability of the printed form.
-- |
-- | Printing is checked for **idempotence through a parse** rather than for
-- | `parse (print s) == s`, because the printer groups events that share a case
-- | list and grouping does not preserve the order of interleaved rules. Rule
-- | order is behaviourally inert for distinct `(from, on)` pairs, so what
-- | matters is that an author's file is stable once written — which this is.
roundTripsMachine :: String -> Spec -> Effect (Array Boolean)
roundTripsMachine name spec = do
  let text = print Blocks spec
  case parseBlocks text of
    Left err -> do
      a <- check (name <> " parses back — " <> err) false
      pure [ a ]
    Right back -> do
      let world = worldFrom spec
      let
        sameBehaviour = all
          (\(Tuple s e) -> resolve spec world s e == resolve back world s e)
          (crossProduct spec)
      a <- check (name <> ": identical on every (state x event)") sameBehaviour
      b <- check (name <> ": prose survives") (spec.notes == back.notes)
      c <- check (name <> ": printing is idempotent through a parse")
        (print Blocks back == text)
      d <- check (name <> ": declarations survive")
        ( stateIds spec == stateIds back
            && eventIds spec == eventIds back
            && length spec.config == length back.config
            && length spec.facts == length back.facts
            && length spec.refusals == length back.refusals
            && length spec.deadlines == length back.deadlines
            && commandIds spec == commandIds back
        )
      -- Separately from the declarations, because the commands a state runs are
      -- written on the block and folded back onto the declaration, and an
      -- ORDERED comparison is the only one that would notice a reversal.
      e <- check (name <> ": what each state does on the way in and out survives")
        (map hooks spec.states == map hooks back.states)
      pure [ a, b, c, d, e ]
  where
  hooks s = Tuple s.entry s.exit

crossProduct :: Spec -> Array (Tuple StateId EventId)
crossProduct spec = do
  s <- stateIds spec
  e <- eventIds spec
  pure (Tuple s e)

-- =============================================================================
-- The awkward corners, as fixtures rather than corpus
-- =============================================================================

dslUnits :: Effect (Array Boolean)
dslUnits = do
  section "DSL.Blocks — print and parse"

  a <- check "an empty-ish machine round-trips"
    ( case parseBlocks (print Blocks blank) of
        Right back -> back.id == blank.id && back.initial == blank.initial
        Left _ -> false
    )

  -- `always` and `never` are how the empty connectives print, so they must not
  -- come back as facts that happen to be spelled that way.
  b <- check "always parses as the empty conjunction, never as the empty disjunction"
    ( case parseBlocks (print Blocks alwaysNever) of
        Right back -> map _.cases back.rules == map _.cases alwaysNever.rules
        Left _ -> false
    )

  c <- check "an unknown bare name in a guard is refused, not guessed"
    (isLeft (parseBlocks unknownName))

  -- This one failed first time not on the escaping, which was fine, but
  -- because a machine with no rules prints a `!!` marker the parser could not
  -- read back. The fixture is doing double duty; both are worth pinning.
  d <- check "a quoted label survives, and so does a state with no rules"
    ( case parseBlocks (print Blocks quotedLabel) of
        Right back -> map _.label back.states == map _.label quotedLabel.states
        Left _ -> false
    )

  e <- check "final is read off the block header, not the states section"
    ( case parseBlocks (print Blocks withFinal) of
        Right back -> map _.final back.states == [ false, true ]
        Left _ -> false
    )

  f <- check "nonsense is a parse error rather than an empty machine"
    (isLeft (parseBlocks "this is not a machine"))

  pure [ a, b, c, d, e, f ]

alwaysNever :: Spec
alwaysNever = blank
  { states = [ st "a" ]
  , events = [ ev "go", ev "no" ]
  , rules =
      [ { from: sid "a", on: EventId "go", cases: [ { when: Just (And []), outcome: Stay } ] }
      , { from: sid "a", on: EventId "no", cases: [ { when: Just (Or []), outcome: Stay } ] }
      ]
  }

quotedLabel :: Spec
quotedLabel = blank
  { states = [ (st "a") { label = "an \"awkward\" label" } ]
  }

withFinal :: Spec
withFinal = blank
  { states = [ st "a", (st "b") { final = true } ]
  , events = [ ev "go" ]
  , rules =
      [ { from: sid "a", on: EventId "go", cases: [ { when: Nothing, outcome: Move (sid "b") } ] }
      , { from: sid "b", on: EventId "go", cases: [ { when: Nothing, outcome: Stay } ] }
      ]
  }

unknownName :: String
unknownName =
  "machine x\nformat 1\ninitial a\n\nstates\n  a\n\nevents\n  go\n\na:\n  go  when nobody-declared-this  -> stay\n"
