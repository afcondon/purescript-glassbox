-- | Test.Unit.Lint
-- |
-- | `the-same-machine.md` sets the acceptance criterion for this module and it
-- | is not "the lints run":
-- |
-- | > Take the loop machine, and produce one holed variant per failure mode the
-- | > exhaustive `case` would have caught. Every one must be rejected, **by
-- | > name**, with a message that points at the offending pair. A single hole
-- | > that slips through means the FFI trade was a bad one.
-- |
-- | So each test below starts from a machine that passes, makes exactly one
-- | hole, and asserts both that it is caught and that the right lint caught it.
-- | A finding from the wrong lint is a failure here, because a check that says
-- | only "invalid" has replaced a type error with a shrug.
module Test.Unit.Lint
  ( lintUnits
  ) where

import Prelude

import Data.Array (any, filter, length, null)
import Data.Maybe (Maybe(..))
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Glassbox.Lint (Severity(..), lint, accepts, rejections, warnings)
import Glassbox.Spec (Outcome(..), Spec, StateId(..))
import Test.Fixture (always, blank, cid, command, doing, ev, refusal, refuse, sid, st, stay, to)
import Test.Harness (check, section)

-- | A small machine that passes every lint: two states, two events, a command
-- | on entry, a refusal, and a rule for all four pairs.
sound :: Spec
sound = blank
  { id = "sound"
  , initial = sid "off"
  , states = [ st "off", doing "on" [ "light-lamp" ] [] ]
  , events = [ ev "flip", ev "poke" ]
  , commands = [ command "light-lamp" ]
  , refusals = [ refusal "already-on" "it is already on" ]
  , rules =
      [ { from: sid "off", on: ev' "flip", cases: [ to always "on" ] }
      , { from: sid "off", on: ev' "poke", cases: [ stay always ] }
      , { from: sid "on", on: ev' "flip", cases: [ to always "off" ] }
      , { from: sid "on", on: ev' "poke", cases: [ refuse always "already-on" ] }
      ]
  }
  where
  ev' = (_.id) <<< ev

-- | Did exactly the named lint reject this, and does the message point at the
-- | thing that is wrong?
rejectedBy :: String -> String -> Spec -> Boolean
rejectedBy name needle spec =
  let found = rejections (lint spec)
  in not (null found)
    && any (\f -> f.lint == name && contains (Pattern needle) f.message) found

warnedBy :: String -> String -> Spec -> Boolean
warnedBy name needle spec =
  any (\f -> f.lint == name && contains (Pattern needle) f.message) (warnings (lint spec))

lintUnits :: Effect (Array Boolean)
lintUnits = do
  section "Lint"

  a <- check "a well-formed machine produces no findings at all"
    (null (lint sound) && accepts sound)

  -- Totality: the check the exhaustive `case` used to do.
  b <- check "a missing (state x event) pair is rejected, naming both"
    ( rejectedBy "totality" "`on` has no declared outcome for `poke`"
        (sound { rules = filter (\r -> not (r.from == sid "on" && r.on == sid' "poke")) sound.rules })
    )

  -- Closure: a target that is not a state.
  c <- check "a transition to an undeclared state is rejected, naming the target"
    ( rejectedBy "closure" "moves to `ghost`"
        (sound { rules = [ { from: sid "off", on: sid' "flip", cases: [ to always "ghost" ] } ] <> drop1 sound.rules })
    )
  d <- check "an undeclared initial state is rejected"
    (rejectedBy "closure" "the initial state `nowhere`" (sound { initial = sid "nowhere" }))
  e <- check "a refusal tag that is not declared is rejected"
    (rejectedBy "closure" "refuses with `already-on`" (sound { refusals = [] }))

  -- Vocabulary: the runtime publishes a closed list.
  f <- check "an entry command that is not declared is rejected, naming the state"
    (rejectedBy "vocabulary" "the entry of `on` names the command `light-lamp`" (sound { commands = [] }))

  -- Overlap: determinism's replacement. Warns, never rejects.
  g <- check "a case hidden behind an unguarded one warns and does not reject"
    ( let holed = sound
            { rules =
                [ { from: sid "off", on: sid' "flip", cases: [ to always "on", to always "off" ] } ]
                  <> drop1 sound.rules
            }
      in warnedBy "overlap" "can never fire" holed && accepts holed
    )
  h <- check "a second rule for the same pair warns that only the first is found"
    ( warnedBy "overlap" "more than one rule"
        (sound { rules = sound.rules <> [ { from: sid "off", on: sid' "flip", cases: [ stay always ] } ] })
    )

  -- Reachability: a warning by design, because a state stranded under one
  -- configuration and not another is not a mistake.
  -- An orphan needs its own rules, or totality rejects it before reachability
  -- gets a look in — which is itself worth pinning, since it is the reason the
  -- two lints cannot be tested with the same fixture.
  let
    stranded = sound
      { states = sound.states <> [ st "orphan" ]
      , rules = sound.rules <>
          [ { from: sid "orphan", on: sid' "flip", cases: [ stay always ] }
          , { from: sid "orphan", on: sid' "poke", cases: [ stay always ] }
          ]
      }

  i <- check "an unreachable state warns, naming it and the initial state"
    (warnedBy "reachability" "`orphan` cannot be reached from `off`" stranded)
  j <- check "reachability does not gate the boot"
    (accepts stranded)
  j2 <- check "an orphan with no rules of its own fails totality first"
    (not (accepts (sound { states = sound.states <> [ st "orphan" ] })))

  -- The distinction the two severities are carrying.
  k <- check "every finding is a rejection or a warning and never both"
    ( let fs = lint (sound { states = [], rules = [], commands = [] })
      in length fs == length (rejections fs) + length (warnings fs)
    )
  l <- check "a machine with several holes reports all of them, not the first"
    (length (rejections (lint (sound { commands = [], refusals = [] }))) >= 2)

  pure [ a, b, c, d, e, f, g, h, i, j, j2, k, l ]
  where
  sid' = (_.id) <<< ev
  drop1 xs = filter (\r -> not (r.from == sid "off" && r.on == sid' "flip")) xs
