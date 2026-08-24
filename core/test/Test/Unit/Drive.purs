-- | Test.Unit.Drive
-- |
-- | `Glassbox.Drive` claims to run a machine in **any monad**, and the way to
-- | test that claim is to run one somewhere that is not a UI at all. Everything
-- | below drives the machine in `State`, with `fork` supplied as `identity` so
-- | commands run inline and the log is deterministic.
-- |
-- | That is not a convenience for testing. It is the acceptance criterion: the
-- | driver came out of a Halogen component, and if any of it had still been
-- | about Halogen this module could not exist.
module Test.Unit.Drive
  ( driveUnits
  ) where

import Prelude

import Control.Monad.State (State, execState, gets, modify_)
import Data.Array (elem, snoc)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Glassbox.Drive (Wiring, enter, fire, start)
import Glassbox.Run (Alarm, worldFrom)
import Glassbox.Spec (Clock(..), CommandId(..), EventId(..), Expr(..), RefusalId(..), Spec, StateId(..), Value(..))
import Test.Fixture (always, blank, command, doing, ev, refusal, refuse, sid, st, stay, to)
import Test.Harness (check, section)

-- | Everything a host would keep, and a log of what it was asked to do.
type Host =
  { phase :: StateId
  , did :: Array String
  , alarm :: Maybe Alarm
  }

-- | A machine with one of everything the driver has a rule about: an entry
-- | command, an exit command, a refusal, a `stay`, a deadline, and a command
-- | that answers with an event.
machine :: Spec
machine = blank
  { id = "drive"
  , initial = sid "idle"
  , clock = Just Milliseconds
  , states =
      [ st "idle"
      , doing "working" [ "begin" ] [ "cleanup" ]
      , st "done"
      ]
  , events = [ ev "go", ev "finished", ev "poke" ]
  , commands = [ command "begin", command "cleanup" ]
  , refusals = [ refusal "not-yet" "nothing to do" ]
  , rules =
      [ { from: sid "idle", on: eid "go", cases: [ to always "working" ] }
      , { from: sid "idle", on: eid "finished", cases: [ stay always ] }
      , { from: sid "idle", on: eid "poke", cases: [ refuse always "not-yet" ] }
      , { from: sid "working", on: eid "finished", cases: [ to always "done" ] }
      , { from: sid "working", on: eid "go", cases: [ stay always ] }
      , { from: sid "working", on: eid "poke", cases: [ stay always ] }
      , { from: sid "done", on: eid "go", cases: [ to always "working" ] }
      , { from: sid "done", on: eid "finished", cases: [ stay always ] }
      , { from: sid "done", on: eid "poke", cases: [ stay always ] }
      ]
  , deadlines = [ { inState: sid "working", fires: eid "finished", after: Lit (VNumber 250.0) } ]
  }
  where
  eid = EventId

-- | `begin` answers with an event, which is how the format says "and then this
-- | happened" without a continuation. `cleanup` answers with nothing.
wiring :: Wiring (State Host)
wiring =
  { spec: machine
  , phase: gets _.phase
  , move: \p -> modify_ (\h -> h { phase = p, did = snoc h.did ("-> " <> raw p) })
  , world: pure (worldFrom machine)
  , perform: \(CommandId c) -> do
      modify_ (\h -> h { did = snoc h.did c })
      pure (if c == "begin" then Just (EventId "finished") else Nothing)
  , refused: \(RefusalId r) -> modify_ (\h -> h { did = snoc h.did ("refused " <> r) })
  -- Inline rather than deferred, so the log is an order rather than a race.
  , fork: identity
  , arm: \a -> modify_ (\h -> h { alarm = a, did = snoc h.did (armText a) })
  }
  where
  raw (StateId s) = s
  armText = case _ of
    Nothing -> "disarm"
    Just a -> "arm " <> show a.after

run :: StateId -> State Host Unit -> Host
run from action = execState action { phase: from, did: [], alarm: Nothing }

driveUnits :: Effect (Array Boolean)
driveUnits = do
  section "Drive — a machine running somewhere that is not a UI"

  a <- check "a Move writes the phase before it runs anything"
    ( (run (sid "idle") (fire wiring (EventId "go"))).did
        == [ "-> working", "arm 250.0", "begin", "-> done", "disarm", "cleanup" ]
    )

  -- That log IS the design, read left to right: the phase moved, the alarm was
  -- set, the entry command ran, its answering event came back in as an ordinary
  -- event, arriving somewhere with no deadline cancelled the one that was
  -- running, and leaving `working` ran its exit command last. Six things, one
  -- press, and not one of them written by the host.
  b <- check "a command that answers with an event feeds it straight back in"
    ((run (sid "idle") (fire wiring (EventId "go"))).phase == sid "done")
  c <- check "and the alarm it was carrying is cancelled by the state it reached"
    ((run (sid "idle") (fire wiring (EventId "go"))).alarm == Nothing)

  d <- check "a refusal is reported and commands nothing"
    ((run (sid "idle") (fire wiring (EventId "poke"))).did == [ "refused not-yet" ])
  e <- check "and leaves the phase exactly where it was"
    ((run (sid "idle") (fire wiring (EventId "poke"))).phase == sid "idle")

  -- The one that is easy to get wrong, and expensive: a `stay` is a statechart
  -- INTERNAL transition. Re-arming here would make every ignored keypress
  -- quietly extend a running timeout.
  f <- check "a stay does nothing at all — no move, no command, no re-arm"
    ((run (sid "working") (fire wiring (EventId "poke"))).did == [])

  g <- check "an event the machine has no rule for is simply not an event"
    ((run (sid "idle") (fire wiring (EventId "sneeze"))).did == [])

  -- `Move` to the state you are already in exits and re-enters, which is the
  -- external self-transition and is where a re-arm IS wanted.
  h <- check "entering a state with a deadline arms it, at the moment of entry"
    (elem "arm 250.0" (run (sid "idle") (enter wiring (sid "working"))).did)
  i <- check "entering runs the entry commands, which fire alone cannot be asked to do"
    ( (run (sid "idle") (enter wiring (sid "working"))).did
        == [ "-> working", "arm 250.0", "begin", "-> done", "disarm", "cleanup" ]
    )

  j <- check "start enters the machine's own initial state"
    ((run (sid "elsewhere") (start wiring)).phase == sid "idle")
  k <- check "and a state that asks for nothing on entry disarms rather than leaving one running"
    ((run (sid "elsewhere") (start wiring)).did == [ "-> idle", "disarm" ])

  pure [ a, b, c, d, e, f, g, h, i, j, k ]
