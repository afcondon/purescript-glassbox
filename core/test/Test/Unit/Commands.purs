-- | The effects fork, pinned.
-- |
-- | Every decision in `docs/kb/plans/glassbox-open-forks.md` that could have
-- | gone the other way gets an assertion here, because each of them is a
-- | judgement rather than a consequence and a future change would otherwise
-- | look like a refactor.
-- |
-- | The one worth reading twice is `stayIsInternalMoveIsExternal`. Statecharts
-- | need an explicit annotation to say whether re-entering a state re-runs its
-- | entry action; this format gets the distinction free from a field it has for
-- | other reasons, and that is only true for as long as these two lines hold.
module Test.Unit.Commands (commandUnits) where

import Prelude

import Data.Array (length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Glassbox.Codec.JSON (decodeSpec, encodeSpec, parseSpec, printSpec)
import Glassbox.DSL.Parse (parseBlocks)
import Glassbox.DSL.Print (Syntax(..), print)
import Glassbox.Run (commandsFor, startCommands, step, worldFrom)
import Glassbox.Spec
  ( CommandId(..)
  , EventId(..)
  , Outcome(..)
  , RefusalId(..)
  , Spec
  , StateId(..)
  , entryOf
  , exitOf
  , requiredVersion
  , versionFor
  )
import Test.Fixture (always, blank, cid, command, doing, ev, refusal, refuse, sid, st, stay, to)
import Test.Harness (check, section)

-- =============================================================================
-- The machine under test
-- =============================================================================

-- | Two states that do something, one that does nothing, and a refusal
-- | reachable from a state with an exit command — which is the case the "a
-- | refusal commands nothing" rule is actually about.
--
--   idle  --go-->  busy       busy  --go-->  busy   (external self-transition)
--   busy  --hold->  (stay)                          (internal)
--   busy  --no-->  refused
wired :: Spec
wired = blank
  { id = "wired"
  , initial = sid "idle"
  , states =
      [ doing "idle" [ "light-lamp" ] [ "dim-lamp" ]
      , doing "busy" [ "spin-up", "open-valve" ] [ "close-valve" ]
      , st "quiet"
      ]
  , events = [ ev "go", ev "hold", ev "no", ev "away" ]
  , refusals = [ refusal "busy-now" "it is busy" ]
  , commands =
      [ command "light-lamp"
      , command "dim-lamp"
      , command "spin-up"
      , command "open-valve"
      , command "close-valve"
      ]
  , rules =
      [ { from: sid "idle", on: EventId "go", cases: [ to always "busy" ] }
      , { from: sid "busy", on: EventId "go", cases: [ to always "busy" ] }
      , { from: sid "busy", on: EventId "hold", cases: [ stay always ] }
      , { from: sid "busy", on: EventId "no", cases: [ refuse always "busy-now" ] }
      , { from: sid "busy", on: EventId "away", cases: [ to always "quiet" ] }
      ]
  }

commandUnits :: Effect (Array Boolean)
commandUnits = do
  section "Commands — the effects fork"

  let world = worldFrom wired

  -- --- the three clauses of commandsFor -------------------------------------

  a <- check "a move exits the old state and then enters the new one, in that order"
    ( commandsFor wired (sid "idle") (Move (sid "busy"))
        == map CommandId [ "dim-lamp", "spin-up", "open-valve" ]
    )

  b <- check "Stay is an internal transition and commands nothing"
    (commandsFor wired (sid "busy") Stay == [])

  c <- check "a refusal commands nothing, even leaving a state that has an exit command"
    (commandsFor wired (sid "busy") (Refuse (RefusalId "busy-now")) == [])

  -- --- the free statechart distinction --------------------------------------

  d <- check "Move to the state you are already in DOES exit and re-enter"
    ( commandsFor wired (sid "busy") (Move (sid "busy"))
        == map CommandId [ "close-valve", "spin-up", "open-valve" ]
    )

  e <- check "and Stay in the same place does not — the two are genuinely different"
    (commandsFor wired (sid "busy") (Move (sid "busy")) /= commandsFor wired (sid "busy") Stay)

  -- --- a state with no commands ---------------------------------------------

  f <- check "a state that declares nothing contributes nothing to either side"
    ( commandsFor wired (sid "busy") (Move (sid "quiet"))
        == map CommandId [ "close-valve" ]
    )

  g <- check "and a state nobody declared has no commands rather than an error"
    (entryOf wired (sid "ghost") == [] && exitOf wired (sid "ghost") == [])

  -- --- entering the machine -------------------------------------------------

  h <- check "starting the machine owes the initial state's entry commands"
    (startCommands wired == [ cid "light-lamp" ])

  i <- check "and a machine whose initial state does nothing owes nothing"
    (startCommands (wired { initial = sid "quiet" }) == [])

  -- --- step carries them out ------------------------------------------------

  let Tuple _ record = step wired world (sid "idle") (EventId "go")
  j <- check "step reports the commands alongside where it went"
    ( record.current == StateId "busy"
        && record.commands == map CommandId [ "dim-lamp", "spin-up", "open-valve" ]
    )

  let Tuple _ refusedStep = step wired world (sid "busy") (EventId "no")
  k <- check "and a refused step reports the refusal with an empty command list"
    (refusedStep.outcome == Refuse (RefusalId "busy-now") && refusedStep.commands == [])

  -- --- versioning -----------------------------------------------------------

  l <- check "a machine with commands needs format 2"
    (requiredVersion wired == 2)

  m <- check "a machine without them is still a format 1 machine"
    (requiredVersion (blank { states = [ st "a" ] }) == 1)

  n <- check "declaring a command counts, even with no state using it"
    (requiredVersion (blank { commands = [ command "unused" ] }) == 2)

  o <- check "a version already above what is needed is left alone"
    (versionFor (blank { version = 7 }) == 7)

  p <- check "and an artifact stamps the version it needs, not the one it was handed"
    (mentionsIn "\"glassbox\":2" (printSpec wired))

  q <- check "a commandless machine is still written as format 1"
    (mentionsIn "\"glassbox\":1" (printSpec (blank { states = [ st "a" ] })))

  -- --- both codecs ----------------------------------------------------------

  r <- check "commands survive the JSON round trip, in order"
    (sameCommands wired (viaJson wired))

  s <- check "and the Blocks round trip"
    (sameCommands wired (viaBlocks wired))

  t <- check "the command declarations survive too"
    ( map (map _.id <<< _.commands) (viaJson wired) == Just (map CommandId
        [ "light-lamp", "dim-lamp", "spin-up", "open-valve", "close-valve" ])
    )

  -- --- the parser is closed over the declared vocabulary --------------------

  u <- check "an entry line naming a command nobody declared is refused, not invented"
    ( case parseBlocks undeclaredCommand of
        Left _ -> true
        Right _ -> false
    )

  v <- check "the JSON side accepts it (documented — that is the closure lint's job)"
    ( case parseSpec undeclaredCommandJson of
        Left _ -> false
        Right back -> entryOf back (sid "a") == [ cid "nobody-declared-this" ]
    )

  pure [ a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v ]

-- =============================================================================
-- Helpers
-- =============================================================================

viaJson :: Spec -> Maybe Spec
viaJson spec = case decodeSpec (encodeSpec spec) of
  Left _ -> Nothing
  Right back -> Just back

viaBlocks :: Spec -> Maybe Spec
viaBlocks spec = case parseBlocks (print Blocks spec) of
  Left _ -> Nothing
  Right back -> Just back

-- | Compared per state and in order, because the order commands run in is the
-- | whole of their meaning and a set comparison would pass on a reversal.
sameCommands :: Spec -> Maybe Spec -> Boolean
sameCommands spec = case _ of
  Nothing -> false
  Just back ->
    length spec.states == length back.states
      && map hooks spec.states == map hooks back.states
  where
  hooks s = Tuple s.entry s.exit

mentionsIn :: String -> String -> Boolean
mentionsIn needle = String.contains (Pattern needle)

undeclaredCommand :: String
undeclaredCommand =
  "machine x\nformat 2\ninitial a\n\nstates\n  a\n\nevents\n  go\n\na:\n  entry  nobody-declared-this\n  go  -> stay\n"

undeclaredCommandJson :: String
undeclaredCommandJson =
  """{"glassbox":2,"id":"x","initial":"a","states":[{"id":"a","entry":["nobody-declared-this"]}]}"""
