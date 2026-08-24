-- | Phase 1 acceptance tests for `docs/kb/plans/the-same-machine.md`.
-- |
-- | The headline test is `carRadioNeedsNoPureScript`. Everything else here
-- | supports it. If a machine can only be added by writing PureScript then the
-- | thesis has failed and no amount of round-tripping rescues it.
module Test.Main where

import Prelude

import Data.Array (all, elem, filter, length, null)
import Data.Array as Array
import Data.Foldable (any)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (for, sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Glassbox.Codec.JSON (decodeSpec, encodeSpec, parseSpec)
import Glassbox.Edges (defaultOptions, edgesOf, inboundTo)
import Glassbox.Run (commandsFor, dueAt, resolve, setConfig, setFact, startCommands, step, worldFrom)
import Glassbox.Spec
  ( CommandId(..)
  , ConfigId(..)
  , EventId(..)
  , FactId(..)
  , Outcome(..)
  , RefusalId(..)
  , Spec
  , StateId(..)
  , Value(..)
  , entryOf
  , eventIds
  , exitOf
  , stateIds
  , userEvents
  )
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, readTextFile)
import Node.Process (exit', setExitCode)
import Test.Harness (check, section)
import Test.Property (properties)
import Test.Unit.Codec (codecUnits)
import Test.Unit.Commands (commandUnits)
import Test.Unit.Edges (edgeUnits)
import Test.Unit.DSL (dslUnits, roundTripsMachine)
import Test.Unit.Export (exportUnits)
import Test.Unit.Lint (lintUnits)
import Test.Unit.Run (runUnits)

-- =============================================================================
-- Harness
-- =============================================================================
--
-- `check` and `section` moved to `Test.Harness` when the library suite grew
-- past this file; the style is unchanged — a plain-Effect runner printing
-- `ok`/`FAIL` and counting — and `Test.Harness.prop` folds a QuickCheck
-- property into the same tally so one run still gives one verdict.

-- | spago may run tests from the package directory or from the workspace root.
-- |
-- | These five artifacts are the library's conformance corpus, not the demo's
-- | content — the demo copies them, not the other way round.
readMachine :: String -> Effect String
readMachine name = do
  let local = "machines/" <> name
  let fromRoot = "core/machines/" <> name
  here <- exists local
  readTextFile UTF8 (if here then local else fromRoot)

-- | The five artifacts every corpus-wide check runs over.
corpus :: Array String
corpus = [ "loop.json", "car-radio.json", "elevator.json", "washing-machine.json", "heating.json" ]

loadOrDie :: String -> Effect Spec
loadOrDie name = do
  text <- readMachine name
  case parseSpec text of
    Left err -> do
      log ("  FATAL could not parse " <> name <> ": " <> err)
      exit' 1
    Right spec -> pure spec

-- =============================================================================
-- The acceptance test
-- =============================================================================

-- | **The phase 1 acceptance criterion.**
-- |
-- | `car-radio.json` was authored as a file. No constructor names its states, no
-- | module was recompiled to add it, and nothing in this repository knows what a
-- | car radio is. It must nonetheless load, run, refuse, and draw.
carRadioNeedsNoPureScript :: Effect (Array Boolean)
carRadioNeedsNoPureScript = do
  section "A machine that exists only as a file"
  spec <- loadOrDie "car-radio.json"
  let world = worldFrom spec

  a <- check "loads and reports its own identity"
    (spec.id == "car-radio" && spec.title == "Car radio")

  b <- check "declares five states and six events"
    (length (stateIds spec) == 5 && length (eventIds spec) == 6)

  -- off --power--> radio --seek--> seeking
  let Tuple s1 _ = step spec world spec.initial (EventId "power")
  let Tuple s2 _ = step spec world s1 (EventId "seek")
  c <- check "runs: off -power-> radio -seek-> seeking"
    (s1 == StateId "radio" && s2 == StateId "seeking")

  -- The seek gives up by itself after the configured interval.
  d <- check "seeking resolves by itself after seek-seconds"
    (dueAt spec world (StateId "seeking") 0.0 == Just { fires: EventId "giveup", at: 8.0 })

  -- A refusal carries a tag, not prose, and not silence.
  e <- check "ejecting with no disc refuses by tag"
    (resolve spec world (StateId "radio") (EventId "eject") == Refuse (RefusalId "no-disc"))

  -- Facts are supplied by the host and change what an event means.
  let loaded = setFact (FactId "disc-loaded") (VBoolean true) world
  f <- check "with a disc loaded, source reaches CD"
    (resolve spec loaded (StateId "radio") (EventId "source") == Move (StateId "cd"))
  g <- check "with no disc, the same press refuses"
    (resolve spec world (StateId "radio") (EventId "source") == Refuse (RefusalId "no-disc"))

  h <- check "only user events are offered as buttons"
    (not (elem (EventId "giveup") (userEvents spec)) && length (userEvents spec) == 5)

  pure [ a, b, c, d, e, f, g, h ]

-- =============================================================================
-- Config reshapes the machine
-- =============================================================================

-- | The mechanical test the format uses to sort config from parameter: *does the
-- | diagram change when you change it?* If this fails, the distinction is a
-- | judgement call again, and two runtimes have nothing to agree about.
configReshapesTheDiagram :: Effect (Array Boolean)
configReshapesTheDiagram = do
  section "Config reshapes the machine, and the drawing says so"

  radio <- loadOrDie "car-radio.json"
  let fitted = setFact (FactId "disc-loaded") (VBoolean true) (worldFrom radio)
  let stripped = setConfig (ConfigId "has-cd-slot") (VBoolean false) fitted

  a <- check "with a CD slot, something reaches the CD state"
    (hasInbound fitted radio "cd")
  b <- check "without one, the CD state is stranded"
    (not (hasInbound stripped radio "cd"))

  loop <- loadOrDie "loop.json"
  let withCountIn = worldFrom loop
  let noCountIn = setConfig (ConfigId "count-in-beats") (VNumber 0.0) withCountIn

  c <- check "with a count-in, record reaches Armed"
    (resolve loop withCountIn (StateId "empty") (EventId "record") == Move (StateId "armed"))
  d <- check "with none, record goes straight to Recording"
    (resolve loop noCountIn (StateId "empty") (EventId "record") == Move (StateId "recording"))
  e <- check "and Armed is then unreachable in the derived diagram"
    (not (hasInbound noCountIn loop "armed"))

  pure [ a, b, c, d, e ]
  where
  hasInbound world spec target =
    not (null (inboundTo (StateId target) (edgesOf defaultOptions world spec)))

-- =============================================================================
-- The round trip
-- =============================================================================

-- | Encoding and decoding must be inverses, and must preserve *behaviour* and
-- | not merely structure — so the check drives the whole (state x event) cross
-- | product through both copies and compares outcomes.
roundTrips :: Effect (Array Boolean)
roundTrips = do
  section "The round trip"
  for corpus \name -> do
    spec <- loadOrDie name
    case decodeSpec (encodeSpec spec) of
      Left err -> check (name <> " survives encode/decode") false <* log ("    " <> err)
      Right back -> do
        let world = worldFrom spec
        let same = all (\(Tuple s e) -> resolve spec world s e == resolve back world s e) (crossProduct spec)
        let sameShape = stateIds spec == stateIds back && eventIds spec == eventIds back
        check (name <> " round-trips, identically on every (state x event)") (same && sameShape)

-- | The Blocks syntax over the same five machines the JSON codec is held to.
dslCorpus :: Effect (Array Boolean)
dslCorpus = do
  section "DSL.Blocks over the corpus"
  groups <- for corpus \name -> do
    spec <- loadOrDie name
    roundTripsMachine name spec
  pure (join groups)

crossProduct :: Spec -> Array (Tuple StateId EventId)
crossProduct spec = do
  s <- stateIds spec
  e <- eventIds spec
  pure (Tuple s e)

-- =============================================================================
-- Behaviour parity with the PureScript machine this replaces
-- =============================================================================

-- | The table below is `Glassbox.Demo.Loop.transition` as it stood at commit
-- | b057e2b, before the machine became data. Keeping it here is what makes the
-- | rewrite a port rather than a rewrite that happens to compile.
loopParity :: Effect (Array Boolean)
loopParity = do
  section "Parity with the PureScript loop machine it replaces"
  spec <- loadOrDie "loop.json"
  let world = worldFrom spec
  let held = setFact (FactId "converter-held") (VBoolean true) world

  let
    expected =
      [ Tuple (Tuple "empty" "record") (Move (StateId "armed"))
      , Tuple (Tuple "empty" "play") (Refuse (RefusalId "nothing-yet"))
      , Tuple (Tuple "empty" "clear") Stay
      , Tuple (Tuple "empty" "stop") Stay
      , Tuple (Tuple "empty" "elapsed") Stay
      , Tuple (Tuple "armed" "elapsed") (Move (StateId "recording"))
      , Tuple (Tuple "armed" "record") (Move (StateId "empty"))
      , Tuple (Tuple "armed" "stop") (Move (StateId "empty"))
      , Tuple (Tuple "armed" "play") Stay
      , Tuple (Tuple "armed" "clear") (Move (StateId "empty"))
      , Tuple (Tuple "recording" "record") (Move (StateId "playing"))
      , Tuple (Tuple "recording" "play") (Move (StateId "playing"))
      , Tuple (Tuple "recording" "stop") (Move (StateId "stopped"))
      , Tuple (Tuple "recording" "clear") (Move (StateId "empty"))
      , Tuple (Tuple "recording" "elapsed") Stay
      , Tuple (Tuple "playing" "record") (Move (StateId "overdubbing"))
      , Tuple (Tuple "playing" "stop") (Move (StateId "stopped"))
      , Tuple (Tuple "playing" "clear") (Move (StateId "empty"))
      , Tuple (Tuple "playing" "play") Stay
      , Tuple (Tuple "playing" "elapsed") Stay
      , Tuple (Tuple "overdubbing" "record") (Move (StateId "playing"))
      , Tuple (Tuple "overdubbing" "play") (Move (StateId "playing"))
      , Tuple (Tuple "overdubbing" "stop") (Move (StateId "stopped"))
      , Tuple (Tuple "overdubbing" "clear") (Move (StateId "empty"))
      , Tuple (Tuple "overdubbing" "elapsed") Stay
      , Tuple (Tuple "stopped" "play") (Move (StateId "playing"))
      , Tuple (Tuple "stopped" "clear") (Move (StateId "empty"))
      , Tuple (Tuple "stopped" "record") (Refuse (RefusalId "overdub-in-play"))
      , Tuple (Tuple "stopped" "stop") Stay
      , Tuple (Tuple "stopped" "elapsed") Stay
      ]

  let
    wrong = filter
      (\(Tuple (Tuple s e) want) -> resolve spec world (StateId s) (EventId e) /= want)
      expected

  a <- check ("all " <> show (length expected) <> " (state, event) pairs match the old table")
    (null wrong)
  b <- check "the converter refusal still fires when another loop holds it"
    (resolve spec held (StateId "empty") (EventId "record") == Refuse (RefusalId "converter-busy"))
  c <- check "the count-in deadline is declared on Armed"
    (isJust (dueAt spec world (StateId "armed") 0.0))
  pure [ a, b, c ]

-- =============================================================================
-- A machine the format was not designed around
-- =============================================================================

-- | The looper and the car radio were both authored by whoever designed the
-- | format, so their fitting proves little. This one was written to push back.
-- |
-- | It is here mainly to pin the modelling decision it forced: floor NUMBER is
-- | host state reaching the machine as facts, not a state and not config, since
-- | changing floors does not change the diagram. If a later format change makes
-- | that unexpressible, these fail.
elevatorStressesTheFormat :: Effect (Array Boolean)
elevatorStressesTheFormat = do
  section "A machine the format was not designed around"
  spec <- loadOrDie "elevator.json"
  let idle = worldFrom spec
  let above = setFact (FactId "wanted-above") (VBoolean true) idle
  let blocked = setFact (FactId "doors-blocked") (VBoolean true) idle

  a <- check "with nothing requested, a call refuses rather than moving"
    (resolve spec idle (StateId "idle") (EventId "call") == Refuse (RefusalId "nowhere-to-go"))
  b <- check "with a floor requested above, it goes up"
    (resolve spec above (StateId "idle") (EventId "call") == Move (StateId "up"))
  c <- check "the doors refuse to close on an obstruction"
    (resolve spec blocked (StateId "doors-open") (EventId "close") == Refuse (RefusalId "blocked"))
  d <- check "and the dwell timeout does not close them either"
    (resolve spec blocked (StateId "doors-open") (EventId "dwelt") == Stay)
  e <- check "emergency stop is reachable from every moving state"
    (all (\s -> resolve spec idle (StateId s) (EventId "alarm") == Move (StateId "stopped"))
      [ "idle", "up", "down", "arriving", "doors-open", "held" ])
  f <- check "removing the hold button strands the held state"
    (resolve spec (setConfig (ConfigId "has-hold-button") (VBoolean false) idle)
      (StateId "doors-open") (EventId "open") == Refuse (RefusalId "no-hold-button"))
  pure [ a, b, c, d, e, f ]

-- =============================================================================
-- The rule five machines agree on
-- =============================================================================

-- | Three machines in a row forced the same move, so it is a rule and not an
-- | anecdote: **a machine here has no memory beyond the state it is in**, so
-- | every count, index, position and history lives in the host and arrives as a
-- | fact. These pin the three instances, because if a later format change makes
-- | any of them unexpressible the rule has quietly moved.
hostHoldsTheMemory :: Effect (Array Boolean)
hostHoldsTheMemory = do
  section "Counts, positions and history live in the host"

  wash <- loadOrDie "washing-machine.json"
  let base = worldFrom wash

  -- A count: the drum controller counts rinses; the machine only asks.
  a <- check "another rinse loops back; none goes on to spin"
    ( resolve wash (setFact (FactId "more-rinses") (VBoolean true) base)
        (StateId "draining") (EventId "step-done") == Move (StateId "rinse-filling")
      && resolve wash base (StateId "draining") (EventId "step-done") == Move (StateId "spinning")
    )

  -- A history: resume returns to where it was interrupted.
  b <- check "resume returns to the interrupted step, not a fixed one"
    ( resolve wash (setFact (FactId "paused-in-rinse") (VBoolean true) base)
        (StateId "paused") (EventId "resume") == Move (StateId "rinsing")
      && resolve wash (setFact (FactId "paused-in-fill") (VBoolean true) base)
        (StateId "paused") (EventId "resume") == Move (StateId "filling")
    )

  c <- check "the door refuses to open mid-cycle, with a reason"
    (resolve wash base (StateId "spinning") (EventId "open") == Refuse (RefusalId "door-locked"))

  d <- check "prewash is config: switching it off strands the prewash state"
    (not (reachesIn base wash "prewashing"))
  e <- check "and switching it on gives it a way in"
    (reachesIn (setConfig (ConfigId "has-prewash") (VBoolean true) base) wash "prewashing")

  heat <- loadOrDie "heating.json"
  let cold = worldFrom heat
  f <- check "no frost protection fitted: boost from off is refused"
    (resolve heat (setConfig (ConfigId "has-frost-watch") (VBoolean false) cold)
      (StateId "off") (EventId "boost") == Refuse (RefusalId "no-frost"))
  g <- check "a locked-out boiler refuses everything but reset"
    ( resolve heat cold (StateId "fault") (EventId "mode") == Refuse (RefusalId "in-fault")
      && resolve heat cold (StateId "fault") (EventId "reset") == Move (StateId "off")
    )
  h <- check "boost ends on its own after the configured interval"
    (dueAt heat cold (StateId "boosting") 0.0 == Just { fires: EventId "boost-over", at: 60.0 })

  pure [ a, b, c, d, e, f, g, h ]
  where
  reachesIn world spec target =
    not (null (inboundTo (StateId target) (edgesOf defaultOptions world spec)))

-- =============================================================================
-- The effects fork, over real artifacts
-- =============================================================================

-- | The claim the fork was chosen for, checked against all five machines.
-- |
-- | *Entering Recording starts the recorder, and that holds for every inbound
-- | edge including ones that do not exist yet.* Stated over the corpus rather
-- | than over a fixture, because the failure mode it prevents — one edge out of
-- | four forgetting its command — only appears in a machine with four edges.
-- |
-- | The second check is the more interesting one. It walks every (state, event)
-- | pair in every machine and insists that whenever two of them land in the
-- | same place, they ask the host for exactly the same thing. Under the rejected
-- | shape — commands hanging off the edge — this would be false by construction
-- | for any machine whose author slipped once, and nothing would have said so.
commandsAreSaidOnce :: Effect (Array Boolean)
commandsAreSaidOnce = do
  section "A command is written once on the state and holds for every way in"

  results <- for corpus \name -> do
    spec <- loadOrDie name
    let world = worldFrom spec

    let
      arrivals = do
        Tuple from event <- crossProduct spec
        case resolve spec world from event of
          Move to -> [ { from, event, to, commands: commandsFor spec from (Move to) } ]
          _ -> []

    -- Every arrival at a state ends with that state's entry commands, whichever
    -- edge it came along.
    let missing = filter (\a -> not (endsWith (entryOf spec a.to) a.commands)) arrivals

    a <- check (name <> ": all " <> show (length arrivals) <> " arrivals run the destination's entry commands")
      (null missing)

    -- And two events landing in the same place from the same place are
    -- indistinguishable to the host.
    let
      inconsistent = filter
        (\a -> any (\b -> b.from == a.from && b.to == a.to && b.commands /= a.commands) arrivals)
        arrivals
    b <- check (name <> ": two events with the same destination ask for the same thing")
      (null inconsistent)

    pure [ a, b ]

  -- The looper starts in Empty, which discards audio; four of the five machines
  -- owe the host something before a single event has been delivered.
  loop <- loadOrDie "loop.json"
  c <- check "starting the looper owes the host its initial state's entry commands"
    (startCommands loop == [ CommandId "discard-audio" ])

  -- The one that would be a silent bug: a refusal that nonetheless does
  -- something. Checked on the real machine, where a refusal leaves a state that
  -- genuinely has an exit command.
  radio <- loadOrDie "car-radio.json"
  let rworld = worldFrom radio
  d <- check "a refused eject in Seeking runs nothing, though leaving Seeking would"
    ( resolve radio rworld (StateId "seeking") (EventId "eject") == Refuse (RefusalId "no-disc")
        && commandsFor radio (StateId "seeking") (Refuse (RefusalId "no-disc")) == []
        && exitOf radio (StateId "seeking") == [ CommandId "stop-seek" ]
    )

  pure (join results <> [ c, d ])
  where
  endsWith suffix xs = Array.drop (length xs - length suffix) xs == suffix

-- =============================================================================
-- The boundary refuses what it does not understand
-- =============================================================================

boundaryIsChecked :: Effect (Array Boolean)
boundaryIsChecked = do
  section "The boundary is checked, not trusted"
  a <- check "a future format version is refused outright"
    (isLeft (parseSpec """{"glassbox":99,"id":"x","initial":"a","states":[{"id":"a"}]}"""))
  b <- check "a malformed case is refused"
    (isLeft (parseSpec """{"glassbox":1,"id":"x","initial":"a","states":[{"id":"a"}],"rules":[{"from":"a","on":"e","cases":[{}]}]}"""))
  c <- check "text that is not JSON is refused"
    (isLeft (parseSpec "not json at all"))
  pure [ a, b, c ]
  where
  isLeft = case _ of
    Left _ -> true
    Right _ -> false

-- =============================================================================

-- | Two suites, one tally.
-- |
-- | The **conformance corpus** comes first: the five machines in
-- | `core/machines/`, driven as artifacts, which is what the phase 1 acceptance
-- | criterion is stated over. The **library suite** follows: the same code
-- | examined one function at a time, plus properties over generated machines.
-- |
-- | They answer different questions and neither replaces the other. The corpus
-- | says a real artifact works; the library suite says which decision breaks
-- | when it stops working.
main :: Effect Unit
main = do
  log "Glassbox — phase 1"
  groups <- sequence
    [ carRadioNeedsNoPureScript
    , configReshapesTheDiagram
    , roundTrips
    , loopParity
    , elevatorStressesTheFormat
    , hostHoldsTheMemory
    , commandsAreSaidOnce
    , boundaryIsChecked
    , dslCorpus
    , dslUnits
    , runUnits
    , edgeUnits
    , codecUnits
    , commandUnits
    , exportUnits
    , lintUnits
    , properties
    ]
  let results = join groups
  let passed = length (filter identity results)
  let total = length results
  log ("\n" <> show passed <> "/" <> show total <> " passed")
  if passed == total then log "All tests passed!" else setExitCode 1
