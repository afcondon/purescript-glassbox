-- | Glassbox.Conformance
-- |
-- | The claim, as data another runtime can be held to.
-- |
-- | The README states the claim as a property: *given the same sequence of
-- | input events, the artifact reaches the same state, emits the same effects
-- | in the same order, and refuses with the same tag* — and says it is a
-- | property rather than an intention because it is checkable: enumerate every
-- | (state × event × guard-assignment) the artifact admits and drive each
-- | implementation through all of them. This module is the enumeration, and
-- | the PureScript runtime's answer to each, written out so that a second
-- | implementation can replay the same table without importing anything here.
-- |
-- | ### What is enumerated
-- |
-- | Every (state, event) pair the declarations admit — not only the pairs with
-- | a rule, because a pair with no rule is an answer too (`Stay`, nothing) and
-- | a runtime that disagreed about it would be wrong.
-- |
-- | For each pair, every assignment of **the facts and config that pair's
-- | guards actually read**. `Edges.assignments` enumerates every declared fact
-- | for every pair, which is the right question for a drawing and the wrong
-- | size for a table: a fact no guard on this rule names cannot change what
-- | `resolve` answers, so a vector that varies it is the same vector twice.
-- | Enumerating per rule is exactly complete and stays small on a machine
-- | with a dozen facts.
-- |
-- | Facts are enumerated over `false` and `true`, since a fact is something the
-- | host reports and a guard tests. Config is enumerated over its **declared
-- | default plus every value a guard compares it against** (and the negation
-- | of a boolean), so a `count-in-beats > 0` case is exercised at 4 and at 0
-- | without anyone having to guess which numbers matter. Config the pair does
-- | not read stays at its default, and that convention is part of the file's
-- | contract: a replayer starts from `worldFrom` and applies what the vector
-- | says.
-- |
-- | ### Timers
-- |
-- | A deadline is two facts about a state: the alarm a host is told to set on
-- | entry, and what the delivered event does when it arrives. The second is
-- | already in the table, because the event a deadline fires is an ordinary
-- | event with an ordinary rule. The first is written out separately, as the
-- | `alarms` section: for every state with a deadline, under every assignment
-- | its `after` reads, the alarm `alarmFor` computes. Whether the host fires it
-- | on time is outside the claim, exactly as the README says timing is.
-- |
-- | ### Why the canonical form rides along
-- |
-- | `Glassbox.Canonical` argues that "the same machine, two runtimes" needs
-- | both runtimes to compute the same canonical text, and until now nothing
-- | measured that outside the Go probe. Each suite carries the text this
-- | runtime produced, so the other one can produce its own and compare.
-- |
-- | Everything here is a pure function of a `Spec`; writing the file is the
-- | CLI's job, for the reason every printer in this package leaves IO out.
module Glassbox.Conformance
  ( Suite
  , Vector
  , AlarmVector
  , Assignment
  , suiteVersion
  , suiteFor
  , worldsFor
  , candidatesFor
  , encodeSuite
  , printSuite
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Array (concatMap, elem, filter, find, foldl, mapMaybe, nub, unsnoc)
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Glassbox.Canonical (canonicalBytes)
import Glassbox.Run (Alarm, World, alarmFor, setConfig, setFact, startCommands, step, worldFrom)
import Glassbox.Spec
  ( Clock
  , CommandId(..)
  , ConfigId(..)
  , EventId(..)
  , Expr(..)
  , FactId(..)
  , Guard(..)
  , Outcome(..)
  , RefusalId(..)
  , Spec
  , StateId(..)
  , Value(..)
  , clockName
  , deadlineFor
  , eventIds
  , ruleFor
  , stateIds
  )

-- | The version of the suite file, not of the format. Bump it when the shape
-- | below changes, so a replayer built against the old shape refuses the new
-- | file instead of misreading it.
suiteVersion :: Int
suiteVersion = 1

-- | The part of a world a vector spells out: the config and facts the pair
-- | reads, in declaration order. Everything else is at its default (config)
-- | or unreported (facts).
type Assignment =
  { config :: Array (Tuple ConfigId Value)
  , facts :: Array (Tuple FactId Value)
  }

-- | One line of the table: where it was, what arrived, what the world said,
-- | and what the runtime answered.
type Vector =
  { from :: StateId
  , event :: EventId
  , assignment :: Assignment
  , outcome :: Outcome
  , current :: StateId
  , commands :: Array CommandId
  }

-- | What a host is told to arm on entering a state, under an assignment.
type AlarmVector =
  { state :: StateId
  , assignment :: Assignment
  , alarm :: Maybe Alarm
  }

type Suite =
  { machine :: String
  , version :: Int
  , canonical :: String
  , start :: Array CommandId
  , vectors :: Array Vector
  , alarms :: Array AlarmVector
  }

-- =============================================================================
-- Enumeration
-- =============================================================================

-- | The whole table for one machine.
suiteFor :: Spec -> Suite
suiteFor spec =
  { machine: spec.id
  , version: spec.version
  , canonical: canonicalBytes spec
  , start: startCommands spec
  , vectors: do
      from <- stateIds spec
      event <- eventIds spec
      let refs = maybe noRefs (\r -> foldMap caseRefs r.cases) (ruleFor spec from event)
      Tuple assignment world <- worldsFor spec refs
      let Tuple current record = step spec world from event
      pure { from, event, assignment, outcome: record.outcome, current, commands: record.commands }
  , alarms: do
      state <- stateIds spec
      case deadlineFor spec state of
        Nothing -> []
        Just d -> do
          Tuple assignment world <- worldsFor spec (exprRefs d.after)
          pure { state, assignment, alarm: alarmFor spec world state }
  }
  where
  caseRefs c = maybe noRefs guardRefs c.when

-- | Every world the named config and facts could put the machine in, each
-- | paired with the assignment that produced it.
-- |
-- | Starts from `worldFrom`, so unnamed config is at its default and unnamed
-- | facts are absent. Declaration order is kept and the last key varies
-- | fastest, so the table is stable across runs and reads in a predictable
-- | order.
worldsFor :: Spec -> Refs -> Array (Tuple Assignment World)
worldsFor spec refs =
  foldl spreadFact (foldl spreadConfig [ Tuple emptyAssignment (worldFrom spec) ] configs) facts
  where
  configs = filter (_ `elem` refs.config) (map _.id spec.config)
  facts = filter (_ `elem` refs.facts) (map _.id spec.facts)

  spreadConfig acc cid = do
    Tuple a w <- acc
    v <- candidatesFor spec cid
    pure (Tuple (a { config = a.config <> [ Tuple cid v ] }) (setConfig cid v w))

  spreadFact acc fid = do
    Tuple a w <- acc
    v <- [ VBoolean false, VBoolean true ]
    pure (Tuple (a { facts = a.facts <> [ Tuple fid v ] }) (setFact fid v w))

  emptyAssignment = { config: [], facts: [] }

-- | The values a config setting is worth trying: its default, the negation of
-- | a boolean, and every literal any guard or deadline in the machine compares
-- | it against. Nothing is guessed — a number that appears in no comparison
-- | cannot change a branch.
candidatesFor :: Spec -> ConfigId -> Array Value
candidatesFor spec cid = nub ([ decl ] <> flipped <> compared)
  where
  -- A config nobody declared has no default to try; the vocabulary lint is
  -- what rejects the guard that named it.
  decl = maybe (VBoolean false) _.default (find (\c -> c.id == cid) spec.config)

  flipped = case decl of
    VBoolean b -> [ VBoolean (not b) ]
    _ -> []

  compared = concatMap literalsIn (allGuards spec)

  literalsIn = case _ of
    Holds _ -> []
    Not g -> literalsIn g
    And gs -> concatMap literalsIn gs
    Or gs -> concatMap literalsIn gs
    Cmp _ (ConfigOf c) (Lit v) | c == cid -> [ v ]
    Cmp _ (Lit v) (ConfigOf c) | c == cid -> [ v ]
    Cmp _ _ _ -> []

allGuards :: Spec -> Array Guard
allGuards spec = mapMaybe _.when (concatMap _.cases spec.rules)

-- | What a guard reads. The same walk `Lint.vocabulary` does, kept here rather
-- | than exported from there because the lint's version carries a sum type it
-- | has no reason to publish.
type Refs = { config :: Array ConfigId, facts :: Array FactId }

noRefs :: Refs
noRefs = { config: [], facts: [] }

guardRefs :: Guard -> Refs
guardRefs = case _ of
  Holds e -> exprRefs e
  Not g -> guardRefs g
  And gs -> foldMap guardRefs gs
  Or gs -> foldMap guardRefs gs
  Cmp _ a b -> exprRefs a <> exprRefs b

exprRefs :: Expr -> Refs
exprRefs = case _ of
  Lit _ -> noRefs
  ConfigOf c -> { config: [ c ], facts: [] }
  FactOf f -> { config: [], facts: [ f ] }

-- =============================================================================
-- Encoding
-- =============================================================================

-- | The suite as one JSON value. Key order is what the printer below relies on
-- | for a readable file; nothing else depends on it.
encodeSuite :: Suite -> Json
encodeSuite suite = obj (header suite <> [ Tuple "vectors" (J.fromArray (map encodeVector suite.vectors)), Tuple "alarms" (J.fromArray (map encodeAlarm suite.alarms)) ])

header :: Suite -> Array (Tuple String Json)
header suite =
  [ Tuple "glassbox-vectors" (J.fromNumber (toNumber suiteVersion))
  , Tuple "machine" (J.fromString suite.machine)
  , Tuple "glassbox" (J.fromNumber (toNumber suite.version))
  , Tuple "canonical" (J.fromString suite.canonical)
  , Tuple "start" (J.fromArray (map (\(CommandId c) -> J.fromString c) suite.start))
  ]

-- | One vector per line, so the file diffs the way a table should: a changed
-- | rule is a changed line, not a reflowed block.
printSuite :: Suite -> String
printSuite suite =
  joinWith "\n"
    ( [ "{" ]
        <> map (\(Tuple k v) -> "  " <> J.stringify (J.fromString k) <> ": " <> J.stringify v <> ",") (header suite)
        <> section "vectors" (map encodeVector suite.vectors) ","
        <> section "alarms" (map encodeAlarm suite.alarms) ""
        <> [ "}", "" ]
    )
  where
  section name items trailing = case items of
    [] -> [ "  \"" <> name <> "\": []" <> trailing ]
    _ ->
      [ "  \"" <> name <> "\": [" ]
        <> mapLast (\line -> line <> ",") (map (\j -> "    " <> J.stringify j) items)
        <> [ "  ]" <> trailing ]

  -- Every item but the last takes a comma.
  mapLast f xs = case unsnoc xs of
    Nothing -> []
    Just { init, last } -> map f init <> [ last ]

encodeVector :: Vector -> Json
encodeVector v = obj
  ( [ Tuple "from" (stateJson v.from)
    , Tuple "event" (eventJson v.event)
    ]
      <> assignmentFields v.assignment
      <> outcomeFields v.outcome
      <> [ Tuple "current" (stateJson v.current)
         , Tuple "commands" (J.fromArray (map (\(CommandId c) -> J.fromString c) v.commands))
         ]
  )

encodeAlarm :: AlarmVector -> Json
encodeAlarm a = obj
  ( [ Tuple "state" (stateJson a.state) ]
      <> assignmentFields a.assignment
      <> [ Tuple "alarm" (maybe J.jsonNull alarmJson a.alarm) ]
  )

alarmJson :: Alarm -> Json
alarmJson a = obj
  [ Tuple "fires" (eventJson a.fires)
  , Tuple "after" (J.fromNumber a.after)
  , Tuple "clock" (maybe J.jsonNull clockJson a.clock)
  ]

assignmentFields :: Assignment -> Array (Tuple String Json)
assignmentFields a =
  [ Tuple "config" (obj (map (\(Tuple (ConfigId k) v) -> Tuple k (valueJson v)) a.config))
  , Tuple "facts" (obj (map (\(Tuple (FactId k) v) -> Tuple k (valueJson v)) a.facts))
  ]

outcomeFields :: Outcome -> Array (Tuple String Json)
outcomeFields = case _ of
  Move _ -> [ Tuple "outcome" (J.fromString "move") ]
  Stay -> [ Tuple "outcome" (J.fromString "stay") ]
  Refuse (RefusalId r) -> [ Tuple "outcome" (J.fromString "refuse"), Tuple "refusal" (J.fromString r) ]

valueJson :: Value -> Json
valueJson = case _ of
  VNumber n -> J.fromNumber n
  VBoolean b -> J.fromBoolean b
  VString s -> J.fromString s

stateJson :: StateId -> Json
stateJson (StateId s) = J.fromString s

eventJson :: EventId -> Json
eventJson (EventId e) = J.fromString e

clockJson :: Clock -> Json
clockJson = J.fromString <<< clockName

obj :: Array (Tuple String Json) -> Json
obj = J.fromObject <<< Object.fromFoldable
