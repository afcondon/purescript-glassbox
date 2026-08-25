-- | Glassbox.Canonical
-- |
-- | One machine, one string, the same on every runtime.
-- |
-- | ### Why this is not `printSpec`
-- |
-- | `Glassbox.Codec.JSON` already turns a `Spec` back into text, and it is the
-- | wrong text for this job in a way the Glassbox-on-Gnomon probe found
-- | empirically: object key order **differs between backends**. Go's
-- | `encoding/json` sorts keys where `JSON.stringify` preserves insertion
-- | order, so 314 of 315 sections of that probe agreed and the odd one out was
-- | `printSpecPretty`. Hash that, and a pinned Go binary rejects the very
-- | artifact the node one accepts — the safety mechanism firing on the two
-- | halves of the same program.
-- |
-- | So this touches no backend encoder at all. Every field is written in a
-- | fixed order by hand, every collection is explicitly sorted here, and the
-- | result is therefore identical on every runtime by construction rather than
-- | by agreement between two JSON libraries. It also makes the machine hash
-- | independent of the order-preserving `Foreign.Object` that phase 6 wants
-- | upstream, which is the point: the pin should not wait on the backend.
-- |
-- | ### What it is for
-- |
-- | Two things, and they are the same act. A **pinned host** carries the hash
-- | of the one machine it was built and tested against, and refuses anything
-- | else at load — the same shape as `Host.reportClock` refusing a clock it
-- | cannot honour, and for the same reason: find out at load, by refusal,
-- | rather than at the first transition that goes the wrong way. And
-- | **conformance** gains its missing leg, because "the same machine, two
-- | runtimes, same behaviour" currently establishes its first clause by
-- | inspection; have both runtimes compute this and compare.
-- |
-- | The artifact must never carry its own expected hash. That is circular, and
-- | pinning is a deployment posture rather than a feature of the format: an
-- | unpinned playground loads anything well-formed, a production host loads
-- | one thing. Pinning by default would be codegen with a slower loader.
-- |
-- | ### What it deliberately ignores
-- |
-- | It hashes the decoded `Spec`, not the file. So whitespace, key order, and
-- | anything the codec drops cannot move the hash, and a reformatted but
-- | behaviourally identical machine is still the same machine — which is the
-- | difference between a check people keep and one they switch off.
-- |
-- | Declaration order is ignored for the same reason: listing states in a
-- | different order changes what a table looks like and changes nothing the
-- | machine does, because `initial` names the entry point explicitly rather
-- | than taking the first.
-- |
-- | ### What it must NOT ignore, and why the sort is stable
-- |
-- | Three orders in this format carry meaning and are preserved exactly:
-- |
-- |   * `cases`, because `resolve` takes `find matches rule.cases` — a rule is
-- |     first-match-wins, and sorting it would silently rewrite the machine;
-- |   * `entry` and `exit`, because commands are carried out in the order
-- |     given;
-- |   * `notes`, because it is prose.
-- |
-- | `rules` and `deadlines` ARE sorted, by the key each is looked up under, and
-- | the sort is stable (`Data.Array.sortBy` is a merge sort) for a subtle
-- | reason: `ruleFor` is `find` and `deadlineFor` is `head <<< filter`, so where
-- | an artifact carries two entries under one key the first still wins. A stable
-- | sort keeps that. An unstable one could swap them and change behaviour while
-- | claiming to be canonical.
module Glassbox.Canonical
  ( canonicalBytes
  , canonicalVersion
  ) where

import Prelude

import Data.Array (length, sortBy)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..))
import Data.Number (isFinite, isNaN)
import Data.Number.Format (toString)
import Data.String.Common (joinWith, replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Glassbox.Spec
  ( Case
  , Clock
  , CmpOp(..)
  , CommandDecl
  , CommandId(..)
  , ConfigDecl
  , ConfigId(..)
  , Deadline
  , EventDecl
  , EventId(..)
  , EventSource(..)
  , Expr(..)
  , FactDecl
  , FactId(..)
  , Guard(..)
  , Outcome(..)
  , RefusalDecl
  , RefusalId(..)
  , Rule
  , Spec
  , StateDecl
  , StateId(..)
  , Value(..)
  , clockName
  )

-- | The version of this encoding, not of the format.
-- |
-- | Written into the first line so that a stored hash says which scheme
-- | produced it. Changing how anything below is written must bump this, because
-- | every pinned host in existence would otherwise start refusing a machine
-- | that had not changed.
canonicalVersion :: Int
canonicalVersion = 1

-- | The canonical text of a machine.
canonicalBytes :: Spec -> String
canonicalBytes spec = joinWith "\n" (lines <> [ "" ])
  where
  lines =
    [ "glassbox-canonical " <> show canonicalVersion
    , "version " <> show spec.version
    , "id " <> quoted spec.id
    , "title " <> quoted spec.title
    , "clock " <> maybeStr clockName spec.clock
    , "initial " <> quoted (unStateId spec.initial)
    ]
      <> block "notes" (map (\n -> "note " <> quoted n) spec.notes)
      <> block "states" (map state (sortBy (comparing _.id) spec.states))
      <> block "events" (map event (sortBy (comparing _.id) spec.events))
      <> block "config" (map config (sortBy (comparing _.id) spec.config))
      <> block "facts" (map fact (sortBy (comparing _.id) spec.facts))
      <> block "refusals" (map refusal (sortBy (comparing _.id) spec.refusals))
      <> block "commands" (map command (sortBy (comparing _.id) spec.commands))
      <> block "rules" (map rule (sortBy byRuleKey spec.rules))
      <> block "deadlines" (map deadline (sortBy (comparing _.inState) spec.deadlines))

  byRuleKey a b = compare a.from b.from <> compare a.on b.on

-- | A counted section. The count is not redundant: it makes a truncated
-- | canonical form detectable rather than merely shorter.
block :: String -> Array String -> Array String
block name items = [ name <> " " <> show (length items) ] <> map ("  " <> _) items

state :: StateDecl -> String
state s = joinWith " "
  [ "state"
  , quoted (unStateId s.id)
  , quoted s.label
  , bool s.final
  , list "entry" (map (quoted <<< unCommandId) s.entry)
  , list "exit" (map (quoted <<< unCommandId) s.exit)
  ]

event :: EventDecl -> String
event e = joinWith " "
  [ "event", quoted (unEventId e.id), quoted e.label, source e.source ]

config :: ConfigDecl -> String
config c = joinWith " "
  [ "config", quoted (unConfigId c.id), quoted c.label, value c.default ]

fact :: FactDecl -> String
fact f = joinWith " " [ "fact", quoted (unFactId f.id), quoted f.label ]

refusal :: RefusalDecl -> String
refusal r = joinWith " " [ "refusal", quoted (unRefusalId r.id), quoted r.text ]

command :: CommandDecl -> String
command c = joinWith " " [ "command", quoted (unCommandId c.id), quoted c.label ]

-- | `cases` keeps its order. See the module header — this one is behaviour.
rule :: Rule -> String
rule r = joinWith " "
  [ "rule"
  , quoted (unStateId r.from)
  , quoted (unEventId r.on)
  , list "cases" (map kase r.cases)
  ]

kase :: Case -> String
kase c = paren [ "case", maybeStr guard c.when, outcome c.outcome ]

deadline :: Deadline -> String
deadline d = joinWith " "
  [ "deadline", quoted (unStateId d.inState), quoted (unEventId d.fires), expr d.after ]

outcome :: Outcome -> String
outcome = case _ of
  Move to -> paren [ "move", quoted (unStateId to) ]
  Stay -> paren [ "stay" ]
  Refuse rid -> paren [ "refuse", quoted (unRefusalId rid) ]

guard :: Guard -> String
guard = case _ of
  Holds e -> paren [ "holds", expr e ]
  Not g -> paren [ "not", guard g ]
  And gs -> paren ([ "and", show (length gs) ] <> map guard gs)
  Or gs -> paren ([ "or", show (length gs) ] <> map guard gs)
  Cmp op a b -> paren [ "cmp", cmpOp op, expr a, expr b ]

expr :: Expr -> String
expr = case _ of
  Lit v -> paren [ "lit", value v ]
  ConfigOf cid -> paren [ "config", quoted (unConfigId cid) ]
  FactOf fid -> paren [ "fact", quoted (unFactId fid) ]

cmpOp :: CmpOp -> String
cmpOp = case _ of
  OpEq -> "eq"
  OpNe -> "ne"
  OpLt -> "lt"
  OpLe -> "le"
  OpGt -> "gt"
  OpGe -> "ge"

source :: EventSource -> String
source = case _ of
  FromUser -> "user"
  FromRuntime -> "runtime"

value :: Value -> String
value = case _ of
  VNumber n -> paren [ "num", number n ]
  VBoolean b -> paren [ "bool", bool b ]
  VString s -> paren [ "str", quoted s ]

-- | The one place a runtime could disagree, handled rather than hoped about.
-- |
-- | An integral number is written as exact digits via `Int`, which every
-- | backend renders identically — and the corpus is entirely integers and
-- | booleans, so in practice this is the whole story. `toString` is reached
-- | only by a genuinely fractional default, and it is the sole construct here
-- | whose output rests on the backend's own float formatting rather than on
-- | this module. The Go conformance run is what checks that, and it is the
-- | reason `num` is tagged: a fractional default that ever disagreed would show
-- | up as a differing line rather than as a differing hash with no cause.
-- |
-- | NaN and the infinities are named rather than formatted, because `toString`
-- | is free to spell them differently and because a number that is not equal to
-- | itself has no business deciding whether two machines are the same one.
number :: Number -> String
number n
  | isNaN n = "nan"
  | not (isFinite n) = if n > 0.0 then "inf" else "-inf"
  | otherwise = case fromNumber n of
      Just i -> show i
      Nothing -> toString n

bool :: Boolean -> String
bool b = if b then "true" else "false"

-- | A counted, parenthesised list, so nesting needs no delimiter discipline.
list :: String -> Array String -> String
list name items = paren ([ name, show (length items) ] <> items)

paren :: Array String -> String
paren parts = "(" <> joinWith " " parts <> ")"

maybeStr :: forall a. (a -> String) -> Maybe a -> String
maybeStr f = case _ of
  Nothing -> "-"
  Just a -> f a

-- | The ids are newtypes over `String` and there is no unwrapping helper in
-- | `Spec`; every module that needs the raw text imports the constructor and
-- | takes it apart, so these follow that.
unStateId :: StateId -> String
unStateId = case _ of
  StateId s -> s

unEventId :: EventId -> String
unEventId = case _ of
  EventId s -> s

unConfigId :: ConfigId -> String
unConfigId = case _ of
  ConfigId s -> s

unFactId :: FactId -> String
unFactId = case _ of
  FactId s -> s

unRefusalId :: RefusalId -> String
unRefusalId = case _ of
  RefusalId s -> s

unCommandId :: CommandId -> String
unCommandId = case _ of
  CommandId s -> s

-- | Length-free escaping: nothing here counts characters, because what counts
-- | as one character is exactly the sort of thing two runtimes disagree about.
-- | The backslash must be escaped first or it would escape the escapes.
quoted :: String -> String
quoted s = "\"" <> escaped s <> "\""
  where
  escaped =
    replaceAll (Pattern "\\") (Replacement "\\\\")
      >>> replaceAll (Pattern "\"") (Replacement "\\\"")
      >>> replaceAll (Pattern "\n") (Replacement "\\n")
      >>> replaceAll (Pattern "\r") (Replacement "\\r")
      >>> replaceAll (Pattern "\t") (Replacement "\\t")
