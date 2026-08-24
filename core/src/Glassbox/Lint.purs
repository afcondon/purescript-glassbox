-- | Glassbox.Lint
-- |
-- | The checks that replace the compiler at the boundary.
-- |
-- | A machine is data loaded at runtime, so the PureScript compiler can no
-- | longer prove it handles every (state, event) pair. That was a real benefit
-- | and giving it up is only defensible if something else does the job at the
-- | boundary — which is the whole content of the FFI analogy the design
-- | document leans on. This module is that something.
-- |
-- | **Correctness lints gate the boot.** An artifact failing one is rejected
-- | with the same seriousness as a type error: a machine with a hole in it must
-- | never reach a user, and "we'll find out when someone presses the button" is
-- | precisely the standard being lowered too far.
-- |
-- | **Design lints advise and never gate.** Nothing here is one; they belong
-- | with the drawing, where distance from tree-ness and return asymmetry can be
-- | seen as well as counted.
-- |
-- | ### One correction to the list this was written from
-- |
-- | `the-same-machine.md` names five gating lints, of which the third is
-- | *determinism* — "no (state, event, guard-assignment) admits two outcomes".
-- | That check has nothing to do here, because determinism is not a property
-- | this format can lose: `Case`s are tried in order and the first match wins,
-- | so a `Spec` is deterministic **by construction**. Asking for it back as a
-- | lint would be asking whether the artifact is the kind of thing it is.
-- |
-- | What ordering costs instead is that a case can be hidden behind an earlier
-- | one and never fire. That is not a soundness problem — the machine still
-- | does exactly one thing — but it is a lie in the artifact, because a reader
-- | and the runtime will disagree about what the machine says. So `overlap`
-- | takes determinism's place, and warns rather than rejects.
module Glassbox.Lint
  ( Severity(..)
  , Finding
  , lint
  , rejections
  , warnings
  , accepts
  , explain
  ) where

import Prelude

import Data.Array
  ( catMaybes
  , cons
  , drop
  , elem
  , filter
  , findIndex
  , length
  , mapWithIndex
  , nub
  , null
  , uncons
  )
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), isNothing)
import Data.String (joinWith)
import Glassbox.Spec
  ( CommandId(..)
  , ConfigId(..)
  , EventId(..)
  , Expr(..)
  , FactId(..)
  , Guard(..)
  , Outcome(..)
  , RefusalId(..)
  , Spec
  , StateId(..)
  , eventIds
  , ruleFor
  , stateIds
  )

-- | Whether a finding stops the machine loading.
-- |
-- | Only two levels, and the line between them is doing real work: `Reject`
-- | means the artifact is not well-formed and no amount of care at the host
-- | can make it safe. `Warn` means it is well-formed and probably not what
-- | somebody meant.
data Severity = Reject | Warn

derive instance eqSeverity :: Eq Severity

instance showSeverity :: Show Severity where
  show = case _ of
    Reject -> "reject"
    Warn -> "warn"

-- | Every finding names the lint that produced it and points at the offending
-- | thing, because a check that says only "invalid" has replaced a type error
-- | with a shrug.
type Finding =
  { lint :: String
  , severity :: Severity
  , message :: String
  }

lint :: Spec -> Array Finding
lint spec =
  closure spec
    <> totality spec
    <> vocabulary spec
    <> overlap spec
    <> reachability spec

rejections :: Array Finding -> Array Finding
rejections = filter (\f -> f.severity == Reject)

warnings :: Array Finding -> Array Finding
warnings = filter (\f -> f.severity == Warn)

-- | Would this artifact be allowed to load?
accepts :: Spec -> Boolean
accepts = null <<< rejections <<< lint

explain :: Array Finding -> String
explain fs = joinWith "\n" (map line fs)
  where
  line f = "  " <> show f.severity <> "  " <> f.lint <> ": " <> f.message

-- ---------------------------------------------------------------------------
-- closure — every name a rule uses is a name the machine declares
-- ---------------------------------------------------------------------------
--
-- First, because everything below reads better against a machine whose names
-- resolve. A dangling target is the one failure that makes the rest of the
-- report noise.

closure :: Spec -> Array Finding
closure spec = catMaybes (cons initialOk (ruleFindings <> deadlineFindings))
  where
  states = stateIds spec
  events = eventIds spec
  refusals = map _.id spec.refusals

  initialOk
    | spec.initial `elem` states = Nothing
    | otherwise = Just (reject "closure" ("the initial state " <> quoteState spec.initial <> " is not declared"))

  ruleFindings = spec.rules >>= \r ->
    [ guardMissing (r.from `elem` states)
        ("a rule is written from " <> quoteState r.from <> ", which is not a declared state")
    , guardMissing (r.on `elem` events)
        ("a rule from " <> quoteState r.from <> " fires on " <> quoteEvent r.on <> ", which is not a declared event")
    ] <> (r.cases >>= \c -> caseFindings r c)

  caseFindings r c = case c.outcome of
    Move target ->
      [ guardMissing (target `elem` states)
          (context r <> " moves to " <> quoteState target <> ", which is not a declared state")
      ]
    Refuse tag ->
      [ guardMissing (tag `elem` refusals)
          (context r <> " refuses with " <> quoteRefusal tag <> ", which is not a declared refusal")
      ]
    Stay -> []

  deadlineFindings = spec.deadlines >>= \d ->
    [ guardMissing (d.inState `elem` states)
        ("a deadline is set in " <> quoteState d.inState <> ", which is not a declared state")
    , guardMissing (d.fires `elem` events)
        ("the deadline in " <> quoteState d.inState <> " fires " <> quoteEvent d.fires <> ", which is not a declared event")
    ]

  guardMissing ok message = if ok then Nothing else Just (reject "closure" message)
  context r = quoteState r.from <> " on " <> quoteEvent r.on

-- ---------------------------------------------------------------------------
-- totality — every (state × event) pair has a declared outcome
-- ---------------------------------------------------------------------------
--
-- This is the one the exhaustive `case` used to do. `Run.step` has a fallback
-- for a pair with no rule; the point of this lint is that no real artifact ever
-- reaches it.

totality :: Spec -> Array Finding
totality spec = catMaybes do
  s <- stateIds spec
  e <- eventIds spec
  pure case ruleFor spec s e of
    Just _ -> Nothing
    Nothing -> Just $ reject "totality"
      (quoteState s <> " has no declared outcome for " <> quoteEvent e)

-- ---------------------------------------------------------------------------
-- vocabulary — every guard, command and deadline names something declared
-- ---------------------------------------------------------------------------
--
-- The runtime publishes a closed list; this is where an artifact is held to
-- it. A machine asking a host for a command nobody implements is the failure
-- this exists to catch, and it is the one a host cannot defend itself against.

vocabulary :: Spec -> Array Finding
vocabulary spec = catMaybes (commandFindings <> guardFindings <> deadlineFindings)
  where
  configs = map _.id spec.config
  facts = map _.id spec.facts
  commands = map _.id spec.commands

  commandFindings = spec.states >>= \s ->
    map (checkCommand s "entry") s.entry <> map (checkCommand s "exit") s.exit

  checkCommand s which c
    | c `elem` commands = Nothing
    | otherwise = Just $ reject "vocabulary"
        ("the " <> which <> " of " <> quoteState s.id <> " names the command "
          <> quoteCommand c <> ", which is not declared")

  guardFindings = spec.rules >>= \r ->
    r.cases >>= \c -> case c.when of
      Nothing -> []
      Just g -> map (checkRef ("a guard on " <> quoteState r.from <> " " <> quoteEvent r.on)) (refsOfGuard g)

  deadlineFindings = spec.deadlines >>= \d ->
    map (checkRef ("the deadline in " <> quoteState d.inState)) (refsOfExpr d.after)

  checkRef where' = case _ of
    Left' cid
      | cid `elem` configs -> Nothing
      | otherwise -> Just $ reject "vocabulary"
          (where' <> " reads the config " <> quoteConfig cid <> ", which is not declared")
    Right' fid
      | fid `elem` facts -> Nothing
      | otherwise -> Just $ reject "vocabulary"
          (where' <> " reads the fact " <> quoteFact fid <> ", which is not declared")

-- | A reference out of a guard: to config, or to a fact.
data Ref = Left' ConfigId | Right' FactId

refsOfGuard :: Guard -> Array Ref
refsOfGuard = case _ of
  Holds e -> refsOfExpr e
  Not g -> refsOfGuard g
  And gs -> foldMap refsOfGuard gs
  Or gs -> foldMap refsOfGuard gs
  Cmp _ a b -> refsOfExpr a <> refsOfExpr b

refsOfExpr :: Expr -> Array Ref
refsOfExpr = case _ of
  Lit _ -> []
  ConfigOf c -> [ Left' c ]
  FactOf f -> [ Right' f ]

-- ---------------------------------------------------------------------------
-- overlap — a case that can never fire, and a rule that can never be found
-- ---------------------------------------------------------------------------
--
-- Determinism's replacement, and a warning rather than a rejection because
-- neither of these makes the machine do two things. They make it do one thing
-- while appearing to offer two, which is a documentation failure with teeth.

overlap :: Spec -> Array Finding
overlap spec = deadCases <> duplicateRules
  where
  deadCases = spec.rules >>= \r ->
    case findIndex (\c -> isNothing c.when) r.cases of
      Nothing -> []
      Just i ->
        let after = drop (i + 1) r.cases
        in if null after then []
           else
             [ warn "overlap"
                 ( quoteState r.from <> " on " <> quoteEvent r.on <> " has "
                     <> show (length after)
                     <> " case(s) after an unguarded one; they can never fire"
                 )
             ]

  duplicateRules =
    let keys = map (\r -> ruleKey r) spec.rules
    in catMaybes $ mapWithIndex
      ( \i k ->
          if findIndex (_ == k) keys == Just i then Nothing
          else Just $ warn "overlap"
            ("more than one rule is written for " <> k <> "; only the first is ever found")
      )
      keys

  ruleKey r = quoteState r.from <> " on " <> quoteEvent r.on

-- ---------------------------------------------------------------------------
-- reachability — every declared state can be got to
-- ---------------------------------------------------------------------------
--
-- A warning, deliberately. The demo machine has a legitimate case: a count-in
-- of zero strands `Armed` under that configuration and not others, so a state
-- unreachable *today* is not necessarily a mistake. Guards are ignored here —
-- a state reachable under some assignment counts as reachable.

reachability :: Spec -> Array Finding
reachability spec = map unreachable (filter (\s -> not (s `elem` seen)) (stateIds spec))
  where
  seen = walk [ spec.initial ] [ spec.initial ]

  walk acc frontier = case uncons frontier of
    Nothing -> acc
    Just { head, tail } ->
      let
        next = filter (\s -> not (s `elem` acc)) (nub (targetsFrom head))
      in
        walk (acc <> next) (tail <> next)

  targetsFrom s = catMaybes do
    r <- filter (\r' -> r'.from == s) spec.rules
    c <- r.cases
    pure case c.outcome of
      Move target -> Just target
      _ -> Nothing

  unreachable s = warn "reachability"
    (quoteState s <> " cannot be reached from " <> quoteState spec.initial)

-- ---------------------------------------------------------------------------

reject :: String -> String -> Finding
reject l m = { lint: l, severity: Reject, message: m }

warn :: String -> String -> Finding
warn l m = { lint: l, severity: Warn, message: m }

quoteState :: StateId -> String
quoteState (StateId s) = "`" <> s <> "`"

quoteEvent :: EventId -> String
quoteEvent (EventId e) = "`" <> e <> "`"

quoteRefusal :: RefusalId -> String
quoteRefusal (RefusalId r) = "`" <> r <> "`"

quoteCommand :: CommandId -> String
quoteCommand (CommandId c) = "`" <> c <> "`"

quoteConfig :: ConfigId -> String
quoteConfig (ConfigId c) = "`" <> c <> "`"

quoteFact :: FactId -> String
quoteFact (FactId f) = "`" <> f <> "`"
