-- | Test.Gen
-- |
-- | Generators for the property tests.
-- |
-- | These are `Gen` **values**, not `Arbitrary` instances, for the same reason
-- | `Glassbox.Codec.JSON` is a codec value rather than a class instance: there
-- | is no single canonical random `Spec`, and pretending there is would make
-- | every test that wants a differently-shaped machine fight the instance. A
-- | test that needs a machine with no config asks for one.
-- |
-- | Two deliberate choices about what gets generated.
-- |
-- | **Machines stay small** — two to five states, two to four events. The
-- | round-trip property drives the full (state × event) cross product under a
-- | generated world, so size is quadratic in the test's cost and roughly
-- | constant in its power: a bug that survives five states survives fifty.
-- |
-- | **The world is generated with holes in it.** A config is as likely to be
-- | absent as set, and a fact more so, because "a missing fact is not a false
-- | fact" is the runtime's most load-bearing claim and a generator that always
-- | supplies everything would never test it.
module Test.Gen
  ( Vocab
  , vocabOf
  , genValue
  , genCmpOp
  , genExpr
  , genGuard
  , genOutcome
  , genCase
  , genSpec
  , genWorldFor
  , genEdge
  , genEdges
  , pick
  ) where

import Prelude

import Data.Array (catMaybes, take)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Glassbox.Edges (Edge, EdgeKind(..))
import Glassbox.Run (World)
import Glassbox.Spec
  ( Case
  , CmpOp(..)
  , CommandId(..)
  , ConfigId(..)
  , EventId(..)
  , EventSource(..)
  , Expr(..)
  , FactId(..)
  , Guard(..)
  , Outcome(..)
  , RefusalId(..)
  , Spec
  , StateId(..)
  , Value(..)
  , versionFor
  )
import Test.QuickCheck.Gen (Gen, chooseInt, elements, oneOf, vectorOf)

-- =============================================================================
-- Small combinators
-- =============================================================================

-- | Choose from an array that may be empty, in which case the fallback stands
-- | in. The fallback is normally an identifier the machine does NOT declare,
-- | which is exactly the interesting case for the runtime's miss handling.
pick :: forall a. a -> Array a -> Gen a
pick fallback = maybe (pure fallback) elements <<< NEA.fromArray

anyOf :: forall a. Gen a -> Array (Gen a) -> Gen a
anyOf first rest = oneOf (NEA.cons' first rest)

-- =============================================================================
-- The vocabulary a generated guard may draw on
-- =============================================================================

-- | The identifiers in play, so that a generated guard mostly refers to things
-- | the machine declares and occasionally does not.
type Vocab =
  { states :: Array StateId
  , events :: Array EventId
  , configs :: Array ConfigId
  , facts :: Array FactId
  , refusals :: Array RefusalId
  }

vocabOf :: Spec -> Vocab
vocabOf spec =
  { states: map _.id spec.states
  , events: map _.id spec.events
  , configs: map _.id spec.config
  , facts: map _.id spec.facts
  , refusals: map _.id spec.refusals
  }

-- =============================================================================
-- Values, expressions, guards
-- =============================================================================

-- | Numbers are small integers, so that a round trip through JSON text is
-- | testing the codec rather than floating-point printing, and so that `NaN` —
-- | which JSON cannot carry at all — never appears.
genValue :: Gen Value
genValue = anyOf
  (VNumber <<< toNumber <$> chooseInt (-2) 3)
  [ VBoolean <$> pick true [ true, false ]
  , VString <$> pick "x" [ "x", "y", "fm", "" ]
  ]

genCmpOp :: Gen CmpOp
genCmpOp = pick OpEq [ OpEq, OpNe, OpLt, OpLe, OpGt, OpGe ]

genExpr :: Vocab -> Gen Expr
genExpr v = anyOf
  (Lit <$> genValue)
  [ ConfigOf <$> pick (ConfigId "undeclared-config") v.configs
  , FactOf <$> pick (FactId "undeclared-fact") v.facts
  ]

-- | `depth` bounds nesting. The operand count starts at zero so that `And []`
-- | and `Or []` — the identities, and the two most easily got wrong — turn up
-- | on their own rather than needing a special case.
genGuard :: Vocab -> Int -> Gen Guard
genGuard v depth =
  if depth <= 0 then leaf
  else anyOf leaf
    [ leaf
    , Not <$> genGuard v (depth - 1)
    , And <$> operands
    , Or <$> operands
    ]
  where
  leaf = anyOf
    (Holds <$> genExpr v)
    [ Cmp <$> genCmpOp <*> genExpr v <*> genExpr v ]

  operands = do
    k <- chooseInt 0 3
    vectorOf k (genGuard v (depth - 1))

-- =============================================================================
-- Outcomes, cases, rules
-- =============================================================================

genOutcome :: Vocab -> Gen Outcome
genOutcome v = anyOf
  (Move <$> pick (StateId "undeclared-state") v.states)
  [ Move <$> pick (StateId "undeclared-state") v.states
  , pure Stay
  , Refuse <$> pick (RefusalId "undeclared-refusal") v.refusals
  ]

genCase :: Vocab -> Gen Case
genCase v = do
  when' <- anyOf (pure Nothing) [ Just <$> genGuard v 2 ]
  outcome <- genOutcome v
  pure { when: when', outcome }

-- =============================================================================
-- Whole machines
-- =============================================================================

genSpec :: Gen Spec
genSpec = do
  nStates <- chooseInt 2 5
  nEvents <- chooseInt 2 4
  nConfig <- chooseInt 0 2
  nFacts <- chooseInt 0 2
  nRefusals <- chooseInt 0 2
  nCommands <- chooseInt 0 2
  -- Prose is part of the value now, so the round-trip property has to carry it.
  nNotes <- chooseInt 0 2
  let notes = take nNotes [ "a note", "another: with a colon" ]

  let stateIds = map StateId (take nStates [ "s0", "s1", "s2", "s3", "s4" ])
  let eventIds = map EventId (take nEvents [ "e0", "e1", "e2", "e3" ])
  let configIds = map ConfigId (take nConfig [ "c0", "c1" ])
  let factIds = map FactId (take nFacts [ "f0", "f1" ])
  let refusalIds = map RefusalId (take nRefusals [ "r0", "r1" ])
  -- `k` rather than `c`, which config already has. Distinct namespaces in the
  -- format, but a counterexample is read by a person.
  let commandIds = map CommandId (take nCommands [ "k0", "k1" ])

  let
    vocab =
      { states: stateIds
      , events: eventIds
      , configs: configIds
      , facts: factIds
      , refusals: refusalIds
      }

  states <- traverse (genStateDecl commandIds) stateIds
  events <- traverse genEventDecl eventIds
  config <- traverse genConfigDecl configIds
  rules <- catMaybes <$> traverse (genRule vocab) (crossProduct stateIds eventIds)
  deadlines <- genDeadlines vocab
  initial <- pick (StateId "s0") stateIds
  title <- pick "Generated" [ "Generated", "A title: with a colon", "" ]

  let
    spec =
      { version: 1
      , notes: notes
      , id: "generated"
      , title
      , initial
      , states
      , events
      , config
      , facts: map (\fid@(FactId raw) -> { id: fid, label: raw <> " is so" }) factIds
      , refusals: map (\rid@(RefusalId raw) -> { id: rid, text: "because " <> raw }) refusalIds
      , commands: map (\kid@(CommandId raw) -> { id: kid, label: "do " <> raw }) commandIds
      , rules
      , deadlines
      }

  -- Honest about itself. The structural round-trip property compares the whole
  -- value, and a decoder normalises the version upward to whatever the machine
  -- actually uses, so a generator that always said 1 would fail that property
  -- the moment it generated a command — for a reason that is the codec being
  -- right rather than wrong.
  pure (spec { version = versionFor spec })
  where
  crossProduct ss es = do
    s <- ss
    e <- es
    pure (Tuple s e)

  -- Commands are drawn from the machine's own declared list, so a generated
  -- artifact is closed over its command vocabulary the way a real one is. Both
  -- hooks are often empty: most states do nothing on the way in or out, and a
  -- generator that gave every state a command would never test the common case.
  genStateDecl commandIds sid@(StateId raw) = do
    final <- pick false [ false, false, false, true ]
    entry <- someOf commandIds
    exit <- someOf commandIds
    pure { id: sid, label: raw <> " state", final, entry, exit }

  someOf pool =
    if Array.null pool then pure []
    else do
      n <- chooseInt 0 2
      if n == 0 then pure [] else vectorOf n (pick (CommandId "k0") pool)

  genEventDecl eid@(EventId raw) = do
    source <- pick FromUser [ FromUser, FromUser, FromRuntime ]
    pure { id: eid, label: raw, source }

  genConfigDecl cid@(ConfigId raw) = do
    default <- genValue
    pure { id: cid, label: raw, default }

  -- Roughly a quarter of pairs get no rule at all, so `resolve`'s "no rule
  -- means Stay" path is exercised by every generated machine.
  genRule vocab (Tuple from on) = do
    present <- chooseInt 0 3
    if present == 0 then pure Nothing
    else do
      k <- chooseInt 0 3
      cases <- vectorOf k (genCase vocab)
      pure (Just { from, on, cases })

  genDeadlines vocab = do
    n <- chooseInt 0 2
    if n == 0 then pure []
    else do
      inState <- pick (StateId "s0") vocab.states
      fires <- pick (EventId "e0") vocab.events
      after <- anyOf
        (Lit <<< VNumber <<< toNumber <$> chooseInt 0 5)
        [ ConfigOf <$> pick (ConfigId "undeclared-config") vocab.configs ]
      pure [ { inState, fires, after } ]

-- | A world with holes in it: a declared config may be absent, at its default,
-- | or overridden; a fact is as often unreported as reported.
genWorldFor :: Spec -> Gen World
genWorldFor spec = do
  cs <- traverse configEntry spec.config
  fs <- traverse factEntry spec.facts
  pure { config: Map.fromFoldable (catMaybes cs), facts: Map.fromFoldable (catMaybes fs) }
  where
  configEntry c = do
    n <- chooseInt 0 3
    case n of
      0 -> pure Nothing
      1 -> pure (Just (Tuple c.id c.default))
      _ -> (Just <<< Tuple c.id) <$> genValue

  factEntry f = do
    n <- chooseInt 0 2
    if n == 0 then pure Nothing
    else (Just <<< Tuple f.id) <$> genValue

-- =============================================================================
-- Edges, for the merge properties
-- =============================================================================

-- | Drawn from a deliberately tiny pool so that parallel edges — the whole
-- | point of `merge` — actually collide.
genEdge :: Gen Edge
genEdge = do
  from <- pick (StateId "a") [ StateId "a", StateId "b", StateId "c" ]
  to <- pick (StateId "a") [ StateId "a", StateId "b", StateId "c" ]
  kind <- pick OnEvent [ OnEvent, OnDeadline, OnRefusal ]
  k <- chooseInt 1 3
  events <- vectorOf k (pick (EventId "x") [ EventId "x", EventId "y", EventId "z" ])
  reason <- anyOf (pure Nothing) [ Just <$> pick (RefusalId "p") [ RefusalId "p", RefusalId "q" ] ]
  pure { from, to, events, kind, reason }

genEdges :: Gen (Array Edge)
genEdges = do
  n <- chooseInt 0 8
  vectorOf n genEdge
