-- | Test.Property
-- |
-- | The properties, run through QuickCheck and reported into the same tally as
-- | everything else — one property, one `ok` line, one verdict for the run.
-- |
-- | The load-bearing one is `the round trip preserves behaviour`. Every other
-- | part of this library is a function that can be tested by example; the codec
-- | is the only part whose contract is *universally quantified over machines*,
-- | because the whole thesis is that a machine nobody here has seen — authored
-- | by a designer, in a graphical editor, next year — survives the boundary
-- | intact. Five hand-picked artifacts cannot say that. A generator can say it
-- | for a few hundred machines in a second, and it is the same claim a second
-- | implementation in another language will have to satisfy.
-- |
-- | The comparison is deliberately **behavioural, not structural**. Two specs
-- | that differ in a field nothing reads are the same machine; two that agree
-- | structurally but resolve one pair differently are not. So each round-trip
-- | property drives the full (state × event) cross product under a generated
-- | world and compares outcomes, and compares the derived edge set as well,
-- | because the edge set is what a drawing shows.
module Test.Property
  ( properties
  ) where

import Prelude

import Data.Array (filter, length, nub, null, sort)
import Data.Either (Either(..))
import Data.Foldable (any, elem)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Glassbox.Codec.JSON (decodeSpec, encodeSpec, parseSpec, printSpec)
import Glassbox.Edges (Edge, EdgeKind(..), assignments, defaultOptions, edgesOf, merge)
import Glassbox.Run (World, commandsFor, evalGuard, lookupConfig, resolve, worldFrom)
import Glassbox.Spec
  ( EventId(..)
  , EventSource(..)
  , Guard(..)
  , Outcome(..)
  , Spec
  , StateId(..)
  , deadlineFor
  , eventIds
  , isRefusal
  , ruleFor
  , stateIds
  )
import Test.Gen (Vocab, genEdges, genGuard, genSpec, genWorldFor, pick, vocabOf)
import Test.Harness (prop, propWith, section)
import Test.QuickCheck (Result(..))
import Test.QuickCheck.Gen (Gen, chooseInt, vectorOf)

-- =============================================================================
-- Helpers
-- =============================================================================

-- | Every (state, event) the machine admits, which is the domain the guarantee
-- | is stated over.
crossProduct :: Spec -> Array (Tuple StateId EventId)
crossProduct spec = do
  s <- stateIds spec
  e <- eventIds spec
  pure (Tuple s e)

-- | The identity `merge` groups by, rendered as a comparable string.
tripleKeys :: Array Edge -> Array String
tripleKeys = sort <<< nub <<< map key
  where
  key e = raw e.from <> " -> " <> raw e.to <> " / " <> kindName e.kind
  raw (StateId s) = s
  kindName = case _ of
    OnEvent -> "event"
    OnDeadline -> "deadline"
    OnRefusal -> "refusal"

holds :: String -> Boolean -> Result
holds why ok = if ok then Success else Failed why

-- | A counterexample is only useful if it can be looked at, and a `Spec` has no
-- | `Show`. Its own wire format is the obvious rendering, and the one that can
-- | be pasted into a file and re-run.
withSpec :: Spec -> String -> Result
withSpec spec why = Failed (why <> "\n      " <> printSpec spec)

vocab :: Spec -> Vocab
vocab = vocabOf

-- =============================================================================
-- The round trip
-- =============================================================================

-- | **The one that matters.** For a generated machine and a generated world,
-- | the decoded re-encoding must answer every (state, event) exactly as the
-- | original did, and derive the same edge set.
roundTripBehaves :: Gen Result
roundTripBehaves = do
  spec <- genSpec
  world <- genWorldFor spec
  pure case decodeSpec (encodeSpec spec) of
    Left err -> withSpec spec ("re-encoding did not decode: " <> err)
    Right back ->
      let
        diverged = filter (\(Tuple s e) -> resolve spec world s e /= resolve back world s e) (crossProduct spec)
      in
        if length diverged > 0 then
          withSpec spec ("diverged on " <> show (length diverged) <> " (state, event) pairs")
        else if edgesOf defaultOptions world spec /= edgesOf defaultOptions world back then
          withSpec spec "the derived edge set differs"
        else if stateIds spec /= stateIds back || eventIds spec /= eventIds back then
          withSpec spec "the declared state or event list differs"
        else Success

-- | The same, through the text an artifact is actually stored as. Separate from
-- | the above because `stringify` is a second lossy step: a value that survives
-- | as `Json` can still be mangled on the way to a file.
roundTripThroughText :: Gen Result
roundTripThroughText = do
  spec <- genSpec
  world <- genWorldFor spec
  pure case parseSpec (printSpec spec) of
    Left err -> withSpec spec ("printed artifact did not parse: " <> err)
    Right back ->
      let
        diverged = filter (\(Tuple s e) -> resolve spec world s e /= resolve back world s e) (crossProduct spec)
      in
        if length diverged > 0 then withSpec spec "text round trip changed behaviour"
        else if back /= spec then withSpec spec "text round trip changed the value"
        else Success

-- | A property suite is only as good as its generator, and a generator that
-- | quietly stopped producing guards would leave every law above passing
-- | vacuously. This samples a population and insists the awkward parts of the
-- | format all turn up in it.
generatorCoversTheFormat :: Gen Result
generatorCoversTheFormat = do
  specs <- vectorOf 40 genSpec
  let reaches what f = holds ("the generator produced no machine with " <> what) (any f specs)
  pure case filter isFailure (checks reaches) of
    [] -> Success
    bad -> case bad of
      [ f ] -> f
      _ -> Failed (show (length bad) <> " parts of the format were never generated")
  where
  isFailure = case _ of
    Success -> false
    Failed _ -> true

  checks reaches =
    [ reaches "a guard" (any (\r -> any (isJust <<< _.when) r.cases) <<< _.rules)
    , reaches "a catch-all case" (any (\r -> any (isNothing <<< _.when) r.cases) <<< _.rules)
    , reaches "a refusal" (any (\r -> any (isRefusal <<< _.outcome) r.cases) <<< _.rules)
    , reaches "a deadline" (not <<< null <<< _.deadlines)
    , reaches "config" (not <<< null <<< _.config)
    , reaches "a fact" (not <<< null <<< _.facts)
    , reaches "a final state" (any _.final <<< _.states)
    , reaches "a runtime-sourced event" (any (\e -> e.source /= userSource) <<< _.events)
    , reaches "a (state, event) pair with no rule" pairWithNoRule
    , reaches "a rule with no cases" (any (null <<< _.cases) <<< _.rules)
    , reaches "a declared command" (not <<< null <<< _.commands)
    , reaches "a state that does something on entry" (any (not <<< null <<< _.entry) <<< _.states)
    , reaches "a state that does something on exit" (any (not <<< null <<< _.exit) <<< _.states)
    ]

  userSource = FromUser

  pairWithNoRule spec = any (\(Tuple s e) -> isNothing (ruleFor spec s e)) (crossProduct spec)

-- =============================================================================
-- merge
-- =============================================================================

mergeIsIdempotent :: Gen Result
mergeIsIdempotent = do
  edges <- genEdges
  let once = merge edges
  pure (holds "merge (merge es) /= merge es" (merge once == once))

mergeKeepsTriples :: Gen Result
mergeKeepsTriples = do
  edges <- genEdges
  pure
    ( holds "merge changed the set of (from, to, kind) triples"
        (tripleKeys (merge edges) == tripleKeys edges)
    )

mergeIsOnePerTriple :: Gen Result
mergeIsOnePerTriple = do
  edges <- genEdges
  pure
    ( holds "merge left more arrows than there are distinct triples"
        (length (merge edges) == length (tripleKeys edges))
    )

mergeCanonicalisesEvents :: Gen Result
mergeCanonicalisesEvents = do
  edges <- genEdges
  let bad = filter (\e -> e.events /= nub (sort e.events)) (merge edges)
  pure (holds "a merged arrow carried unsorted or duplicated events" (length bad == 0))

-- | No event may be invented and none may be lost: the events on the merged
-- | arrows are exactly the events on the arrows they replaced.
mergeKeepsEvents :: Gen Result
mergeKeepsEvents = do
  edges <- genEdges
  let before = sort (nub (edges >>= _.events))
  let after = sort (nub (merge edges >>= _.events))
  pure (holds "merge invented or dropped an event" (before == after))

-- =============================================================================
-- resolve, and the edge set derived from it
-- =============================================================================

-- | `resolve` is total: every (state, event) gets an answer, including pairs
-- | the machine has never heard of. The runtime has no failure mode of its own,
-- | which is what lets a host store a bare `StateId` and feed it anything.
resolveIsTotal :: Gen Result
resolveIsTotal = do
  spec <- genSpec
  world <- genWorldFor spec
  strangers <- genStrangePairs spec
  let answered = map (\(Tuple s e) -> answers (resolve spec world s e)) (crossProduct spec <> strangers)
  pure (holds "resolve failed to answer some pair" (length (filter identity answered) == length answered))
  where
  answers = case _ of
    Move _ -> true
    Stay -> true
    Refuse _ -> true

  genStrangePairs spec = do
    n <- chooseInt 1 4
    vectorOf n do
      s <- pick (StateId "ghost") ([ StateId "ghost", StateId "" ] <> stateIds spec)
      e <- pick (EventId "phantom") ([ EventId "phantom", EventId "" ] <> eventIds spec)
      pure (Tuple s e)

-- | A machine with no rules stays where it is, whatever it is fed. Not the same
-- | statement as totality: that one is about answering, this one about the
-- | answer, and it is the one the "not mine" default rests on.
noRulesMeansStay :: Gen Result
noRulesMeansStay = do
  spec <- genSpec
  world <- genWorldFor spec
  let bare = spec { rules = [], deadlines = [] }
  let wrong = filter (\(Tuple s e) -> resolve bare world s e /= Stay) (crossProduct bare)
  pure (holds "a machine with no rules did something" (length wrong == 0))

-- | The edge set is *derived by asking the machine*, so it must agree with the
-- | machine. This is the property `Glassbox.Edges` exists to protect — that
-- | there is no second description of the transitions that could drift from the
-- | first — stated as a check rather than as a comment.
edgesAgreeWithResolve :: Gen Result
edgesAgreeWithResolve = do
  spec <- genSpec
  world <- genWorldFor spec
  -- Under ONE world, which is what this correspondence is about. The union
  -- over unreported facts is a different statement and is the property below.
  let edges = edgesOf (defaultOptions { factsUnknown = false }) world spec
  let wrong = filter (disagrees spec world edges) (crossProduct spec)
  pure
    ( if length wrong == 0 then Success
      else withSpec spec ("the edge set disagreed with resolve on " <> show (length wrong) <> " pairs")
    )
  where
  disagrees spec world edges (Tuple s e)
    -- An event a deadline delivers is drawn as a deadline edge, never as one a
    -- person could cause, so it is outside this correspondence by design.
    | map _.fires (deadlineFor spec s) == Just e = false
    | otherwise = case resolve spec world s e of
        Move t -> not (any (\g -> g.from == s && g.to == t && g.kind == OnEvent && elem e g.events) edges)
        Refuse _ -> not (any (\g -> g.from == s && g.to == s && g.kind == OnRefusal && elem e g.events) edges)
        Stay -> any (\g -> g.from == s && g.kind /= OnDeadline && elem e g.events) edges

-- | With facts unknown, the edge set is the union over every way they could be
-- | reported — and it has to be exactly that union, in both directions.
-- |
-- | The direction that matters is completeness, and it is the one a real machine
-- | caught: Bosun's supervisor guards `raised -restart-> restarting` on a fact,
-- | and under a bare world the guarded case lost to the refusal beneath it, so
-- | the transition was simply absent from the drawing. A picture that omits an
-- | arrow is worse than no picture, because the reason to draw one is that it is
-- | a second opinion.
edgesUnionEveryAssignment :: Gen Result
edgesUnionEveryAssignment = do
  spec <- genSpec
  world <- genWorldFor spec
  let
    edges = edgesOf defaultOptions world spec
    worlds = assignments world (map _.id spec.facts)

    -- Completeness: a pair that moves under SOME assignment is drawn.
    -- Same carve-out as the single-world property: an event a deadline delivers
    -- is drawn as a deadline edge, never as one a person could cause.
    byDeadline s e = map _.fires (deadlineFor spec s) == Just e

    missed = filter
      (\(Tuple s e) -> not (byDeadline s e) && movesSomewhere s e && not (drawn s e))
      (crossProduct spec)

    movesSomewhere s e = any (\w -> isMove (resolve spec w s e)) worlds

    isMove = case _ of
      Move _ -> true
      _ -> false

    drawn s e = any (\g -> g.from == s && g.kind == OnEvent && elem e g.events) edges

    -- Soundness: every arrow drawn is one the machine really does produce
    -- under some assignment. Nothing invented.
    invented = filter (not <<< real) (filter (\g -> g.kind == OnEvent) edges)

    real g = any (\e -> any (\w -> resolve spec w g.from e == Move g.to) worlds) g.events

  pure
    ( if length missed == 0 && length invented == 0 then Success
      else withSpec spec
        ( "with facts unknown the edge set missed " <> show (length missed)
            <> " pairs and invented " <> show (length invented) <> " arrows"
        )
    )

-- | Hiding refusals removes refusal arrows and touches nothing else.
hidingRefusalsRemovesOnlyRefusals :: Gen Result
hidingRefusalsRemovesOnlyRefusals = do
  spec <- genSpec
  world <- genWorldFor spec
  let shown = edgesOf defaultOptions world spec
  let hidden = edgesOf (defaultOptions { showRefusals = false }) world spec
  pure
    ( holds "hiding refusals changed something other than the refusals"
        ( filter (\e -> e.kind /= OnRefusal) shown == hidden
            && filter (\e -> e.kind == OnRefusal) hidden == []
        )
    )

-- =============================================================================
-- Commands
-- =============================================================================

-- | A refused or ignored event never asks the host to do anything.
-- |
-- | Stated over generated machines rather than by example because it is the
-- | rule most likely to be broken by accident later: the tempting place to hang
-- | an effect is wherever the code already branches, and two of those three
-- | branches must stay silent.
nothingHappensWithoutAMove :: Gen Result
nothingHappensWithoutAMove = do
  spec <- genSpec
  world <- genWorldFor spec
  let
    noisy = filter
      ( \(Tuple s e) -> case resolve spec world s e of
          Move _ -> false
          _ -> not (null (commandsFor spec s (resolve spec world s e)))
      )
      (crossProduct spec)
  pure (holds "a Stay or a Refuse emitted a command" (null noisy))

-- | What the host is asked to do depends on where the machine was and where it
-- | went — and on nothing else. Two different events landing the same way must
-- | ask for exactly the same thing, which is the property that makes "say it
-- | once on the state" equivalent to "say it on every inbound edge".
commandsDependOnlyOnThePair :: Gen Result
commandsDependOnlyOnThePair = do
  spec <- genSpec
  world <- genWorldFor spec
  let
    disagreeing = filter
      ( \(Tuple (Tuple s e1) e2) ->
          resolve spec world s e1 == resolve spec world s e2
            && commandsFor spec s (resolve spec world s e1)
            /= commandsFor spec s (resolve spec world s e2)
      )
      (pairsOfEvents spec)
  pure (holds "two events with the same outcome asked for different commands" (null disagreeing))
  where
  pairsOfEvents spec = do
    s <- stateIds spec
    e1 <- eventIds spec
    e2 <- eventIds spec
    pure (Tuple (Tuple s e1) e2)

-- =============================================================================
-- evalGuard
-- =============================================================================

-- | `Not (Not g)` is `g`. Trivial for a boolean, not trivial here: `Holds` can
-- | fail to produce a boolean at all, and the module collapses that to false
-- | rather than to a third value — which is precisely what makes double
-- | negation hold. A three-valued guard would break this line.
doubleNegation :: Gen Result
doubleNegation = do
  Tuple world guard <- genWorldAndGuard
  pure (holds "Not (Not g) disagreed with g" (evalGuard world (Not (Not guard)) == evalGuard world guard))

deMorganOverAnd :: Gen Result
deMorganOverAnd = do
  Tuple world guards <- genWorldAndGuards
  pure
    ( holds "not (all gs) /= any (not gs)"
        (evalGuard world (Not (And guards)) == evalGuard world (Or (map Not guards)))
    )

deMorganOverOr :: Gen Result
deMorganOverOr = do
  Tuple world guards <- genWorldAndGuards
  pure
    ( holds "not (any gs) /= all (not gs)"
        (evalGuard world (Not (Or guards)) == evalGuard world (And (map Not guards)))
    )

-- | The empty and singleton cases the two laws above pass through are the ones
-- | most easily got wrong, so they are asserted directly rather than left to
-- | the generator's luck.
connectiveUnits :: Gen Result
connectiveUnits = do
  Tuple world guard <- genWorldAndGuard
  pure
    ( holds "the empty connectives are not the identities"
        ( evalGuard world (And [])
            && not (evalGuard world (Or []))
            && evalGuard world (And [ guard ]) == evalGuard world guard
            && evalGuard world (Or [ guard ]) == evalGuard world guard
        )
    )

genWorldAndGuard :: Gen (Tuple World Guard)
genWorldAndGuard = do
  spec <- genSpec
  world <- genWorldFor spec
  guard <- genGuard (vocab spec) 3
  pure (Tuple world guard)

genWorldAndGuards :: Gen (Tuple World (Array Guard))
genWorldAndGuards = do
  spec <- genSpec
  world <- genWorldFor spec
  n <- chooseInt 0 3
  guards <- vectorOf n (genGuard (vocab spec) 2)
  pure (Tuple world guards)

-- =============================================================================
-- worldFrom
-- =============================================================================

-- | The world a host starts from: every declared config at its declared
-- | default, and no facts. Both halves matter — a `worldFrom` that invented a
-- | fact would be inventing a claim about the world.
defaultWorldIsTheDeclaredDefaults :: Gen Result
defaultWorldIsTheDeclaredDefaults = do
  spec <- genSpec
  let w = worldFrom spec
  let missing = filter (\c -> lookupConfig w c.id /= Just c.default) spec.config
  pure
    ( holds "worldFrom did not seed the declared defaults, or invented a fact"
        (length missing == 0 && Map.isEmpty w.facts)
    )

-- =============================================================================

properties :: Effect (Array Boolean)
properties = do
  section "Properties, over generated machines"

  -- The cross-product ones do the most work per case; the rest are cheap.
  a <- propWith 300 "the round trip preserves behaviour on every (state x event)" roundTripBehaves
  b <- propWith 300 "and so does the round trip through the artifact's own text" roundTripThroughText
  c <- propWith 25 "the generator reaches every awkward corner of the format" generatorCoversTheFormat

  d <- prop "merge is idempotent" mergeIsIdempotent
  e <- prop "merge preserves the set of (from, to, kind) triples" mergeKeepsTriples
  f <- prop "merge leaves exactly one arrow per triple" mergeIsOnePerTriple
  g <- prop "merged events are sorted and deduplicated" mergeCanonicalisesEvents
  h <- prop "merge neither invents nor drops an event" mergeKeepsEvents

  i <- prop "resolve is total, including on pairs the machine never heard of" resolveIsTotal
  j <- prop "a machine with no rules stays put whatever it is fed" noRulesMeansStay
  k <- prop "the derived edge set agrees with resolve, pair for pair" edgesAgreeWithResolve
  k2 <- prop "with facts unknown the edge set is exactly the union over them" edgesUnionEveryAssignment
  l <- prop "hiding refusals removes only refusals" hidingRefusalsRemovesOnlyRefusals

  m <- prop "Not (Not g) is g" doubleNegation
  n <- prop "De Morgan: not (all gs) is any (not gs)" deMorganOverAnd
  o <- prop "De Morgan: not (any gs) is all (not gs)" deMorganOverOr
  p <- prop "And [] is true, Or [] is false, and both are units" connectiveUnits

  q <- prop "worldFrom is the declared defaults and no facts" defaultWorldIsTheDeclaredDefaults

  r <- prop "only a Move ever asks the host to do anything" nothingHappensWithoutAMove
  s <- prop "the commands depend on the (state, outcome) pair and nothing else" commandsDependOnlyOnThePair

  pure [ a, b, c, d, e, f, g, h, i, j, k, k2, l, m, n, o, p, q, r, s ]
