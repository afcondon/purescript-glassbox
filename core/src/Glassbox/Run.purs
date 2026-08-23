-- | Glassbox.Run
-- |
-- | Running a decoded `Spec`.
-- |
-- | The interpreter is a **pure function of (world, state, event)** and nothing
-- | else. It holds no state of its own, so a host stores the current `StateId` —
-- | a plain, comparable, serialisable value — rather than an opaque machine
-- | object. The previous design handed the host a `Mealy`, which meant the
-- | current state lived inside a closure and the same phase was stored twice:
-- | once where it could be read and once where it could not. That cost session
-- | restore, time-travel, and the ability to paste a reproducing trace into a
-- | bug report, and bought nothing.
-- |
-- | Since the effects fork the interpreter also answers **what the host must
-- | do** — `commandsFor`, and the `commands` field of a `Step`. That is still a
-- | pure function of the same three things; carrying out the commands is the
-- | host's business, and nothing here performs an effect.
module Glassbox.Run
  ( World
  , worldFrom
  , setConfig
  , setFact
  , lookupConfig
  , lookupFact
  , evalExpr
  , evalGuard
  , resolve
  , commandsFor
  , startCommands
  , step
  , Step
  , initialStep
  , dueAt
  ) where

import Prelude

import Data.Array (find, foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Glassbox.Spec
  ( CommandId
  , ConfigId
  , EventId
  , Expr(..)
  , FactId
  , Guard(..)
  , CmpOp(..)
  , Outcome(..)
  , Spec
  , StateId
  , Value(..)
  , deadlineFor
  , entryOf
  , exitOf
  , outcomeTarget
  , ruleFor
  , truthy
  )

-- | The world a guard may read: what the artifact declares, and what the host
-- | reports.
-- |
-- | Both arrive per step rather than being captured once, because both genuinely
-- | change between events — a setting is turned, another loop takes the
-- | converter — and because a captured world is a closure by another name.
type World =
  { config :: Map ConfigId Value
  , facts :: Map FactId Value
  }

-- | The world implied by a spec alone: every config at its declared default and
-- | no facts. What a host starts from before it knows anything.
worldFrom :: Spec -> World
worldFrom spec =
  { config: foldl (\m c -> Map.insert c.id c.default m) Map.empty spec.config
  , facts: Map.empty
  }

setConfig :: ConfigId -> Value -> World -> World
setConfig k v world = world { config = Map.insert k v world.config }

setFact :: FactId -> Value -> World -> World
setFact k v world = world { facts = Map.insert k v world.facts }

lookupConfig :: World -> ConfigId -> Maybe Value
lookupConfig world k = Map.lookup k world.config

lookupFact :: World -> FactId -> Maybe Value
lookupFact world k = Map.lookup k world.facts

-- =============================================================================
-- Evaluation
-- =============================================================================

-- | An expression that names something the world does not have evaluates to
-- | `Nothing` rather than to a default. A missing fact is not a false fact, and
-- | quietly reading it as one is how a machine takes a branch nobody authored.
-- | The vocabulary lint is what stops such an artifact loading at all.
evalExpr :: World -> Expr -> Maybe Value
evalExpr world = case _ of
  Lit v -> Just v
  ConfigOf k -> lookupConfig world k
  FactOf k -> lookupFact world k

evalGuard :: World -> Guard -> Boolean
evalGuard world = case _ of
  Holds e -> maybe false truthy (evalExpr world e)
  Not g -> not (evalGuard world g)
  And gs -> foldl (\acc g -> acc && evalGuard world g) true gs
  Or gs -> foldl (\acc g -> acc || evalGuard world g) false gs
  Cmp op l r -> case evalExpr world l, evalExpr world r of
    Just a, Just b -> compareValues op a b
    _, _ -> false

-- | Equality is structural; ordering requires both sides to be the same kind of
-- | value. Ordering a number against a string is a mistake in the artifact, and
-- | it reads as false rather than falling back on constructor order, which would
-- | be a well-defined answer to a question nobody asked.
compareValues :: CmpOp -> Value -> Value -> Boolean
compareValues op a b = case op of
  OpEq -> a == b
  OpNe -> a /= b
  _ -> case ordering of
    Nothing -> false
    Just ord -> case op of
      OpLt -> ord == LT
      OpLe -> ord /= GT
      OpGt -> ord == GT
      OpGe -> ord /= LT
      _ -> false
  where
  ordering = case a, b of
    VNumber x, VNumber y -> Just (compare x y)
    VString x, VString y -> Just (compare x y)
    VBoolean x, VBoolean y -> Just (compare x y)
    _, _ -> Nothing

-- =============================================================================
-- Stepping
-- =============================================================================

-- | What a state does with an event, under a given world.
-- |
-- | Cases are tried in order and the first whose guard holds wins; a case with
-- | no guard is the catch-all. A state and event with no rule at all yields
-- | `Stay`, which is the same answer a flat machine gives for "not mine" — the
-- | totality lint is what ensures a real artifact never relies on it.
resolve :: Spec -> World -> StateId -> EventId -> Outcome
resolve spec world from on = case ruleFor spec from on of
  Nothing -> Stay
  Just rule -> case find matches rule.cases of
    Nothing -> Stay
    Just c -> c.outcome
  where
  matches c = case c.when of
    Nothing -> true
    Just g -> evalGuard world g

-- | What the host is asked to do, given where the machine was and what the
-- | outcome was.
-- |
-- | Three clauses, and each of them is a decision rather than a consequence.
-- |
-- |   * **A refusal commands nothing.** A refusal means nothing happened, and a
-- |     refused press that nonetheless does something is precisely the bug the
-- |     `Stay`/`Refuse` distinction was introduced to make visible. The tag
-- |     already reaches the display; that is the whole of its effect.
-- |   * **`Stay` commands nothing.** This is a statechart *internal*
-- |     transition: the machine did not leave, so nothing was exited and
-- |     nothing entered.
-- |   * **`Move` exits then enters**, and it does so **even when the target is
-- |     the state it is already in**. That is the *external* self-transition,
-- |     and re-entering Recording genuinely restarts the recorder.
-- |
-- | The last two together are worth pausing on. Statecharts need an explicit
-- | internal-versus-external annotation on a self-transition to say whether
-- | re-entry re-runs the entry action. This format already draws that
-- | distinction, in a field it has for entirely different reasons: `Stay` is
-- | internal and `Move current` is external. Nothing had to be added.
commandsFor :: Spec -> StateId -> Outcome -> Array CommandId
commandsFor spec from = case _ of
  Refuse _ -> []
  Stay -> []
  Move to -> exitOf spec from <> entryOf spec to

-- | What the host must do before the machine has been fed anything at all.
-- |
-- | The initial state is entered, so its entry commands are owed. A host that
-- | skips this starts a machine whose Recording state is current and whose
-- | recorder was never started, and every subsequent transition is correct
-- | about a world that is already wrong.
startCommands :: Spec -> Array CommandId
startCommands spec = entryOf spec spec.initial

-- | Where it was, what happened, where it now is, and what the host owes.
-- |
-- | `outcome` is carried out rather than collapsed so that a refusal reaches the
-- | display instead of vanishing into an unchanged state.
type Step =
  { from :: StateId
  , event :: EventId
  , outcome :: Outcome
  , current :: StateId
  , commands :: Array CommandId
  }

-- | One step. Pure, total, and the only thing a host needs to run a machine.
step :: Spec -> World -> StateId -> EventId -> Tuple StateId Step
step spec world from event =
  let
    outcome = resolve spec world from event
    current = outcomeTarget from outcome
    commands = commandsFor spec from outcome
  in
    Tuple current { from, event, outcome, current, commands }

-- | The step a machine is notionally in before anything has been fed to it.
initialStep :: Spec -> EventId -> Step
initialStep spec event =
  { from: spec.initial
  , event
  , outcome: Stay
  , current: spec.initial
  -- NOT `startCommands`. This is the step a machine is notionally in before
  -- anything happened, and nothing has yet happened; entering the initial
  -- state is a separate event in the host's life and `startCommands` is what
  -- it asks for.
  , commands: []
  }

-- | When the deadline for a state falls due, relative to when it was entered.
-- |
-- | `Nothing` when the state resolves only on events, or when the deadline's
-- | quantity does not evaluate to a number under this world.
dueAt :: Spec -> World -> StateId -> Number -> Maybe { fires :: EventId, at :: Number }
dueAt spec world sid enteredAt = case deadlineFor spec sid of
  Nothing -> Nothing
  Just d -> case evalExpr world d.after of
    Just (VNumber n) -> Just { fires: d.fires, at: enteredAt + n }
    _ -> Nothing
