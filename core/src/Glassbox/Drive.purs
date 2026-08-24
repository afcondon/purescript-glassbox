-- | Glassbox.Drive
-- |
-- | Running a machine inside a program, in **any monad**.
-- |
-- | This started life inside a Halogen component, and almost none of it was
-- | about Halogen. What it actually needs is: somewhere to keep a `StateId`,
-- | somewhere to read the world from, a way to carry out a command, a way to
-- | report a refusal, a way to not block on a command, and a way to set a
-- | timer. Six questions, and a host is the only thing that can answer any of
-- | them — so they are fields rather than constraints, and what is left is
-- | `Monad m` and nothing else.
-- |
-- | That matters more than tidiness. A `MonadState` constraint would have
-- | pulled in `transformers` and quietly excluded a host keeping its phase in
-- | a `Ref`, in a database row, or in an Erlang process dictionary — and the
-- | whole point of the artifact is that the *same* machine runs in places that
-- | do not resemble each other. A driver that only runs under one effect system
-- | would concede the argument at the last step.
-- |
-- | What is deliberately NOT here: anything that decides. `fire` chooses
-- | nothing, guards nothing and has no opinion about optimistic updates — that
-- | is a question about how a state renders, not how it is run. Every decision
-- | is in the artifact, which is the claim the whole project rests on.
module Glassbox.Drive
  ( Wiring
  , fire
  , enter
  , start
  ) where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Glassbox.Run (Alarm, World, alarmFor, step)
import Glassbox.Spec (CommandId, EventId, Outcome(..), RefusalId, Spec, StateId, entryOf)

-- | What a host supplies to run a machine.
-- |
-- | `phase` and `move` say where the current state is kept — and it is kept in
-- | the **host's** own storage as a plain, comparable, serialisable `StateId`,
-- | not inside a closure here. That is what makes session restore, time travel
-- | and pasting a reproducing trace into a bug report possible at all.
-- |
-- | `perform` returns **an event, not a result**. The machine asks for
-- | `send-follow` and stops; the host feeds `ok` or `failed` back in as an
-- | ordinary event with ordinary arms. Completion is an event, not a
-- | continuation — which is what keeps the failure arm in the enumeration where
-- | a lint can see it, instead of in a callback where it can be forgotten.
-- |
-- | `refused` is required rather than optional, deliberately: a refusal that
-- | reaches nobody is the failure the `Stay`/`Refuse` distinction exists to
-- | expose.
-- |
-- | `fork` is how a host says "do this, but let me return first". Supplying
-- | `void <<< H.fork` under Halogen is what makes an update optimistic — the
-- | phase is already written, so the component paints before the request goes
-- | out. A host with nothing to paint may supply `identity` and run commands
-- | inline; both are correct, and the machine cannot tell.
-- |
-- | `arm` is called on **every** state change with the new state's alarm or
-- | with `Nothing`, so "leaving a state cancels its deadline" is a rule of the
-- | driver rather than a thing each host has to remember. `Nothing` means
-- | cancel and set nothing; `Just` means cancel and set this.
type Wiring m =
  { spec :: Spec
  , phase :: m StateId
  , move :: StateId -> m Unit
  , world :: m World
  , perform :: CommandId -> m (Maybe EventId)
  , refused :: RefusalId -> m Unit
  , fork :: m Unit -> m Unit
  , arm :: Maybe Alarm -> m Unit
  }

-- | Feed one event to the machine, move it, and carry out what it asks for.
-- |
-- | The order is load-bearing. The phase is written **before** any command
-- | runs, and each command is forked, so a host that paints has painted by the
-- | time the request goes out.
-- |
-- | A command that yields an event feeds it straight back in. That recursion
-- | terminates because a machine that answers an event with a command has to
-- | reach a state whose entry commands are empty, or it was written wrong — a
-- | thing a lint can say and this function cannot.
fire :: forall m. Monad m => Wiring m -> EventId -> m Unit
fire w ev = do
  here <- w.phase
  world <- w.world
  let Tuple next record = step w.spec world here ev
  case record.outcome of
    -- Nothing moved, so nothing is entered, nothing exited, and a running
    -- deadline keeps running. That is a statechart *internal* transition, and
    -- re-arming here would silently make every ignored keypress extend the
    -- timeout.
    Stay -> pure unit
    Refuse rid -> w.refused rid
    Move _ -> do
      w.move next
      w.arm (alarmFor w.spec world next)
  runAll w record.commands

-- | Put the machine into a state the host already knows it is in, and run that
-- | state's entry commands.
-- |
-- | This is the obligation `fire` cannot discharge. A host that seeds its phase
-- | from outside the machine — from a server response, from a config file, from
-- | a process it found already running — has performed a `Move` that no event
-- | caused, and the entry commands of the state it lands in are owed just the
-- | same. Writing the field directly skips them, silently, for ever.
-- |
-- | Deliberately more general than `start`, which answers only for the
-- | machine's declared `initial`. A host that seeds has as many entry points as
-- | it has seedable states.
enter :: forall m. Monad m => Wiring m -> StateId -> m Unit
enter w phase = do
  world <- w.world
  w.move phase
  w.arm (alarmFor w.spec world phase)
  runAll w (entryOf w.spec phase)

-- | Enter the machine's declared initial state. What a host calls once, at the
-- | point it has nothing better to seed from.
start :: forall m. Monad m => Wiring m -> m Unit
start w = enter w w.spec.initial

-- | Carry out commands, off the caller's path, feeding any follow-up event
-- | straight back in.
runAll :: forall m. Monad m => Wiring m -> Array CommandId -> m Unit
runAll w cmds = for_ cmds \cmd -> w.fork do
  followUp <- w.perform cmd
  for_ followUp (fire w)
