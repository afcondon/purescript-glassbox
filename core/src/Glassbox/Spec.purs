-- | Glassbox.Spec
-- |
-- | A state machine as **data loaded at runtime**, not as code.
-- |
-- | This is the shape a decoded artifact takes. Nothing here is a function, so
-- | every part of a machine can be serialised, hashed, diffed, drawn and linted;
-- | the previous design made `transition` a PureScript function, which meant a
-- | machine could only be written by a programmer and could never leave the
-- | binary. See `docs/kb/plans/the-same-machine.md`.
-- |
-- | ### The four vocabularies
-- |
-- | The format has four vocabularies and they are owned by different people.
-- |
-- |   * **States and events** are opaque identifiers owned by the machine's
-- |     author. No runtime interprets them; it moves between them and reports
-- |     where it is. This is why they are `String` newtypes rather than a sum
-- |     type — a sum type would mean the set was fixed by whoever compiled the
-- |     host, which is exactly the property being given up.
-- |   * **Config and facts** are the two halves of the world a guard may read.
-- |     Config is declared by the artifact and reshapes the machine (a count-in
-- |     of zero genuinely removes a state); facts are supplied by the host at
-- |     each step and describe the world (another loop holds the converter).
-- |   * **Refusals** are a closed list the artifact declares, so a refusal
-- |     carries a tag rather than prose. A second runtime cannot pattern-match
-- |     English, and a display that wants to draw a refusal needs to know which
-- |     one it was.
-- |   * **Commands** are the fourth vocabulary and the newest. They are a closed
-- |     list the artifact declares, like refusals, and they hang off **state
-- |     entry and exit** rather than off edges. That choice is the whole of the
-- |     effects fork and it is argued in `docs/kb/plans/glassbox-open-forks.md`:
-- |     four edges lead to Recording and each would have to remember to start
-- |     the recorder, whereas *entering Recording starts the recorder* is said
-- |     once and holds for every inbound edge including the one somebody adds
-- |     next month. A command carries no arguments — the machine emits an
-- |     intent and the host, which holds all the memory, resolves the details.
-- |
-- | ### What is deliberately absent
-- |
-- | **Edge-specific commands.** A command that depends on which event you
-- | arrived by — `PressClear` from Armed has audio to discard, from Playing it
-- | does not — cannot be said. Shipping both shapes at once is how a format
-- | becomes unreadable; this one waits for an application that genuinely needs
-- | it.
-- |
-- | **Hierarchy.** No state contains another. The cheap version costs one
-- | `Outcome` constructor and a `parent` field, and is written up in the same
-- | document, but nothing yet needs it.
module Glassbox.Spec
  ( StateId(..)
  , EventId(..)
  , RefusalId(..)
  , ConfigId(..)
  , FactId(..)
  , CommandId(..)
  , Value(..)
  , valueToString
  , truthy
  , Expr(..)
  , CmpOp(..)
  , Guard(..)
  , Outcome(..)
  , outcomeTarget
  , isRefusal
  , Case
  , Rule
  , Deadline
  , StateDecl
  , EventDecl
  , EventSource(..)
  , ConfigDecl
  , FactDecl
  , RefusalDecl
  , CommandDecl
  , Spec
  , formatVersion
  , requiredVersion
  , versionFor
  , stateIds
  , eventIds
  , commandIds
  , labelOfState
  , labelOfEvent
  , labelOfCommand
  , textOfRefusal
  , entryOf
  , exitOf
  , ruleFor
  , deadlineFor
  , userEvents
  ) where

import Prelude

import Data.Array (any, filter, find, head, null)
import Data.Maybe (Maybe, maybe)
import Data.Number.Format (toString)

-- | The newest wire format version this module understands.
-- |
-- | Present from the first commit because a second implementation — in another
-- | language, on another runtime — will read these artifacts, and a format with
-- | two implementations and no version is a format that cannot be changed. It
-- | earned its keep at version 2, which added commands.
formatVersion :: Int
formatVersion = 2

-- | The **oldest** version that can express a given machine.
-- |
-- | Version is a fact about the artifact rather than a preference of whoever
-- | wrote it, so it is computed and not asked for. A machine with no commands
-- | is still a version 1 machine and is written as one, which is what lets an
-- | older reader reject exactly the files it genuinely cannot run instead of
-- | every file written since the feature landed.
requiredVersion :: Spec -> Int
requiredVersion spec =
  if null spec.commands && not (any hasCommands spec.states) then 1 else 2
  where
  hasCommands s = not (null s.entry) || not (null s.exit)

-- | The version to stamp on an artifact, and the version a decoder should
-- | record: never below what the file claimed, never below what it uses.
-- |
-- | Both halves matter. A hand-written file that says 1 and carries a command
-- | is understating itself, and correcting it on read is kinder than refusing
-- | it — the author added a feature, not a mistake. A file that says 7 and uses
-- | nothing keeps its 7, because we are in no position to say what version 7
-- | means. (Anything above `formatVersion` never gets this far; the codec
-- | refuses it outright.)
versionFor :: Spec -> Int
versionFor spec = max spec.version (requiredVersion spec)

-- =============================================================================
-- Identifiers
-- =============================================================================

newtype StateId = StateId String

derive newtype instance eqStateId :: Eq StateId
derive newtype instance ordStateId :: Ord StateId
derive newtype instance showStateId :: Show StateId

newtype EventId = EventId String

derive newtype instance eqEventId :: Eq EventId
derive newtype instance ordEventId :: Ord EventId
derive newtype instance showEventId :: Show EventId

newtype RefusalId = RefusalId String

derive newtype instance eqRefusalId :: Eq RefusalId
derive newtype instance ordRefusalId :: Ord RefusalId
derive newtype instance showRefusalId :: Show RefusalId

newtype ConfigId = ConfigId String

derive newtype instance eqConfigId :: Eq ConfigId
derive newtype instance ordConfigId :: Ord ConfigId
derive newtype instance showConfigId :: Show ConfigId

newtype FactId = FactId String

derive newtype instance eqFactId :: Eq FactId
derive newtype instance ordFactId :: Ord FactId
derive newtype instance showFactId :: Show FactId

-- | Something the artifact asks the host to do.
-- |
-- | Opaque here, exactly as a state is: the format knows that `start-recorder`
-- | must be carried out on entering Recording, and knows nothing whatever about
-- | recorders. Every meaning lives on the host side of the boundary, which is
-- | what lets a browser, an Erlang node and a microcontroller run the same
-- | machine and each do something different with it.
newtype CommandId = CommandId String

derive newtype instance eqCommandId :: Eq CommandId
derive newtype instance ordCommandId :: Ord CommandId
derive newtype instance showCommandId :: Show CommandId

-- =============================================================================
-- Values, expressions and guards
-- =============================================================================

-- | What a config setting or a fact can hold.
data Value
  = VNumber Number
  | VBoolean Boolean
  | VString String

derive instance eqValue :: Eq Value
derive instance ordValue :: Ord Value

instance showValue :: Show Value where
  show = valueToString

valueToString :: Value -> String
valueToString = case _ of
  VNumber n -> toString n
  VBoolean b -> if b then "true" else "false"
  VString s -> s

-- | Whether a value counts as true where a guard expects a condition.
-- |
-- | Only a boolean is true or false. A number or a string used where a
-- | condition belongs is a mistake in the artifact rather than a value to
-- | coerce, and it reads as false so that the vocabulary lint has something to
-- | catch rather than the machine silently taking a branch.
truthy :: Value -> Boolean
truthy = case _ of
  VBoolean b -> b
  _ -> false

-- | Something a guard can evaluate to a value.
data Expr
  = Lit Value
  | ConfigOf ConfigId
  | FactOf FactId

derive instance eqExpr :: Eq Expr

data CmpOp = OpEq | OpNe | OpLt | OpLe | OpGt | OpGe

derive instance eqCmpOp :: Eq CmpOp
derive instance ordCmpOp :: Ord CmpOp

-- | A condition on the world, as data.
data Guard
  = Holds Expr
  | Not Guard
  | And (Array Guard)
  | Or (Array Guard)
  | Cmp CmpOp Expr Expr

derive instance eqGuard :: Eq Guard

-- =============================================================================
-- Outcomes and rules
-- =============================================================================

-- | What happened when an event met a state.
-- |
-- | `Stay` is deliberately distinct from `Refuse`: doing nothing because the
-- | event is meaningless here is not the same as doing nothing because something
-- | else in the system forbade it, and a press that leaves no trace is the
-- | failure this distinction exists to prevent.
data Outcome
  = Move StateId
  | Stay
  | Refuse RefusalId

derive instance eqOutcome :: Eq Outcome

-- | Where the machine ends up, given where it was and what happened.
outcomeTarget :: StateId -> Outcome -> StateId
outcomeTarget current = case _ of
  Move next -> next
  Stay -> current
  Refuse _ -> current

isRefusal :: Outcome -> Boolean
isRefusal = case _ of
  Refuse _ -> true
  _ -> false

-- | One branch of a rule.
-- |
-- | A `when` of `Nothing` is the catch-all. Cases are tried in order and the
-- | first match wins, which makes the machine deterministic by construction
-- | rather than by a check — at the cost that an artifact can hide an
-- | unreachable case behind an earlier one. That is what the overlap lint is
-- | for; it is not a soundness problem.
type Case =
  { when :: Maybe Guard
  , outcome :: Outcome
  }

-- | What one state does with one event.
type Rule =
  { from :: StateId
  , on :: EventId
  , cases :: Array Case
  }

-- | A transition that fires with no event from anybody.
-- |
-- | Names the **event that will be delivered**, not the state that will be
-- | reached, so that `rules` stays the single authority on where anything goes.
-- | Naming a target here would let the deadline and the rules disagree, and a
-- | diagram drawn from one would then lie about the machine driven by the other.
-- |
-- | `after` is a bare quantity in the artifact's own unit. What a `Deadline`
-- | should be so that it admits frames and beats and milliseconds without the
-- | format importing any of them is an open question in the plan.
type Deadline =
  { inState :: StateId
  , fires :: EventId
  , after :: Expr
  }

-- =============================================================================
-- Declarations
-- =============================================================================

-- | `entry` and `exit` are the commands the host must carry out on the way in
-- | and on the way out.
-- |
-- | They live on the **state**, not on the edges, and that is the load-bearing
-- | choice: it is what makes "entering Recording starts the recorder" a fact
-- | about Recording rather than a duty each of its four inbound edges has to
-- | remember. Order within each list is the order they are to be carried out
-- | in, so both are arrays and not sets.
type StateDecl =
  { id :: StateId
  , label :: String
  , final :: Boolean
  , entry :: Array CommandId
  , exit :: Array CommandId
  }

-- | Who delivers an event.
-- |
-- | A machine that draws a button for every event would offer the reader a
-- | button for the count-in elapsing, which misrepresents the machine. The
-- | distinction is in the artifact rather than inferred, because only the author
-- | knows it.
data EventSource
  = FromUser
  | FromRuntime

derive instance eqEventSource :: Eq EventSource
derive instance ordEventSource :: Ord EventSource

type EventDecl =
  { id :: EventId
  , label :: String
  , source :: EventSource
  }

type ConfigDecl =
  { id :: ConfigId
  , label :: String
  , default :: Value
  }

type FactDecl =
  { id :: FactId
  , label :: String
  }

type RefusalDecl =
  { id :: RefusalId
  , text :: String
  }

-- | A command the host must know how to carry out.
-- |
-- | Declared, and not merely mentioned on a state, for the same reason refusals
-- | are: the list is the **contract with the host**. A host can read it at load
-- | time and say up front which commands it cannot honour, rather than
-- | discovering the gap when a user presses something and nothing happens.
type CommandDecl =
  { id :: CommandId
  , label :: String
  }

-- | A whole machine, as decoded from an artifact.
-- |
-- | `notes` is prose the author wrote about the machine, and it is **part of
-- | the value** rather than a comment the codec throws away. It has to be: an
-- | artifact that cannot carry the reasoning behind it loses that reasoning the
-- | first time anything saves the file, and the corpus in `core/machines/` was
-- | already carrying seventy lines of it before the format could hold any.
type Spec =
  { version :: Int
  , id :: String
  , title :: String
  , notes :: Array String
  , initial :: StateId
  , states :: Array StateDecl
  , events :: Array EventDecl
  , config :: Array ConfigDecl
  , facts :: Array FactDecl
  , refusals :: Array RefusalDecl
  , commands :: Array CommandDecl
  , rules :: Array Rule
  , deadlines :: Array Deadline
  }

-- =============================================================================
-- Lookups
-- =============================================================================

stateIds :: Spec -> Array StateId
stateIds spec = map _.id spec.states

eventIds :: Spec -> Array EventId
eventIds spec = map _.id spec.events

commandIds :: Spec -> Array CommandId
commandIds spec = map _.id spec.commands

-- | Events a person can cause, which is what a surface should offer buttons for.
userEvents :: Spec -> Array EventId
userEvents spec = map _.id (filter (\e -> e.source == FromUser) spec.events)

labelOfState :: Spec -> StateId -> String
labelOfState spec sid@(StateId raw) =
  maybe raw _.label (find (\s -> s.id == sid) spec.states)

labelOfEvent :: Spec -> EventId -> String
labelOfEvent spec eid@(EventId raw) =
  maybe raw _.label (find (\e -> e.id == eid) spec.events)

labelOfCommand :: Spec -> CommandId -> String
labelOfCommand spec cid@(CommandId raw) =
  maybe raw _.label (find (\c -> c.id == cid) spec.commands)

-- | What the host must do on the way into a state, and on the way out.
-- |
-- | A state nobody declared has no commands, rather than being an error: the
-- | runtime is total everywhere else and there is no reason for this to be the
-- | one place it is not. The closure lint is what will object to the rule that
-- | named it.
entryOf :: Spec -> StateId -> Array CommandId
entryOf spec sid = maybe [] _.entry (find (\s -> s.id == sid) spec.states)

exitOf :: Spec -> StateId -> Array CommandId
exitOf spec sid = maybe [] _.exit (find (\s -> s.id == sid) spec.states)

textOfRefusal :: Spec -> RefusalId -> String
textOfRefusal spec rid@(RefusalId raw) =
  maybe raw _.text (find (\r -> r.id == rid) spec.refusals)

ruleFor :: Spec -> StateId -> EventId -> Maybe Rule
ruleFor spec from on = find (\r -> r.from == from && r.on == on) spec.rules

deadlineFor :: Spec -> StateId -> Maybe Deadline
deadlineFor spec sid = head (filter (\d -> d.inState == sid) spec.deadlines)
