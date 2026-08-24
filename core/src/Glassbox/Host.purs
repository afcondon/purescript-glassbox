-- | Glassbox.Host
-- |
-- | The **host side of the seam**: how a program that runs a machine declares
-- | what it can do, and how the compiler is made to check it.
-- |
-- | Making the machine data gave up the exhaustiveness a `case` expression
-- | provided. A `case` over `CommandId` cannot be total, because `CommandId` is
-- | a string; the compiler cannot know the list. So the host declares its own
-- | vocabulary as a **row**, and two different mechanisms recover the two
-- | halves of what was lost:
-- |
-- | - a **record** is a product, so a host cannot leave a handler out — that is
-- |   `ToObject` and `dispatch`, and it makes the outbound side total;
-- | - a **`Variant`** is a sum, so a renderer cannot forget a state — that is
-- |   `Reify`, and it makes the inbound side total.
-- |
-- | And it is worth saying plainly what this does *not* do: the compiler is not
-- | checking the host against the file. It is checking the host against **its
-- | own declaration**. `reportCommands` and friends check the declaration
-- | against the file, at load, once. Neither half is sufficient alone, and a
-- | host that skips the second half has a type-checked lie.
-- |
-- | Nothing here knows what a UI is, and nothing here performs an effect. This
-- | module was written three times inside one application before it was lifted;
-- | asking every host to re-derive it is the same duplication the project
-- | exists to remove.
module Glassbox.Host
  ( class Labels
  , labels
  , vocabularyOf
  , class ToObject
  , toObject
  , handlers
  , dispatch
  , class Reify
  , reifyImpl
  , reifyTag
  , reify
  , event
  , VocabularyReport
  , agrees
  , reportAgainst
  , reportCommands
  , reportStates
  , reportEvents
  , reportRefusals
  ) where

import Prelude

import Data.Array (cons, difference, null)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import Foreign.Object (Object)
import Foreign.Object as Object
import Glassbox.Spec
  ( CommandId(..)
  , EventId(..)
  , RefusalId(..)
  , Spec
  , StateId(..)
  , commandIds
  , eventIds
  , stateIds
  )
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Record as Record
import Type.Proxy (Proxy(..))

-- =============================================================================
-- A row's labels, as strings
-- =============================================================================

-- | Reflect a row's labels to strings.
-- |
-- | The value type is ignored, so one row can be declared once and instantiated
-- | at whatever a given host's handlers happen to be — which is why a generated
-- | vocabulary is written as `Commands v` rather than fixed to one monad. The
-- | check needs the names; it has no opinion about the values.
class Labels (rl :: RowList Type) where
  labels :: Proxy rl -> Array String

instance labelsNil :: Labels Nil where
  labels _ = []

instance labelsCons :: (IsSymbol k, Labels rest) => Labels (Cons k v rest) where
  labels _ = cons (reflectSymbol (Proxy :: Proxy k)) (labels (Proxy :: Proxy rest))

-- | Every label of a row, as strings.
vocabularyOf :: forall r rl. RowToList r rl => Labels rl => Proxy r -> Array String
vocabularyOf _ = labels (Proxy :: Proxy rl)

-- =============================================================================
-- A record of handlers, as a lookup table
-- =============================================================================

-- | Turn a homogeneous record into an `Object`, keyed by its own labels.
-- |
-- | No unsafe step: `Record.get` needs a `Row.Cons` proof that the label really
-- | is in the record, and the fold produces one per field. What comes out is a
-- | lookup table whose keys the compiler guaranteed.
class ToObject (rl :: RowList Type) (r :: Row Type) v where
  toObject :: Proxy rl -> Record r -> Object v

instance toObjectNil :: ToObject Nil r v where
  toObject _ _ = Object.empty

instance toObjectCons ::
  ( IsSymbol k
  , Row.Cons k v rx r
  , ToObject rest r v
  ) =>
  ToObject (Cons k v rest) r v where
  toObject _ rec =
    Object.insert
      (reflectSymbol (Proxy :: Proxy k))
      (Record.get (Proxy :: Proxy k) rec)
      (toObject (Proxy :: Proxy rest) rec)

handlers :: forall r rl v. RowToList r rl => ToObject rl r v => Record r -> Object v
handlers = toObject (Proxy :: Proxy rl)

-- | Look one command up in a record of handlers.
-- |
-- | This is what replaces the `case` a host used to write. It returns `Maybe`
-- | rather than taking a fallback, because the two callers want opposite
-- | things: a driver wants to do nothing, and a test wants to know. The `Nothing`
-- | branch is reached only by a command the record has no field for, which
-- | `reportCommands` is meant to have caught at load — so it is belt-and-braces
-- | rather than routine.
dispatch
  :: forall r rl v
   . RowToList r rl
  => ToObject rl r v
  => Record r
  -> CommandId
  -> Maybe v
dispatch rec (CommandId cmd) = Object.lookup cmd (handlers rec)

-- =============================================================================
-- A string, as a member of a declared row
-- =============================================================================

-- | Match a runtime string against a row's labels, injecting on the first hit.
-- |
-- | The value type is fixed to `Unit` because every identifier in the format is
-- | an **opaque** identifier and carries nothing. If they ever gain payloads,
-- | this is the class that has to learn about them.
class Reify (rl :: RowList Type) (r :: Row Type) where
  reifyImpl :: Proxy rl -> String -> Maybe (Variant r)

instance reifyNil :: Reify Nil r where
  reifyImpl _ _ = Nothing

instance reifyCons ::
  ( IsSymbol k
  , Row.Cons k Unit rx r
  , Reify rest r
  ) =>
  Reify (Cons k Unit rest) r where
  reifyImpl _ s =
    if s == reflectSymbol (Proxy :: Proxy k) then Just (V.inj (Proxy :: Proxy k) unit)
    else reifyImpl (Proxy :: Proxy rest) s

-- | The one place a string becomes a member of a row, or fails.
-- |
-- | One function serves states, refusals and anything else that grows a row
-- | later, because they are all opaque strings. It goes one way, it can fail,
-- | and the reports below are what prove at load that for a given artifact it
-- | never will.
reifyTag :: forall r rl. RowToList r rl => Reify rl r => String -> Maybe (Variant r)
reifyTag = reifyImpl (Proxy :: Proxy rl)

-- | The `StateId` case spelled out, because it is the one hosts reach for.
reify :: forall r rl. RowToList r rl => Reify rl r => StateId -> Maybe (Variant r)
reify (StateId s) = reifyTag s

-- =============================================================================
-- Naming an event the machine declares
-- =============================================================================

-- | Name an event, checked against the machine's declared alphabet.
-- |
-- | `EventId` is a string, so `EventId "pres"` typechecks and then quietly does
-- | nothing for ever — the machine has no rule for it, the step falls through,
-- | and the button is simply dead. The row makes the misspelling a compile
-- | error, because `Row.Cons` has to find the label.
-- |
-- | This is the direction the vocabulary reports cannot help with. They compare
-- | *sets* at load; this checks a single *use* at compile time, which is what
-- | a typo needs.
event
  :: forall @sym r rx
   . IsSymbol sym
  => Row.Cons sym Unit rx r
  => Proxy r
  -> EventId
event _ = EventId (reflectSymbol (Proxy :: Proxy sym))

-- =============================================================================
-- Relating a host's declaration to an artifact
-- =============================================================================

-- | What a host's declared vocabulary and a machine's have to say to each
-- | other.
-- |
-- | `missing` is fatal — the machine names something the host has no word for.
-- | `unused` is not, because one host may legitimately serve several machines,
-- | so it is worth a warning and nothing more.
type VocabularyReport = { missing :: Array String, unused :: Array String }

-- | Whether a report is one a host can proceed on.
agrees :: VocabularyReport -> Boolean
agrees r = null r.missing

reportAgainst
  :: forall r rl
   . RowToList r rl
  => Labels rl
  => Proxy r
  -> Array String
  -> VocabularyReport
reportAgainst proxy wanted =
  { missing: difference wanted implemented
  , unused: difference implemented wanted
  }
  where
  implemented = vocabularyOf proxy

-- | Does this host implement every command the machine asks for?
reportCommands
  :: forall r rl. RowToList r rl => Labels rl => Proxy r -> Spec -> VocabularyReport
reportCommands proxy spec =
  reportAgainst proxy (map (\(CommandId c) -> c) (commandIds spec))

-- | Does this host render every state the machine can be in?
-- |
-- | `missing` means the machine can reach a state the renderer has no case for
-- | — exactly the hole a catch-all used to hide, now a load failure. `unused`
-- | means a case for a state this machine never reaches, which is dead code
-- | rather than a fault.
reportStates
  :: forall r rl. RowToList r rl => Labels rl => Proxy r -> Spec -> VocabularyReport
reportStates proxy spec =
  reportAgainst proxy (map (\(StateId s) -> s) (stateIds spec))

-- | Does this host know every event the machine listens for?
-- |
-- | `missing` is milder here than elsewhere and is worth reading carefully: an
-- | event the host never sends is a **feature it has not wired up**, not a
-- | broken machine. It is reported so that absence is a decision.
reportEvents
  :: forall r rl. RowToList r rl => Labels rl => Proxy r -> Spec -> VocabularyReport
reportEvents proxy spec =
  reportAgainst proxy (map (\(EventId e) -> e) (eventIds spec))

-- | Does this host have something to say for every refusal the machine can
-- | give? A refusal that reaches nobody is the failure the `Stay`/`Refuse`
-- | distinction exists to prevent.
reportRefusals
  :: forall r rl. RowToList r rl => Labels rl => Proxy r -> Spec -> VocabularyReport
reportRefusals proxy spec =
  reportAgainst proxy (map (\(RefusalId r) -> r) (map _.id spec.refusals))
