-- | Test.Fixture
-- |
-- | Hand-built machines, small enough to reason about in one glance.
-- |
-- | The corpus in `core/machines/` answers "does a real artifact work"; these
-- | answer "what does this one function do", and for that a five-state washing
-- | machine is noise. Everything here is a record update on `blank`, so a
-- | fixture shows only the part of the format the test is about.
module Test.Fixture
  ( blank
  , st
  , finalSt
  , ev
  , runtimeEv
  , cfg
  , fact
  , refusal
  , command
  , doing
  , cid
  , to
  , stay
  , refuse
  , always
  , edge
  , emptyWorld
  , sid
  , eid
  , num
  , str
  , bool
  ) where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Glassbox.Edges (Edge, EdgeKind)
import Glassbox.Run (World)
import Glassbox.Spec
  ( Case
  , CommandDecl
  , CommandId(..)
  , ConfigDecl
  , ConfigId(..)
  , EventDecl
  , EventId(..)
  , EventSource(..)
  , FactDecl
  , FactId(..)
  , Guard
  , Outcome(..)
  , RefusalDecl
  , RefusalId(..)
  , Spec
  , StateDecl
  , StateId(..)
  , Value(..)
  )

-- | A well-formed machine with nothing in it. Every fixture is a record update
-- | on this, which keeps a fixture to the lines the test actually cares about.
blank :: Spec
blank =
  { version: 1
  , id: "fixture"
  , notes: []
  , title: "Fixture"
  , initial: StateId "a"
  , states: []
  , events: []
  , config: []
  , facts: []
  , refusals: []
  , commands: []
  , rules: []
  , deadlines: []
  , clock: Nothing
  }

sid :: String -> StateId
sid = StateId

eid :: String -> EventId
eid = EventId

st :: String -> StateDecl
st raw = { id: StateId raw, label: raw, final: false, entry: [], exit: [] }

finalSt :: String -> StateDecl
finalSt raw = { id: StateId raw, label: raw, final: true, entry: [], exit: [] }

-- | A state that tells the host to do something on the way in and on the way
-- | out. The two arrays are separate arguments rather than a record update so
-- | that a command fixture reads in the order the commands actually run.
doing :: String -> Array String -> Array String -> StateDecl
doing raw entry exit =
  (st raw) { entry = map CommandId entry, exit = map CommandId exit }

command :: String -> CommandDecl
command raw = { id: CommandId raw, label: raw }

cid :: String -> CommandId
cid = CommandId

ev :: String -> EventDecl
ev raw = { id: EventId raw, label: raw, source: FromUser }

runtimeEv :: String -> EventDecl
runtimeEv raw = { id: EventId raw, label: raw, source: FromRuntime }

cfg :: String -> Value -> ConfigDecl
cfg raw def = { id: ConfigId raw, label: raw, default: def }

fact :: String -> FactDecl
fact raw = { id: FactId raw, label: raw }

refusal :: String -> String -> RefusalDecl
refusal raw text = { id: RefusalId raw, text }

-- | A guarded case that moves.
to :: Maybe Guard -> String -> Case
to when' target = { when: when', outcome: Move (StateId target) }

stay :: Maybe Guard -> Case
stay when' = { when: when', outcome: Stay }

refuse :: Maybe Guard -> String -> Case
refuse when' rid = { when: when', outcome: Refuse (RefusalId rid) }

-- | The catch-all `when`, spelled out so a fixture reads as prose.
always :: Maybe Guard
always = Nothing

edge :: String -> String -> Array String -> EdgeKind -> Maybe String -> Edge
edge from target events kind reason =
  { from: StateId from
  , to: StateId target
  , events: map EventId events
  , kind
  , reason: map RefusalId reason
  }

-- | Not `worldFrom blank` — deliberately a world that knows nothing at all, so
-- | a lookup of anything is a genuine miss.
emptyWorld :: World
emptyWorld = { config: Map.empty, facts: Map.empty }

num :: Number -> Value
num = VNumber

str :: String -> Value
str = VString

bool :: Boolean -> Value
bool = VBoolean
