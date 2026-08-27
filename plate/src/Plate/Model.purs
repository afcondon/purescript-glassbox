-- | Plate.Model
-- |
-- | Everything the page needs, derived from a `Spec` before any markup exists.
-- |
-- | The rule this module obeys is the one `Glassbox.Edges` states for itself:
-- | **nothing here is a second description of the machine.** The grid is
-- | produced by looking at `spec.rules`, the reachability by asking
-- | `Glassbox.Edges`, the findings by asking `Glassbox.Lint`. Where a fact
-- | could be either derived or written down, it is derived, because a page
-- | whose job is to help you catch a wrong cell must not have a wrong cell of
-- | its own.
-- |
-- | ### The six cell kinds, and why `Absent` is one of them
-- |
-- | A `(state, event)` pair with no rule resolves to `Stay` at run time and is
-- | a completely different thing at design time: nobody decided it. The format
-- | has no totality lint, so the grid is the only place that difference can be
-- | seen, and it is the reason a grid beats a list here. `Turns` is the second
-- | distinction the text rendering cannot draw: a `Move` to the state you are
-- | already in is not a `Stay` — it exits and re-enters, and re-entering runs
-- | the entry commands. `raised × tick -> raised` is a supervisor's whole
-- | keep-alive loop and it would read as an idle cell without this.
-- |
-- | ### Three readings of one artifact
-- |
-- | A `Plate` carries the machine three ways, and all three come out of the
-- | library rather than out of this package: the grid is `spec.rules` set as a
-- | table, `blocks` is `Glassbox.DSL.Print`'s authoring syntax, and `diagram`
-- | is `Glassbox.Export.StateDiagram` over `Glassbox.Edges`. They are here to
-- | be *compared*. Where two of them disagree about a machine, one of them is
-- | wrong about code somebody is running, and the point of setting them on one
-- | page is that the disagreement is then something you can see.
module Plate.Model
  ( CellKind(..)
  , Cell
  , Row
  , Note
  , Tally
  , Audit
  , MissingEdge
  , Plate
  , plateOf
  , cellTarget
  , caseLadder
  , Branch
  , configReads
  , factReads
  ) where

import Prelude

import Data.Array (concatMap, elem, filter, foldl, length, mapWithIndex, nub, null, snoc, take)
import Data.Foldable (all, any)
import Data.Maybe (Maybe(..), isNothing)
import Glassbox.DSL.Print (Syntax(..), print)
import Glassbox.Edges (Edge, EdgeKind(..), defaultOptions, edgesOf, inboundTo, unreachableStates)
import Glassbox.Export.StateDiagram (toStateDiagram)
import Glassbox.Lint (Finding, lint)
import Glassbox.Run (worldFrom)
import Glassbox.Spec
  ( Case
  , CommandId
  , ConfigId
  , Deadline
  , EventId
  , Expr(..)
  , FactId
  , Guard(..)
  , Outcome(..)
  , RefusalId
  , Rule
  , Spec
  , StateDecl
  , StateId
  , deadlineFor
  )

-- | What one state does with one event, as a page has to draw it.
data CellKind
  = Absent
  -- ^ no rule. The runtime says `Stay`; nobody said so.
  | Stays
  | Moves StateId
  | Turns StateId
  -- ^ a `Move` to the state it is already in: exit, re-enter, run entry again.
  | Refuses RefusalId
  | Guarded Int
  -- ^ escapes to the numbered case ladder below the grid.

derive instance eqCellKind :: Eq CellKind

type Cell =
  { on :: EventId
  , kind :: CellKind
  -- | This pair is also what a deadline in this state fires, so the cell can
  -- | happen with nobody pressing anything.
  , timed :: Boolean
  }

type Row =
  { state :: StateDecl
  , cells :: Array Cell
  , deadline :: Maybe Deadline
  , inbound :: Int
  , initial :: Boolean
  }

type Note =
  { index :: Int
  , from :: StateId
  , on :: EventId
  , cases :: Array Case
  }

type Tally =
  { moves :: Int
  , turns :: Int
  , stays :: Int
  , refusals :: Int
  , guarded :: Int
  , absent :: Int
  , total :: Int
  }

-- | The checks, run over the artifact and reported whether or not they find
-- | anything. A check with no output looks exactly like a check that was never
-- | run, which is the failure mode this whole page exists to prevent.
type Audit =
  { gaps :: Array { from :: StateId, on :: EventId }
  , unreachable :: Array StateId
  , unusedCommands :: Array CommandId
  , unusedRefusals :: Array RefusalId
  , unreadFacts :: Array FactId
  , unreadConfig :: Array ConfigId
  , findings :: Array Finding
  -- | Transitions a rule names that the derived edge set does not contain.
  --
  -- Two readings of one artifact, compared. The grid is built from
  -- `spec.rules`; the reachability and the diagrams come from
  -- `Glassbox.Edges`, which resolves each pair under every assignment of the
  -- facts. Those should agree, and where they do not, one of them is wrong
  -- about a machine somebody is running.
  --
  -- Only rules offering **two or more distinct destinations** are compared,
  -- and that restriction is what makes the answer trustworthy. A single-target
  -- rule can legitimately vanish from the edge set — configuration genuinely
  -- reshapes a machine, and a `cd` state with no CD slot fitted *should* lose
  -- its inbound edge.
  --
  -- Even inside a rule that offers a choice, one arm can be *config*-guarded,
  -- and then losing it is right rather than wrong: `edgesOf` enumerates facts
  -- and holds config at its declared defaults, deliberately, because config is
  -- what decides which machine this is. So each entry says which of the two it
  -- is, and only the fact-guarded ones are a defect.
  , missingEdges :: Array MissingEdge
  }

-- | A transition a rule names that no drawing shows, and why it is absent.
-- |
-- | `configGuarded` is true when every arm of the rule that produces this
-- | destination sits behind a config read — its own guard, or the guard of a
-- | case tried before it, since an earlier case winning is what stops a later
-- | one being reached. Config at its defaults can then genuinely rule the arm
-- | out, and a machine with `has-prewash: false` really cannot reach
-- | `prewashing`. Where no config is involved, nothing about the artifact
-- | explains the absence and the edge set is simply short.
type MissingEdge =
  { from :: StateId
  , on :: EventId
  , to :: StateId
  , configGuarded :: Boolean
  }

type Plate =
  { name :: String
  , spec :: Spec
  , rows :: Array Row
  , notes :: Array Note
  , tally :: Tally
  , audit :: Audit
  , edges :: Array Edge
  -- | The machine in the syntax it would be *written* in, from the library's
  -- | own printer. Nothing is sliced out of it: the page sets what
  -- | `Glassbox.DSL.Print` emits, and a reader comparing it against the grid
  -- | above is comparing two renderings and not a rendering against a summary.
  , blocks :: String
  -- | The machine as `stateDiagram-v2` text, generated here and now from the
  -- | edge set this same plate audits — never read from a committed fixture,
  -- | which would be a fourth description of the machine and the one nobody
  -- | regenerates.
  , diagram :: String
  }

plateOf :: String -> Spec -> Plate
plateOf name spec =
  { name
  , spec
  , rows: built.rows
  , notes: built.notes
  , tally: tallyOf built.rows
  , audit: auditOf spec edges built.rows
  , edges
  , blocks: print Blocks spec
  , diagram: toStateDiagram spec edges
  }
  where
  edges = edgesOf defaultOptions (worldFrom spec) spec
  built = buildRows spec edges

-- =============================================================================
-- The grid
-- =============================================================================

-- | One pass in reading order, numbering the escapes as it goes, so `1` is the
-- | first guarded cell an eye meets going left to right and top to bottom. The
-- | numbering matches `Glassbox.DSL.Print`'s deliberately: the two renderings
-- | of a machine should be readable side by side.
buildRows :: Spec -> Array Edge -> { rows :: Array Row, notes :: Array Note }
buildRows spec edges = foldl addRow { rows: [], notes: [] } spec.states
  where
  addRow acc state =
    let
      stepped = foldl (addCell state) { cells: [], notes: acc.notes } spec.events
      row =
        { state
        , cells: stepped.cells
        , deadline: deadlineFor spec state.id
        , inbound: length (filter (\e -> e.from /= e.to) (inboundTo state.id edges))
        , initial: state.id == spec.initial
        }
    in
      { rows: snoc acc.rows row, notes: stepped.notes }

  addCell state acc event =
    let
      matching = filter (\r -> r.from == state.id && r.on == event.id) spec.rules
      timed = not (null (filter (\d -> d.inState == state.id && d.fires == event.id) spec.deadlines))
      index = length acc.notes + 1
    in
      case matching of
        [] ->
          { cells: snoc acc.cells { on: event.id, kind: Absent, timed }, notes: acc.notes }
        [ r ] | simple r ->
          { cells: snoc acc.cells { on: event.id, kind: plain state.id r, timed }
          , notes: acc.notes
          }
        rules ->
          { cells: snoc acc.cells { on: event.id, kind: Guarded index, timed }
          , notes: snoc acc.notes
              { index
              , from: state.id
              , on: event.id
              , cases: concatMap _.cases rules
              }
          }

  simple r = case r.cases of
    [ c ] -> isNothing c.when
    _ -> false

  plain from r = case r.cases of
    [ c ] -> outcomeKind from c.outcome
    _ -> Guarded 0

outcomeKind :: StateId -> Outcome -> CellKind
outcomeKind from = case _ of
  Move to | to == from -> Turns to
  Move to -> Moves to
  Stay -> Stays
  Refuse rid -> Refuses rid

-- | Where a cell points, for the reach highlighting. `Nothing` for the cells
-- | that go nowhere.
cellTarget :: CellKind -> Maybe StateId
cellTarget = case _ of
  Moves to -> Just to
  Turns to -> Just to
  _ -> Nothing

tallyOf :: Array Row -> Tally
tallyOf rows = foldl count zero' (concatMap _.cells rows)
  where
  zero' = { moves: 0, turns: 0, stays: 0, refusals: 0, guarded: 0, absent: 0, total: 0 }
  count t c =
    let
      t' = t { total = t.total + 1 }
    in
      case c.kind of
        Moves _ -> t' { moves = t.moves + 1 }
        Turns _ -> t' { turns = t.turns + 1 }
        Stays -> t' { stays = t.stays + 1 }
        Refuses _ -> t' { refusals = t.refusals + 1 }
        Guarded _ -> t' { guarded = t.guarded + 1 }
        Absent -> t' { absent = t.absent + 1 }

-- =============================================================================
-- The case ladder
-- =============================================================================

-- | One rung, as it is read aloud: *when* this, *elif* that, *else*.
-- |
-- | Cases are tried in order and the first match wins, so the ladder is the
-- | machine's own order and must not be sorted. The keyword is a function of
-- | position, which is why it is computed here rather than stored.
type Branch =
  { keyword :: String
  , guard :: Maybe Guard
  , outcome :: Outcome
  , from :: StateId
  }

caseLadder :: StateId -> Array Case -> Array Branch
caseLadder from cases = mapWithIndex rung cases
  where
  rung i c =
    { keyword: case c.when of
        Nothing -> "else"
        Just _ -> if i == 0 then "when" else "elif"
    , guard: c.when
    , outcome: c.outcome
    , from
    }

-- =============================================================================
-- The audit
-- =============================================================================

auditOf :: Spec -> Array Edge -> Array Row -> Audit
auditOf spec edges rows =
  { gaps
  , unreachable: unreachableStates spec edges
  , unusedCommands: filter (\c -> not (c.id `elem` usedCommands)) spec.commands # map _.id
  , unusedRefusals: filter (\r -> not (r.id `elem` usedRefusals)) spec.refusals # map _.id
  , unreadFacts: filter (\f -> not (f.id `elem` factReads spec)) spec.facts # map _.id
  , unreadConfig: filter (\c -> not (c.id `elem` configReads spec)) spec.config # map _.id
  , findings: lint spec
  , missingEdges: concatMap ruleGap spec.rules
  }
  where
  ruleGap r =
    let
      targets = nub (concatMap (elsewhere r.from) r.cases)
    in
      if length targets < 2 then []
      else map (\to -> { from: r.from, on: r.on, to, configGuarded: suppressible r to })
        (filter (\to -> not (drawn r.from to)) targets)

  elsewhere from c = case c.outcome of
    Move to | to /= from -> [ to ]
    _ -> []

  -- | Can config, at its declared defaults, account for this arm's absence?
  --
  -- An arm is reached only when its own guard holds *and* every case tried
  -- before it failed, so a config read anywhere in that prefix can rule it
  -- out. `all` rather than `any` over the arms that produce this destination:
  -- one arm reaching it without config's permission is enough to make the
  -- missing edge a defect.
  suppressible r to =
    let
      arms = filter (\a -> a.outcome == Move to) (mapWithIndex armAt r.cases)
    in
      not (null arms) && all (\a -> readsConfigUpTo r a.at) arms

  armAt i c = { at: i, outcome: c.outcome }

  readsConfigUpTo r i = any caseReadsConfig (take (i + 1) r.cases)

  caseReadsConfig c = case c.when of
    Nothing -> false
    Just g -> not (null (guardConfig g))

  drawn from to =
    not (null (filter (\e -> e.from == from && e.to == to && e.kind /= OnRefusal) edges))

  gaps = concatMap gapsIn rows
  gapsIn row =
    map (\c -> { from: row.state.id, on: c.on })
      (filter (\c -> c.kind == Absent) row.cells)

  usedCommands = concatMap (\s -> s.entry <> s.exit) spec.states

  usedRefusals = concatMap refusalsOf spec.rules
  refusalsOf :: Rule -> Array RefusalId
  refusalsOf r = concatMap
    ( \c -> case c.outcome of
        Refuse rid -> [ rid ]
        _ -> []
    )
    r.cases

-- | Every config id a guard or a deadline actually reads.
-- |
-- | The deadline half matters and is easy to miss: a looper's `count-in-beats`
-- | appears in no guard at all, only in `after`, and a check that looked only
-- | at guards would report the machine's most important setting as dead.
configReads :: Spec -> Array ConfigId
configReads spec =
  concatMap guardConfig (allGuards spec) <> concatMap deadlineConfig spec.deadlines
  where
  deadlineConfig :: Deadline -> Array ConfigId
  deadlineConfig d = exprConfig d.after

factReads :: Spec -> Array FactId
factReads spec = concatMap guardFacts (allGuards spec)

allGuards :: Spec -> Array Guard
allGuards spec = concatMap (\r -> concatMap caseGuard r.cases) spec.rules
  where
  caseGuard c = case c.when of
    Just g -> [ g ]
    Nothing -> []

guardConfig :: Guard -> Array ConfigId
guardConfig = case _ of
  Holds e -> exprConfig e
  Not g -> guardConfig g
  And gs -> concatMap guardConfig gs
  Or gs -> concatMap guardConfig gs
  Cmp _ a b -> exprConfig a <> exprConfig b

guardFacts :: Guard -> Array FactId
guardFacts = case _ of
  Holds e -> exprFacts e
  Not g -> guardFacts g
  And gs -> concatMap guardFacts gs
  Or gs -> concatMap guardFacts gs
  Cmp _ a b -> exprFacts a <> exprFacts b

exprConfig :: Expr -> Array ConfigId
exprConfig = case _ of
  ConfigOf cid -> [ cid ]
  _ -> []

exprFacts :: Expr -> Array FactId
exprFacts = case _ of
  FactOf fid -> [ fid ]
  _ -> []
