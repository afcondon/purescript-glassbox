-- | Glassbox.DSL.Print
-- |
-- | Two textual syntaxes for a machine: the authoring one, and a grid that
-- | reads it back to you a different way.
-- |
-- | ### Why this exists
-- |
-- | JSON is a fine *wire* format and a poor *authoring* one. It is also a
-- | badly-shaped one for this particular corpus: measured over the five machines
-- | in `core/machines/`, **229 rules, 211 of them single-case (92%), 102 of those
-- | a bare `stay`, and only 18 with any guard at all**, across 655 lines. Nine
-- | tenths of an artifact is one-liners; the interesting content is concentrated
-- | in a handful of guarded rules. A syntax that does not exploit that shape is
-- | not paying its way.
-- |
-- | `Blocks` was chosen and has a parser — `Glassbox.DSL.Parse` — so it is the
-- | authoring format proper. `Table` remains print-only, deliberately: it is a
-- | *reading* of a machine, and its value is in showing what a list cannot.
-- |
-- | ### The two bets
-- |
-- |   * `Blocks` — grouped by state, events sharing an outcome listed together,
-- |     guards as an aligned alternation. The bet is that a reader's question is
-- |     usually *"what can I do from here?"*, which is a question about a state.
-- |   * `Table` — a state × event grid, with the 8% of guarded rules escaping to
-- |     footnotes. The bet is that the 92% belongs in a cell, and that a grid
-- |     makes a *missing* pair visible in a way a list never can — which matters
-- |     because the format has no totality lint yet and a missing pair silently
-- |     resolves to `Stay`.
-- |
-- | ### The rule both obey
-- |
-- | **Nothing in `Spec` may be dropped.** A syntax that loses information is
-- | disqualified as an authoring format, so where a construct is awkward it is
-- | printed awkwardly rather than omitted. Two consequences are visible in the
-- | output and are deliberate:
-- |
-- |   * A bare identifier in a guard is a config id *or* a fact id, resolved by
-- |     the declarations. When one name is declared as both — which no machine in
-- |     the corpus does, but the format permits — it is printed `config:x` /
-- |     `fact:x`. Short in the common case, exact when it has to be.
-- |   * A rule naming a state or event that is not declared cannot sit on the
-- |     Both are printed under loud headings rather than quietly lost.
-- |
-- | `!!` marks something the printer thinks is wrong with the artifact rather
-- | than with the notation.
module Glassbox.DSL.Print
  ( Syntax(..)
  , syntaxName
  , allSyntaxes
  , print
  , renderAll
  ) where

import Prelude

import Data.Array (concat, filter, find, findIndex, index, length, mapWithIndex, modifyAt, null, range, snoc)
import Data.Foldable (foldl, maximum)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (power)
import Data.Number.Format (toString)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), joinWith, stripSuffix)
import Data.String as String
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Tuple (Tuple(..))
import Glassbox.Spec
  ( Case
  , CmpOp(..)
  , CommandId(..)
  , ConfigId(..)
  , Deadline
  , clockName
  , EventId(..)
  , EventSource(..)
  , Expr(..)
  , FactId(..)
  , Guard(..)
  , Outcome(..)
  , RefusalId(..)
  , Rule
  , Spec
  , StateId(..)
  , Value(..)
  , versionFor
  )

-- =============================================================================
-- The choice
-- =============================================================================

data Syntax = Blocks | Table

derive instance eqSyntax :: Eq Syntax

syntaxName :: Syntax -> String
syntaxName = case _ of
  Blocks -> "blocks"
  Table -> "table"

allSyntaxes :: Array Syntax
allSyntaxes = [ Blocks, Table ]

print :: Syntax -> Spec -> String
print syntax spec = case syntax of
  Blocks -> printBlocks spec
  Table -> printTable spec

-- | Every machine in every syntax, keyed by the filename it should be written
-- | to.
-- |
-- | Pure, and returning the bytes rather than writing them, because the `glassbox`
-- | package has no `Effect` in its dependency list and printing does not need
-- | one. Whatever drives this — a node script, a test, a browser — owns the
-- | filesystem.
renderAll :: Array (Tuple String Spec) -> Array (Tuple String String)
renderAll inputs = do
  Tuple name spec <- inputs
  syntax <- allSyntaxes
  pure (Tuple (name <> "." <> syntaxName syntax <> ".txt") (print syntax spec))

-- =============================================================================
-- Column layout
-- =============================================================================

-- | One line of output: either literal text, or cells to be aligned with the
-- | cells of every other line in the same group.
data Line
  = Raw String
  | Cells (Array String)

-- | Align a group of lines into columns.
-- |
-- | A column that is empty on every row is **dropped**, not padded. That is what
-- | lets one row shape serve both `record  user` and `elapsed  runtime
-- | "count-in"` without every machine whose labels match its ids paying for a
-- | label column it never fills.
-- |
-- | Grouping is per call, so the caller decides what lines up with what: a
-- | declaration section's long free text must not push a transition's outcome
-- | column across the page, and it does not, because they are separate calls.
layout :: String -> Array Line -> Array String
layout indent lines = map render lines
  where
  cellRows = concat (map onlyCells lines)

  onlyCells = case _ of
    Cells cs -> [ cs ]
    Raw _ -> []

  columnCount = fromMaybe 0 (maximum (map length cellRows))

  widthOf i = fromMaybe 0 (maximum (map (\r -> maybe 0 String.length (index r i)) cellRows))

  kept =
    if columnCount == 0 then []
    else filter (\i -> widthOf i > 0) (range 0 (columnCount - 1))

  render = case _ of
    Raw s -> s
    Cells cs ->
      trimRight
        (indent <> joinWith "  " (map (\i -> pad (widthOf i) (fromMaybe "" (index cs i))) kept))

pad :: Int -> String -> String
pad width s = s <> power " " (max 0 (width - String.length s))

-- | Trailing spaces are invisible in an editor and loud in a diff, so a printer
-- | that pads columns has to take them off again.
trimRight :: String -> String
trimRight s = case stripSuffix (Pattern " ") s of
  Just shorter -> trimRight shorter
  Nothing -> s

finish :: Array String -> String
finish lines = joinWith "\n" lines <> "\n"

-- =============================================================================
-- Unwrapping
-- =============================================================================

rawState :: StateId -> String
rawState = case _ of
  StateId s -> s

rawEvent :: EventId -> String
rawEvent = case _ of
  EventId s -> s

rawRefusal :: RefusalId -> String
rawRefusal = case _ of
  RefusalId s -> s

rawConfig :: ConfigId -> String
rawConfig = case _ of
  ConfigId s -> s

rawFact :: FactId -> String
rawFact = case _ of
  FactId s -> s

rawCommand :: CommandId -> String
rawCommand = case _ of
  CommandId s -> s

-- =============================================================================
-- Guards, expressions, outcomes
-- =============================================================================

-- | The names a bare identifier in a guard cannot resolve on its own.
-- |
-- | Config and facts are separate namespaces in the format but share one
-- | notation here, because `converter-held` reads and `fact:converter-held` does
-- | not. A newtype rather than a bare `Set String` so that the *reason* the set
-- | exists travels with it: this is not "all the names", it is the collisions.
newtype Ambiguous = Ambiguous (Set String)

ambiguousIn :: Spec -> Ambiguous
ambiguousIn spec =
  Ambiguous (Set.intersection configNames factNames)
  where
  configNames = Set.fromFoldable (map (\c -> rawConfig c.id) spec.config)
  factNames = Set.fromFoldable (map (\f -> rawFact f.id) spec.facts)

qualify :: Ambiguous -> String -> String -> String
qualify (Ambiguous names) kind name =
  if Set.member name names then kind <> ":" <> name else name

-- | A string is quoted and an identifier is not, which is the only thing telling
-- | the literal `"fm"` apart from a fact called `fm`.
literalText :: Value -> String
literalText = case _ of
  VNumber n -> toString n
  VBoolean b -> if b then "true" else "false"
  VString s -> quote s

exprText :: Ambiguous -> Expr -> String
exprText amb = case _ of
  Lit v -> literalText v
  ConfigOf cid -> qualify amb "config" (rawConfig cid)
  FactOf fid -> qualify amb "fact" (rawFact fid)

opText :: CmpOp -> String
opText = case _ of
  OpEq -> "=="
  OpNe -> "!="
  OpLt -> "<"
  OpLe -> "<="
  OpGt -> ">"
  OpGe -> ">="

-- | `and` binds tighter than `or`, and anything under `not` is bracketed.
-- |
-- | The precedence argument is the strength of the context: 0 at the top or
-- | inside an `or`, 1 inside an `and`, 2 under a `not`. A connective brackets
-- | itself when the context is at least as strong as it is, which flattens
-- | `and` inside `and` and brackets `or` inside `and`.
guardAt :: Ambiguous -> Int -> Guard -> String
guardAt amb prec = case _ of
  Holds e -> exprText amb e
  Cmp op l r -> exprText amb l <> " " <> opText op <> " " <> exprText amb r
  Not g -> "not " <> guardAt amb 2 g
  And gs ->
    if null gs then "always"
    else bracket (prec >= 2) (joinWith " and " (map (guardAt amb 1) gs))
  Or gs ->
    if null gs then "never"
    else bracket (prec >= 1) (joinWith " or " (map (guardAt amb 0) gs))
  where
  bracket needed s = if needed then "(" <> s <> ")" else s

guardText :: Ambiguous -> Guard -> String
guardText amb = guardAt amb 0

-- | `Blocks` puts every outcome behind one arrow, so the three kinds line up in
-- | a single column and the eye reads down it.
outcomeArrow :: Outcome -> String
outcomeArrow = case _ of
  Move sid -> "-> " <> rawState sid
  Stay -> "-> stay"
  Refuse rid -> "-> refuse " <> rawRefusal rid

-- | The flat form put the outcome last with no arrow on the boring kinds, so a real
-- | move is the only thing on the page with an arrow in front of it.
outcomeFlat :: Outcome -> String
outcomeFlat = case _ of
  Move sid -> "-> " <> rawState sid
  Stay -> "stay"
  Refuse rid -> "refuse " <> rawRefusal rid

sourceText :: EventSource -> String
sourceText = case _ of
  FromUser -> "user"
  FromRuntime -> "runtime"

-- | Escaped, because a printer whose output cannot be read back is not a
-- | printer for an authoring format. Backslash first or it eats its own work.
quote :: String -> String
quote s = "\"" <> escape s <> "\""
  where
  escape =
    replaceAll (Pattern "\\") (Replacement "\\\\")
      >>> replaceAll (Pattern "\"") (Replacement "\\\"")

-- | Nothing at all when a label says no more than the id does. A machine whose
-- | events are called `record` and labelled `record` should not have to look at
-- | a column of `"record"`.
labelPart :: String -> String -> String
labelPart id label = if label == id then "" else quote label

-- | Printed only when the machine declares one, so a machine with no deadlines
-- | gains no line it has no use for.
clockRow :: Spec -> Array Line
clockRow spec = maybe [] (\c -> [ Cells [ "clock", clockName c ] ]) spec.clock

deadlineText :: Ambiguous -> Deadline -> String
deadlineText amb d =
  "after " <> exprText amb d.after <> " fires " <> rawEvent d.fires

-- =============================================================================
-- Facts about a spec that all three syntaxes need
-- =============================================================================

rulesFrom :: Spec -> StateId -> Array Rule
rulesFrom spec sid = filter (\r -> r.from == sid) spec.rules

deadlinesIn :: Spec -> StateId -> Array Deadline
deadlinesIn spec sid = filter (\d -> d.inState == sid) spec.deadlines

isFinal :: Spec -> StateId -> Boolean
isFinal spec sid = maybe false _.final (find (\d -> d.id == sid) spec.states)

-- | States a rule or a deadline mentions that nobody declared.
-- |
-- | The closure lint will one day reject these. Until it does they are in real
-- | artifacts, and a printer that drops them is worse than the JSON — a `Blocks`
-- | rendering iterates the declared states, so anything hanging off an
-- | undeclared one would vanish without this.
undeclaredStates :: Spec -> Array StateId
undeclaredStates spec =
  filter (\sid -> not (Set.member (rawState sid) declared))
    (distinct (map _.from spec.rules <> map _.inState spec.deadlines))
  where
  declared = Set.fromFoldable (map (\s -> rawState s.id) spec.states)

-- | First occurrence wins, order preserved.
distinct :: forall a. Ord a => Array a -> Array a
distinct xs = (foldl step { seen: Set.empty, out: [] } xs).out
  where
  step acc a =
    if Set.member a acc.seen then acc
    else { seen: Set.insert a acc.seen, out: snoc acc.out a }

-- =============================================================================
-- Syntax 1 — Blocks
-- =============================================================================

-- | Events whose behaviour is identical, collected under one line.
-- |
-- | Grouping is by the *whole case list*, not just by outcome, so two events
-- | that share a three-way guarded decision collapse together as readily as two
-- | that share a `stay`. First-occurrence order is preserved: an author's
-- | ordering is information, and re-sorting would churn diffs for no gain.
type Group =
  { events :: Array EventId
  , cases :: Array Case
  }

groupRules :: Array Rule -> Array Group
groupRules = foldl add []
  where
  add acc rule = case findIndex (\g -> g.cases == rule.cases) acc of
    Just i -> fromMaybe acc (modifyAt i (\g -> g { events = snoc g.events rule.on }) acc)
    Nothing -> snoc acc { events: [ rule.on ], cases: rule.cases }

printBlocks :: Spec -> String
printBlocks spec = finish (notesLines <> preamble <> declarations <> body)
  where
  amb = ambiguousIn spec

  -- Prose first, because it is what the author wrote first and what a reader
  -- coming to a machine cold needs before any of the declarations mean
  -- anything. `#` to the end of the line; it round-trips because `notes` is a
  -- field of `Spec` rather than something the codec discards.
  notesLines = case spec.notes of
    [] -> []
    ns -> map (\n -> if n == "" then "#" else "# " <> n) ns <> [ "" ]

  preamble = layout "" $
    [ Cells [ "machine", spec.id, quote spec.title ]
    , Cells [ "format", show (versionFor spec) ]
    ] <> clockRow spec <>
      [ Cells [ "initial", rawState spec.initial ] ]

  -- `final` and the deadlines are NOT here: they are on the state's own block
  -- header, where a reader is already looking. Saying a thing once is what keeps
  -- an authoring format from being able to contradict itself.
  declarations = concat
    [ section "states" (map stateRow spec.states)
    , section "events" (map eventRow spec.events)
    , section "config" (map configRow spec.config)
    , section "facts" (map factRow spec.facts)
    , section "refusals" (map refusalRow spec.refusals)
    , section "commands" (map commandRow spec.commands)
    ]

  section name rows =
    if null rows then [] else [ "", name ] <> layout "  " (map Cells rows)

  stateRow s = [ rawState s.id, labelPart (rawState s.id) s.label ]
  eventRow e = [ rawEvent e.id, sourceText e.source, labelPart (rawEvent e.id) e.label ]
  configRow c = [ rawConfig c.id, "= " <> literalText c.default, labelPart (rawConfig c.id) c.label ]
  factRow f = [ rawFact f.id, labelPart (rawFact f.id) f.label ]
  refusalRow r = [ rawRefusal r.id, quote r.text ]
  commandRow c = [ rawCommand c.id, labelPart (rawCommand c.id) c.label ]

  blockStates = map _.id spec.states <> undeclaredStates spec

  -- Both columns are per block, and that is a deliberate trade against looks.
  -- A global event column aligns the whole file and reads better, but it means
  -- the widest event list anywhere sets the gutter everywhere, so renaming one
  -- event repads every block and a one-word change becomes a whole-file diff.
  -- Per block caps the blast radius of any edit at the state it touches, which
  -- matters more for a format that lives in git and is edited by hand. The eye
  -- resets at each blank line anyway.
  body = concat (map block blockStates)

  eventsOf g = joinWith ", " (map rawEvent g.events)

  -- Two `layout` calls, so the commands align with each other and the rules
  -- align with each other, and neither pushes the other's columns about. They
  -- are answering different questions — *what does being here do* and *what do
  -- events do* — and lining them up as one grid implied a relationship between
  -- a command and an outcome that there is not.
  block sid =
    [ "", headerFor sid ]
      <> layout "  " (commandLines sid)
      <> layout "  " (bodyFor sid)

  commandLines sid =
    hookLine "entry" (entryCommands sid) <> hookLine "exit" (exitCommands sid)

  hookLine word = case _ of
    [] -> []
    cs -> [ Cells [ word, joinWith ", " (map rawCommand cs) ] ]

  entryCommands sid = maybe [] _.entry (find (\d -> d.id == sid) spec.states)
  exitCommands sid = maybe [] _.exit (find (\d -> d.id == sid) spec.states)

  headerFor sid =
    rawState sid <> ":" <> case modifiers sid of
      [] -> ""
      mods -> "  " <> joinWith ", " mods

  modifiers sid =
    (if isFinal spec sid then [ "final" ] else [])
      <> (if Set.member (rawState sid) declaredStates then [] else [ "!! not declared" ])
      <> map (deadlineText amb) (deadlinesIn spec sid)

  declaredStates = Set.fromFoldable (map (\s -> rawState s.id) spec.states)

  bodyFor sid = case rulesFrom spec sid of
    [] -> [ Cells [ "!! no rules — every event here silently stays" ] ]
    rules -> concat (map (groupLines (columnFor sid)) (groupRules rules))

  columnFor sid =
    fromMaybe 0 (maximum (map (\g -> String.length (eventsOf g)) (groupRules (rulesFrom spec sid))))

  -- `when` / `elif` / `else` rather than a bare `|`, because in an ordered
  -- alternation the ORDER is load-bearing and nothing else on the page says so.
  -- An `elif` with no `when` above it is a mistake a parser can catch; two
  -- unmarked guards swapped by a careless merge are silent.
  groupLines column group =
    let
      evText = pad column (eventsOf group)
    in
      case group.cases of
        [] -> [ Cells [ evText, "", "!! no cases" ] ]
        [ c ] | c.when == Nothing -> [ Cells [ evText, "", outcomeArrow c.outcome ] ]
        cs -> mapWithIndex
          ( \i c -> Cells
              [ if i == 0 then evText else ""
              , case c.when of
                  Nothing -> "else"
                  Just g -> (if i == 0 then "when " else "elif ") <> guardText amb g
              , outcomeArrow c.outcome
              ]
          )
          cs

-- =============================================================================
-- Syntax 2 — Rules
-- =============================================================================

-- =============================================================================
-- Syntax 3 — Table
-- =============================================================================

-- | A guarded or duplicated pair, lifted out of the grid.
type Note =
  { index :: Int
  , from :: StateId
  , on :: EventId
  , rules :: Array Rule
  }

type Cell =
  { text :: String
  , note :: Maybe Note
  }

printTable :: Spec -> String
printTable spec = finish (preamble <> declarations <> grid <> legend <> notesSection <> deadlineSection <> offGrid)
  where
  amb = ambiguousIn spec

  preamble = layout "" $
    [ Cells [ "machine", spec.id ]
    , Cells [ "title", quote spec.title ]
    , Cells [ "format", show (versionFor spec) ]
    ] <> clockRow spec <>
      [ Cells [ "initial", rawState spec.initial ] ]

  -- Labels are unquoted here, unlike the other two syntaxes. In a column table
  -- the label is always the last field on the line, so a space in it is
  -- unambiguous and a pair of quotes is pure noise.
  declarations = concat
    [ table [ "STATES", "LABEL", "ON ENTRY", "ON EXIT" ]
        ( map
            ( \s ->
                [ "  " <> rawState s.id
                , plainLabel (rawState s.id) s.label
                , joinWith ", " (map rawCommand s.entry)
                , joinWith ", " (map rawCommand s.exit)
                ]
            )
            spec.states
        )
    , table [ "EVENTS", "SOURCE", "LABEL" ]
        (map (\e -> [ "  " <> rawEvent e.id, sourceText e.source, plainLabel (rawEvent e.id) e.label ]) spec.events)
    , table [ "CONFIG", "DEFAULT", "LABEL" ]
        (map (\c -> [ "  " <> rawConfig c.id, literalText c.default, plainLabel (rawConfig c.id) c.label ]) spec.config)
    , table [ "FACTS", "MEANING" ]
        (map (\f -> [ "  " <> rawFact f.id, plainLabel (rawFact f.id) f.label ]) spec.facts)
    , table [ "REFUSALS", "MEANING" ]
        (map (\r -> [ "  " <> rawRefusal r.id, r.text ]) spec.refusals)
    , table [ "COMMANDS", "MEANING" ]
        (map (\c -> [ "  " <> rawCommand c.id, plainLabel (rawCommand c.id) c.label ]) spec.commands)
    ]

  plainLabel id label = if label == id then "" else label

  table header rows =
    if null rows then [] else [ "" ] <> layout "" (map Cells ([ header ] <> rows))

  -- --- the grid -------------------------------------------------------------

  events = map _.id spec.events
  states = map _.id spec.states

  -- One pass in reading order, numbering footnotes as it goes, so `[1]` is the
  -- first thing an eye meets going left-to-right and top-to-bottom.
  numbered = foldl row { counter: 1, rows: [] } states
    where
    row acc sid =
      let
        stepped = foldl (cellFor sid) { counter: acc.counter, cells: [] } events
      in
        { counter: stepped.counter, rows: snoc acc.rows { sid, cells: stepped.cells } }

  cellFor sid acc eid =
    let
      matching = filter (\r -> r.from == sid && r.on == eid) spec.rules
      timed = not (null (filter (\d -> d.inState == sid && d.fires == eid) spec.deadlines))
      mark text = (if timed then "@" else "") <> text
    in
      case matching of
        -- No rule at all. The runtime answers `Stay`, which is indistinguishable
        -- from a declared `stay` at run time and completely different at design
        -- time; the grid is the one place that difference is visible at a glance.
        [] -> { counter: acc.counter, cells: snoc acc.cells { text: mark "????", note: Nothing } }
        [ r ] | isSimple r ->
          { counter: acc.counter
          , cells: snoc acc.cells { text: mark (simpleText r), note: Nothing }
          }
        rules ->
          { counter: acc.counter + 1
          , cells: snoc acc.cells
              { text: mark ("[" <> show acc.counter <> "]")
              , note: Just { index: acc.counter, from: sid, on: eid, rules }
              }
          }

  isSimple r = case r.cases of
    [ c ] -> c.when == Nothing
    _ -> false

  simpleText r = case r.cases of
    [ c ] -> case c.outcome of
      Move sid -> rawState sid
      Stay -> "."
      Refuse rid -> "!" <> rawRefusal rid
    _ -> "!!"

  gridRows = numbered.rows

  notes = concat (map (\r -> concat (map (\c -> maybe [] (\n -> [ n ]) c.note) r.cells)) gridRows)

  grid =
    if null states || null events then []
    else [ "", "GRID   rows are states, columns are events" ]
      <> layout "" (map Cells ([ [ "" ] <> map rawEvent events ] <> map gridLine gridRows))

  gridLine r = [ "  " <> initialMark r.sid <> rawState r.sid <> finalMark r.sid ] <> map _.text r.cells

  initialMark sid = if sid == spec.initial then "> " else "  "
  finalMark sid = if isFinal spec sid then " *" else ""

  -- --- the legend -----------------------------------------------------------

  -- Only the marks actually used. A legend explaining a symbol the reader cannot
  -- see is the kind of noise that makes people stop reading legends.
  legend =
    case filter (\e -> e.used) legendEntries of
      [] -> []
      used -> [ "", "KEY" ] <> layout "  " (map (\e -> Cells [ e.mark, e.meaning ]) used)

  legendEntries =
    [ { mark: ">", meaning: "the initial state", used: true }
    , { mark: "*", meaning: "a final state", used: anyFinal }
    , { mark: ".", meaning: "stay — the event is declared here and does nothing", used: anyCell (contains ".") }
    , { mark: "!tag", meaning: "refuse with that tag — see REFUSALS", used: anyCell (contains "!") }
    , { mark: "[n]", meaning: "guarded, or declared twice — see CASES", used: not (null notes) }
    , { mark: "@", meaning: "also fires by itself — see DEADLINES", used: anyCell (contains "@") }
    , { mark: "????", meaning: "!! NO RULE for this pair — the machine is not total", used: anyCell (contains "????") }
    ]

  anyFinal = not (null (filter _.final spec.states))

  anyCell p = not (null (filter p (concat (map (\r -> map _.text r.cells) gridRows))))

  contains needle text = String.contains (Pattern needle) text

  -- --- the escapes ----------------------------------------------------------

  notesSection =
    if null notes then []
    else [ "", "CASES" ] <> layout "         " (concat (map noteLines notes))

  noteLines n =
    [ Raw ("  [" <> show n.index <> "]  " <> rawState n.from <> " on " <> rawEvent n.on <> duplicateWarning n) ]
      <> concat (map caseLines n.rules)

  duplicateWarning n =
    if length n.rules > 1 then "   !! declared " <> show (length n.rules) <> " times; only the first is reachable"
    else ""

  caseLines rule = case rule.cases of
    [] -> [ Cells [ "!! no cases" ] ]
    cs -> mapWithIndex
      ( \i c -> Cells
          [ case c.when of
              Just g -> (if i == 0 then "when " else "elif ") <> guardText amb g
              Nothing -> "else"
          , outcomeFlat c.outcome
          ]
      )
      cs

  deadlineSection =
    if null spec.deadlines then []
    else [ "", "DEADLINES" ]
      <> layout "" (map (\d -> Cells [ "  " <> rawState d.inState, "after " <> exprText amb d.after, "fires " <> rawEvent d.fires ]) spec.deadlines)

  -- A rule naming a state or an event that nobody declared has no cell to live
  -- in. The grid cannot grow a row for it without pretending it was declared, so
  -- it is printed underneath instead — losing it would make this syntax lossy
  -- and therefore useless as an authoring format.
  offGrid = case filter offTheGrid spec.rules of
    [] -> []
    rogue -> [ "", "!! OFF-GRID — these rules name a state or event that is not declared" ]
      <> layout "  " (map (\r -> Cells [ rawState r.from, rawEvent r.on, summarise r ]) rogue)

  offTheGrid r =
    not (Set.member (rawState r.from) declaredStates)
      || not (Set.member (rawEvent r.on) declaredEvents)

  declaredStates = Set.fromFoldable (map (\s -> rawState s.id) spec.states)
  declaredEvents = Set.fromFoldable (map (\e -> rawEvent e.id) spec.events)

  summarise r = joinWith " ; " (map caseSummary r.cases)

  caseSummary c =
    maybe "" (\g -> "when " <> guardText amb g <> " ") c.when <> outcomeFlat c.outcome
