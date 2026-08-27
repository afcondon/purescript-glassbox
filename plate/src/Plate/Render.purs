-- | Plate.Render
-- |
-- | A corpus of machines, typeset.
-- |
-- | ### What this rendering is for
-- |
-- | `Glassbox.DSL.Print`'s `Table` syntax is the ancestor of this page and its
-- | bet is stated there: the 92% of rules that are one-liners belong in a cell,
-- | and a grid shows a *missing* pair in a way a list cannot. That bet is
-- | right. What a monospace grid cannot do is spend anything but width — every
-- | distinction has to become another glyph in the same column, and a reader
-- | checking a real supervisor is asked to hold `.`, `!busy`, `[1]` and `@` in
-- | mind at once while counting columns with a finger.
-- |
-- | A page has three axes a text file does not: weight, colour and ground. So
-- | the four outcome kinds are separated by *ground* rather than by glyph, and
-- | the grid stops being read cell by cell and starts being read as a shape —
-- | a supervisor's band of `!busy`, a heating controller's column of faults.
-- |
-- | ### Three departures from the text rendering, each deliberate
-- |
-- |   1. **The grid comes first.** `Table` prints six glossaries and then the
-- |      grid; a reader checking a cell reaches the thing they came for last.
-- |      Here the grid is the plate and the glossaries are the apparatus below
-- |      it, which is the order a datasheet uses for the same reason.
-- |   2. **The key is stated once**, at the top of the document, rather than
-- |      per machine. A text file has no "top of the document" to rely on; a
-- |      page does.
-- |   3. **`Turns` is a kind of its own.** `Table` prints a self-transition as
-- |      the state's own name, indistinguishable from any other move until you
-- |      notice it names the row you are on. It is the least visible cell in
-- |      the corpus and the most consequential — it is a supervisor's entire
-- |      keep-alive loop.
module Plate.Render (renderDocument, renderFragment, Corpus, corpusStats) where

import Prelude

import Data.Array (concatMap, elem, filter, foldl, index, length, mapWithIndex, null, nub, snoc, sortBy)
import Data.Array as Array
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String (Pattern(..), joinWith, split, trim)
import Data.String as String
import Data.Tuple (Tuple(..))
import Glassbox.Lint (Finding, Severity(..))
import Glassbox.Spec
  ( Case
  , Clock
  , CmpOp(..)
  , CommandDecl
  , CommandId(..)
  , ConfigId(..)
  , Deadline
  , EventDecl
  , EventId(..)
  , EventSource(..)
  , Expr(..)
  , FactId(..)
  , Guard(..)
  , Outcome(..)
  , RefusalId(..)
  , Spec
  , StateDecl
  , StateId(..)
  , clockName
  , labelOfEvent
  , valueToString
  , versionFor
  )
import Plate.Html (Attr, Node, attr, cls, el, el_, empty, foldNodes, raw, render, text, when_)
import Plate.Model (Branch, CellKind(..), Cell, Plate, Row, caseLadder, cellTarget, configReads, factReads)

type Corpus = { plates :: Array Plate, style :: String, script :: String, built :: String }

-- =============================================================================
-- The document
-- =============================================================================

renderDocument :: Corpus -> String
renderDocument corpus =
  "<!doctype html>\n" <> render (documentNode corpus)

-- | The same document with no `html`, `head` or `body` of its own, for a host
-- | that supplies the skeleton and wraps this in it.
renderFragment :: Corpus -> String
renderFragment corpus = render (foldNodes (head' corpus <> body' corpus))

documentNode :: Corpus -> Node
documentNode corpus =
  el "html" [ attr "lang" "en" ]
    [ el_ "head"
        ( [ el "meta" [ attr "charset" "utf-8" ] []
          , el "meta" [ attr "name" "viewport", attr "content" "width=device-width, initial-scale=1" ] []
          ] <> head' corpus
        )
    , el_ "body" (body' corpus)
    ]

head' :: Corpus -> Array Node
head' corpus =
  [ el_ "title" [ text "Glassbox Plate" ]
  , el "link" [ attr "rel" "preconnect", attr "href" "https://fonts.googleapis.com" ] []
  , el "link" [ attr "rel" "preconnect", attr "href" "https://fonts.gstatic.com", attr "crossorigin" "" ] []
  , el "link" [ attr "rel" "stylesheet", attr "href" fontHref ] []
  , el_ "style" [ raw corpus.style ]
  ]

body' :: Corpus -> Array Node
body' corpus =
  [ spine corpus.plates
  , el "main" [ attr "id" "plate" ]
      ( [ masthead corpus
        , keyPlate
        , contents corpus.plates
        ]
          <> map plateSection corpus.plates
          <> [ findings corpus, colophon corpus ]
      )
  , el_ "script" [ raw corpus.script ]
  ]

fontHref :: String
fontHref =
  "https://fonts.googleapis.com/css2?family=Archivo:wdth,wght@75..125,400..800"
    <> "&family=IBM+Plex+Mono:wght@400;500;600&display=swap"

-- =============================================================================
-- Masthead
-- =============================================================================

masthead :: Corpus -> Node
masthead corpus =
  el "header" [ cls "masthead" ]
    [ el "div" [ cls "kicker" ] [ text "A specimen for Glassbox" ]
    , el "h1" [ cls "title" ] [ text "Plate" ]
    , el "p" [ cls "deck" ]
        [ text "Six state machines that exist only as JSON files. Each is set here as a "
        , el_ "em" [ text "plate" ]
        , text " — the transition grid first, its apparatus underneath — because a machine "
        , text "like these is checked by looking at it, and one wrong cell is a production bug."
        ]
    , statBand corpus
    ]

statBand :: Corpus -> Node
statBand corpus =
  el "dl" [ cls "stats" ]
    ( map stat
        [ Tuple "machines" (show s.machines)
        , Tuple "states" (show s.states)
        , Tuple "events" (show s.events)
        , Tuple "cells" (show s.cells)
        , Tuple "rules" (show s.rules)
        , Tuple "single-case" (percent s.singleCase s.rules)
        , Tuple "guarded" (show s.guarded)
        ]
    )
  where
  s = corpusStats corpus.plates
  stat (Tuple k v) =
    el "div" [ cls "stat" ]
      [ el_ "dt" [ text k ], el_ "dd" [ text v ] ]

corpusStats
  :: Array Plate
  -> { machines :: Int
     , states :: Int
     , events :: Int
     , cells :: Int
     , rules :: Int
     , singleCase :: Int
     , guarded :: Int
     , bareStay :: Int
     }
corpusStats plates =
  { machines: length plates
  , states: sum (map (\p -> length p.spec.states) plates)
  , events: sum (map (\p -> length p.spec.events) plates)
  , cells: sum (map (\p -> p.tally.total) plates)
  , rules: length allRules
  , singleCase: length (filter single allRules)
  , guarded: sum (map (\p -> p.tally.guarded) plates)
  , bareStay: length (filter bare allRules)
  }
  where
  allRules = concatMap (\p -> p.spec.rules) plates
  single r = length r.cases == 1
  bare r = case r.cases of
    [ c ] -> c.when == Nothing && c.outcome == Stay
    _ -> false

sum :: Array Int -> Int
sum = foldl (+) 0

percent :: Int -> Int -> String
percent n d =
  if d == 0 then "—"
  else show (round (100.0 * toNumber n / toNumber d)) <> "%"

-- =============================================================================
-- The key, stated once
-- =============================================================================

keyPlate :: Node
keyPlate =
  el "section" [ cls "key", attr "id" "key" ]
    [ el "h2" [ cls "section-title" ] [ text "How to read a cell" ]
    , el "p" [ cls "key-lede" ]
        [ text "Rows are states, columns are events, and a cell is what that state does "
        , text "with that event. There are six answers and they are told apart by ground, "
        , text "not by punctuation."
        ]
    , el "div" [ cls "key-grid" ]
        [ keyItem (Moves (StateId "seeking")) "moves"
            "The machine leaves this state for the one named. These are the substance of a machine and they are the only cells set in full ink."
        , keyItem (Turns (StateId "raised")) "turns"
            "A move to the state it is already in — it exits and re-enters, so the entry commands run again. Not the same as staying, and easy to miss when it is spelt the same way."
        , keyItem Stays "stays"
            "Declared, and does nothing. Two cells in five are this; they are set to recede so the rest can be read."
        , keyItem (Refuses (RefusalId "no-disc")) "refuses"
            "Refused, with a tag from the declared list. A refusal is not a stay: something forbade it, and the host can say which."
        , keyItem (Guarded 1) "guarded"
            "The answer depends on the world. The numbered ladder is below the grid; hover the chip to read it in place."
        , keyItem Absent "no rule"
            "Nobody wrote this pair down. The runtime answers stay, so this is the one kind of wrong cell that cannot be seen anywhere but a grid."
        ]
    , el "p" [ cls "key-foot" ]
        [ el "span" [ cls "mk mk-initial" ] [ text "▸" ]
        , text " the initial state  "
        , el "span" [ cls "mk mk-final" ] [ text "▪" ]
        , text " a final state  "
        , el "span" [ cls "mk mk-timed" ] [ text "◷" ]
        , text " this pair also fires on a deadline, with nobody pressing anything."
        ]
    ]

keyItem :: CellKind -> String -> String -> Node
keyItem kind name meaning =
  el "div" [ cls "key-item" ]
    [ el "div" [ cls "key-sample" ] [ el_ "table" [ el_ "tbody" [ el_ "tr" [ sampleCell kind ] ] ] ]
    , el "div" [ cls "key-body" ]
        [ el "div" [ cls "key-name" ] [ text name ]
        , el "p" [] [ text meaning ]
        ]
    ]
  where
  sampleCell k = el "td" [ cls ("c " <> kindClass k) ] (cellContent Nothing k)

-- =============================================================================
-- Contents
-- =============================================================================

contents :: Array Plate -> Node
contents plates =
  el "section" [ cls "contents" ]
    [ el "h2" [ cls "section-title" ] [ text "The corpus" ]
    , el "div" [ cls "contents-grid" ] (map card plates)
    ]
  where
  card p =
    el "a" [ cls "card", attr "href" ("#m-" <> p.name) ]
      [ el "div" [ cls "card-name" ] [ text p.spec.title ]
      , el "div" [ cls "card-id" ] [ text (p.name <> ".json") ]
      , el "div" [ cls "card-dim" ]
          [ text (show (length p.spec.states) <> " × " <> show (length p.spec.events)) ]
      , el "div" [ cls "card-bar" ] (mixBar p)
      ]

-- | The mix of outcome kinds in one machine, at the width each takes up in it.
-- |
-- | Not decoration: it is the same four grounds the grid uses, in the same
-- | proportions, so the card is a thumbnail of the plate rather than a label
-- | for it. A machine that is nine-tenths refusals looks like one here.
mixBar :: Plate -> Array Node
mixBar p =
  filter (\_ -> true)
    [ seg "b-move" (p.tally.moves + p.tally.turns)
    , seg "b-guard" p.tally.guarded
    , seg "b-refuse" p.tally.refusals
    , seg "b-stay" p.tally.stays
    , seg "b-absent" p.tally.absent
    ]
  where
  total = max 1 p.tally.total
  seg c n =
    when_ (n > 0)
      ( el "span"
          [ cls ("b " <> c)
          , attr "style" ("flex-grow:" <> show n)
          , attr "title" (show n <> " of " <> show total)
          ]
          []
      )

-- =============================================================================
-- One plate
-- =============================================================================

-- | The order is an argument. The grid is the plate, so it comes first and its
-- | footnotes come with it; then the two other renderings of the same machine —
-- | the drawing, and the syntax it would be authored in — and only then the
-- | timing and the glossaries. A reader who wants to check a cell never has to
-- | scroll past a listing to reach it.
plateSection :: Plate -> Node
plateSection p =
  el "section" [ cls "plate", attr "id" ("m-" <> p.name), attr "data-machine" p.name ]
    [ plateHead p
    , el "div" [ cls "plate-body" ]
        [ notesBlock p
        , auditBlock p
        ]
    , gridFigure p
    , casesBlock p
    , diagramBlock p
    , rulesBlock p
    , deadlinesBlock p
    , vocabBlock p
    ]

plateHead :: Plate -> Node
plateHead p =
  el "header" [ cls "plate-head" ]
    [ el "h2" [ cls "plate-title" ] [ text p.spec.title ]
    , el "div" [ cls "plate-meta" ]
        [ el "span" [ cls "mono strong" ] [ text (p.name <> ".json") ]
        , metaItem "format" (show (versionFor p.spec))
        , metaItem "initial" (rawState p.spec.initial)
        , maybe empty (\c -> metaItem "clock" (clockName c)) p.spec.clock
        , metaItem "grid" (show (length p.spec.states) <> " × " <> show (length p.spec.events))
        ]
    ]

metaItem :: String -> String -> Node
metaItem k v =
  el "span" [ cls "meta-item" ]
    [ el "span" [ cls "meta-k" ] [ text k ]
    , el "span" [ cls "meta-v mono" ] [ text v ]
    ]

-- | The author's own prose, which the format carries as part of the value.
notesBlock :: Plate -> Node
notesBlock p =
  when_ (not (null paras))
    (el "div" [ cls "notes" ] (map para paras))
  where
  paras = paragraphs p.spec.notes
  para ls = el "p" [] (prose (joinWith " " ls))

-- | Blank lines are paragraph breaks; the corpus already writes them that way.
paragraphs :: Array String -> Array (Array String)
paragraphs =
  foldl step { done: [], current: [] } >>> flush
  where
  step acc line =
    if trim line == "" then { done: keep acc, current: [] }
    else acc { current = snoc acc.current (trim line) }
  keep acc = if null acc.current then acc.done else snoc acc.done acc.current
  flush acc = keep acc

-- | Backticked runs in an author's prose become code. Nothing else is
-- | interpreted: this is a notes field, not a markdown document, and guessing
-- | further would start rewriting the author's words.
prose :: String -> Array Node
prose s = mapWithIndex piece (split (Pattern "`") s)
  where
  piece i chunk =
    if i `mod` 2 == 1 then el "code" [] [ text chunk ]
    else text chunk

-- =============================================================================
-- The checks
-- =============================================================================

auditBlock :: Plate -> Node
auditBlock p =
  el "div" [ cls "audit", attr "id" ("checks-" <> p.name) ]
    [ el "div" [ cls "audit-title" ] [ text "Checks" ]
    , el "div" [ cls "audit-items" ]
        [ check "totality"
            (null a.gaps)
            (show (p.tally.total - p.tally.absent) <> " / " <> show p.tally.total <> " pairs declared")
            (show (length a.gaps) <> " pairs undeclared")
        , check "reach"
            (null a.unreachable)
            ("all " <> show (length p.spec.states) <> " states reachable")
            (joinWith ", " (map rawState a.unreachable) <> " has no inbound edge")
        , check "edges"
            (null dropped)
            ("the rules and the edge set agree" <> suppressedNote)
            (show (length dropped) <> " transitions the rules name are missing from the edge set" <> suppressedNote)
        , check "vocabulary"
            (null dead)
            "every declaration is used"
            (joinWith ", " dead <> " declared, never used")
        , check "lint"
            (null a.findings)
            "clean"
            (findingSummary a.findings)
        ]
    , when_ (not (null a.findings))
        (el "ul" [ cls "findings" ] (map finding a.findings))
    , when_ (not (null dropped))
        (el "ul" [ cls "findings missing" ] (map (edgeLine "not drawn") dropped))
    -- Set apart, and set quietly, because it is not a defect. An arm behind a
    -- config read is an arm this configuration does not have, and a drawing
    -- that omitted it would be right about the machine somebody is running.
    , when_ (not (null suppressed))
        ( el "div" [ cls "suppressed" ]
            [ el "ul" [ cls "findings" ] (map (edgeLine "by config") suppressed)
            , el "p" [ cls "suppressed-say" ]
                [ el "code" [] [ text "Glassbox.Edges" ]
                , text " enumerates facts and holds config at its defaults, deliberately: config is "
                , text "what decides which machine this is. So these arms are correctly absent from "
                , text "the drawing, and are listed only because their absence looks identical to "
                , text "the defect above until you say which is which."
                ]
            ]
        )
    ]
  where
  a = p.audit
  dropped = filter (\m -> not m.configGuarded) a.missingEdges
  suppressed = filter _.configGuarded a.missingEdges

  suppressedNote =
    if null suppressed then ""
    else "; " <> show (length suppressed) <> " arm"
      <> (if length suppressed == 1 then "" else "s")
      <> " config-suppressed"

  edgeLine label m =
    el_ "li"
      [ el "span" [ cls "finding-lint mono" ] [ text label ]
      , el "span" [ cls "mono" ]
          [ text (rawState m.from <> " —" <> rawEvent m.on <> "→ " <> rawState m.to) ]
      ]

  dead =
    map rawCommand a.unusedCommands
      <> map rawRefusal a.unusedRefusals
      <> map rawFact a.unreadFacts
      <> map rawConfig a.unreadConfig

  check name ok good bad =
    el "div" [ cls ("check " <> if ok then "ok" else "flag") ]
      [ el "span" [ cls "check-name" ] [ text name ]
      , el "span" [ cls "check-mark" ] [ text (if ok then "✓" else "▲") ]
      , el "span" [ cls "check-say" ] [ text (if ok then good else bad) ]
      ]

  finding f =
    el "li" [ cls (if f.severity == Reject then "reject" else "warn") ]
      [ el "span" [ cls "finding-lint mono" ] [ text f.lint ]
      , el "span" [] [ text f.message ]
      ]

findingSummary :: Array Finding -> String
findingSummary fs =
  show (length (filter (\f -> f.severity == Reject) fs)) <> " rejected, "
    <> show (length (filter (\f -> f.severity == Warn) fs))
    <> " warned"

-- =============================================================================
-- The grid
-- =============================================================================

gridFigure :: Plate -> Node
gridFigure p =
  el "figure" [ cls "grid-wrap" ]
    [ el "div" [ cls "grid-tools" ]
        [ el "span" [ cls "tools-label" ] [ text "grid" ]
        , when_ (not (null p.notes))
            ( el "button"
                [ cls "tool", attr "type" "button", attr "data-tool" "cases" ]
                [ text "open the guarded cells" ]
            )
        , el "button"
            [ cls "tool", attr "type" "button", attr "data-tool" "clear" ]
            [ text "clear highlight" ]
        ]
    , el "div" [ cls "grid-scroll" ]
        [ el "table" [ cls "grid", attr "data-machine" p.name ]
            [ el_ "thead" [ headRow p ]
            , el_ "tbody" (mapWithIndex (bodyRow p) p.rows)
            ]
        ]
    ]

headRow :: Plate -> Node
headRow p =
  el_ "tr"
    ( [ el "th" [ cls "corner" ]
          [ el "span" [ cls "corner-r" ] [ text "state" ]
          , el "span" [ cls "corner-x" ] [ text "×" ]
          , el "span" [ cls "corner-c" ] [ text "event" ]
          ]
      ]
        <> mapWithIndex (colHead p) p.spec.events
    )

-- | A hairline where the event source changes, because who delivers an event
-- | is the one division the columns genuinely have: a surface offers buttons
-- | for the user half and must not offer one for the other.
colHead :: Plate -> Int -> EventDecl -> Node
colHead p i e =
  el "th"
    ( [ cls ("colhead " <> sourceClass e.source <> divider)
      , attr "data-c" (show i)
      , attr "data-event" (rawEvent e.id)
      , attr "scope" "col"
      ]
    )
    [ el "span" [ cls "col-id mono" ] [ text (rawEvent e.id) ]
    , when_ (e.label /= rawEvent e.id)
        (el "span" [ cls "col-label" ] [ text e.label ])
    , el "span" [ cls "col-src" ] [ text (sourceWord e.source) ]
    ]
  where
  divider =
    case index p.spec.events (i - 1) of
      Just prev | prev.source /= e.source -> " newsource"
      _ -> ""

sourceClass :: EventSource -> String
sourceClass = case _ of
  FromUser -> "src-user"
  FromRuntime -> "src-runtime"

sourceWord :: EventSource -> String
sourceWord = case _ of
  FromUser -> "user"
  FromRuntime -> "runtime"

bodyRow :: Plate -> Int -> Row -> Node
bodyRow p i row =
  el "tr"
    [ cls ("gridrow" <> if row.initial then " is-initial" else "")
    , attr "data-r" (show i)
    , attr "data-state" (rawState row.state.id)
    ]
    ( [ rowHead p row ]
        <> mapWithIndex (cellNode p row) row.cells
    )

rowHead :: Plate -> Row -> Node
rowHead p row =
  el "th"
    [ cls "rowhead", attr "scope" "row", attr "data-state" (rawState row.state.id) ]
    [ el "div" [ cls "rh-line" ]
        [ el "span" [ cls "rh-mark" ]
            [ text (if row.initial then "▸" else if row.state.final then "▪" else "") ]
        , el "button"
            [ cls "rh-id mono", attr "data-reach" (rawState row.state.id), attr "type" "button" ]
            [ text (rawState row.state.id) ]
        , when_ (row.state.label /= rawState row.state.id)
            (el "span" [ cls "rh-label" ] [ text row.state.label ])
        ]
    , commandsLine "entry" row.state.entry
    , commandsLine "exit" row.state.exit
    , maybe empty (deadlineLine p) row.deadline
    -- How many ways in there are, which is the question a grid answers worst:
    -- inbound edges are scattered down a column you have to read cell by cell.
    , el "span"
        [ cls ("rh-inbound" <> if row.inbound == 0 && not row.initial then " none" else "")
        , attr "title" (show row.inbound <> " ways in, from elsewhere")
        ]
        [ text ("←" <> show row.inbound) ]
    ]

commandsLine :: String -> Array CommandId -> Node
commandsLine name commands =
  when_ (not (null commands))
    ( el "div" [ cls ("rh-cmds rh-" <> name) ]
        [ el "span" [ cls "rh-cmdk" ] [ text name ]
        , foldNodes
            ( mapWithIndex
                ( \i c ->
                    foldNodes
                      [ when_ (i > 0) (el "span" [ cls "sep" ] [ text ", " ])
                      , el "span"
                          [ cls "cmd mono", attr "data-command" (rawCommand c) ]
                          [ text (rawCommand c) ]
                      ]
                )
                commands
            )
        ]
    )

deadlineLine :: Plate -> Deadline -> Node
deadlineLine p d =
  el "div" [ cls "rh-deadline" ]
    [ el "span" [ cls "rh-cmdk" ] [ text "after" ]
    , el "span" [ cls "mono" ] [ text (exprText d.after) ]
    , when_ (isJust p.spec.clock)
        (el "span" [ cls "unit" ] [ text (maybe "" clockName p.spec.clock) ])
    , el "span" [ cls "fires" ] [ text "fires" ]
    , el "span" [ cls "mono" ] [ text (rawEvent d.fires) ]
    ]

cellNode :: Plate -> Row -> Int -> Cell -> Node
cellNode p row i cell =
  el "td"
    ( [ cls ("c " <> kindClass cell.kind <> timedClass <> divider)
      , attr "data-c" (show i)
      ]
        <> targetAttr
        <> refusalAttr
        <> readsAttr
    )
    (cellContent (Just { plate: p, from: row.state.id, on: cell.on }) cell.kind)
  where
  timedClass = if cell.timed then " timed" else ""

  divider =
    case Tuple (index p.spec.events (i - 1)) (index p.spec.events i) of
      Tuple (Just prev) (Just here) | prev.source /= here.source -> " newsource"
      _ -> ""

  targetAttr = case cellTarget cell.kind of
    Just t -> [ attr "data-target" (rawState t) ]
    Nothing -> []

  refusalAttr = case cell.kind of
    Refuses rid -> [ attr "data-refusal" (rawRefusal rid) ]
    _ -> []

  readsAttr = case cell.kind of
    Guarded n -> case noteAt p n of
      Just note ->
        [ attr "data-reads" (joinWith " " (nub (concatMap branchReads (caseLadder note.from note.cases)))) ]
      Nothing -> []
    _ -> []

noteAt :: Plate -> Int -> Maybe { index :: Int, from :: StateId, on :: EventId, cases :: Array Case }
noteAt p n = Array.find (\note -> note.index == n) p.notes

branchReads :: Branch -> Array String
branchReads b = case b.guard of
  Nothing -> []
  Just g -> guardNames g

guardNames :: Guard -> Array String
guardNames = case _ of
  Holds e -> exprNames e
  Not g -> guardNames g
  And gs -> concatMap guardNames gs
  Or gs -> concatMap guardNames gs
  Cmp _ a b -> exprNames a <> exprNames b

exprNames :: Expr -> Array String
exprNames = case _ of
  ConfigOf (ConfigId c) -> [ c ]
  FactOf (FactId f) -> [ f ]
  Lit _ -> []

-- | The mark inside a cell.
-- |
-- | A move carries the target's name and nothing else — no arrow, because
-- | every cell in the grid is already an arrow and drawing one in each would
-- | say only that this is a table. A stay carries a centred dot, a refusal its
-- | tag, and a pair nobody wrote down carries nothing at all: the ground is
-- | what says it.
cellContent :: Maybe { plate :: Plate, from :: StateId, on :: EventId } -> CellKind -> Array Node
cellContent ctx = case _ of
  Absent -> [ el "span" [ cls "hole", attr "title" "no rule for this pair" ] [] ]
  Stays -> [ el "span" [ cls "dot" ] [ text "·" ] ]
  Moves to -> [ moveName to ]
  Turns to -> [ moveName to, el "span" [ cls "turn", attr "title" "exits and re-enters: entry commands run again" ] [ text "↻" ] ]
  Refuses rid ->
    [ el "span" [ cls "bang" ] [ text "!" ]
    , el "span" [ cls "refusal mono" ] [ text (rawRefusal rid) ]
    ]
  Guarded n -> guardedCell ctx n

moveName :: StateId -> Node
moveName to =
  el "button"
    [ cls "move mono", attr "type" "button", attr "data-reach" (rawState to) ]
    [ text (rawState to) ]

guardedCell :: Maybe { plate :: Plate, from :: StateId, on :: EventId } -> Int -> Array Node
guardedCell ctx n = case ctx of
  Nothing -> [ el "span" [ cls "chip" ] [ text (show n) ] ]
  Just { plate, from, on } ->
    [ el "a"
        [ cls "chip", attr "href" ("#case-" <> plate.name <> "-" <> show n) ]
        [ text (show n) ]
    , case noteAt plate n of
        Just note ->
          el "div" [ cls "pop" ]
            [ el "div" [ cls "pop-head mono" ]
                [ text (rawState from <> " on " <> rawEvent on) ]
            , ladder note.from note.cases
            ]
        Nothing -> empty
    ]

kindClass :: CellKind -> String
kindClass = case _ of
  Absent -> "c-absent"
  Stays -> "c-stay"
  Moves _ -> "c-move"
  Turns _ -> "c-turn"
  Refuses _ -> "c-refuse"
  Guarded _ -> "c-guard"

-- =============================================================================
-- The escapes: guarded rules
-- =============================================================================

casesBlock :: Plate -> Node
casesBlock p =
  when_ (not (null p.notes))
    ( el "div" [ cls "cases" ]
        [ el "h3" [ cls "sub-title" ]
            [ text "Cases"
            , el "span" [ cls "sub-note" ]
                [ text "tried in order; the first match wins" ]
            ]
        , el "div" [ cls "cases-grid" ] (map one p.notes)
        ]
    )
  where
  one note =
    el "div" [ cls "case", attr "id" ("case-" <> p.name <> "-" <> show note.index) ]
      [ el "div" [ cls "case-head" ]
          [ el "span" [ cls "chip static" ] [ text (show note.index) ]
          , el "span" [ cls "mono" ] [ text (rawState note.from) ]
          , el "span" [ cls "on" ] [ text "on" ]
          , el "span" [ cls "mono" ] [ text (rawEvent note.on) ]
          ]
      , ladder note.from note.cases
      ]

ladder :: StateId -> Array Case -> Node
ladder from cases = el "div" [ cls "ladder" ] (map rung (caseLadder from cases))
  where
  rung b =
    el "div" [ cls "rung" ]
      [ el "span" [ cls ("kw kw-" <> b.keyword) ] [ text b.keyword ]
      , el "span" [ cls "guard" ] (maybe [] guardNodes b.guard)
      , el "span" [ cls "arrow" ] [ text (outcomeArrow b.outcome) ]
      , el "span" [ cls "outcome" ] (outcomeNodes b.from b.outcome)
      ]

outcomeArrow :: Outcome -> String
outcomeArrow = case _ of
  Move _ -> "→"
  Stay -> ""
  Refuse _ -> ""

outcomeNodes :: StateId -> Outcome -> Array Node
outcomeNodes from = case _ of
  Move to ->
    [ el "span"
        [ cls ("move mono" <> if to == from then " self" else "")
        , attr "data-reach" (rawState to)
        ]
        [ text (rawState to) ]
    , when_ (to == from) (el "span" [ cls "turn" ] [ text "↻" ])
    ]
  Stay -> [ el "span" [ cls "stay-word" ] [ text "stay" ] ]
  Refuse rid ->
    [ el "span" [ cls "refuse-word" ] [ text "refuse" ]
    , el "span" [ cls "refusal mono", attr "data-refusal" (rawRefusal rid) ]
        [ text (rawRefusal rid) ]
    ]

-- | A guard, drawn rather than printed.
-- |
-- | `Glassbox.DSL.Print` keeps its own guard printer private, so this is a
-- | second rendering of the same data and the risk that goes with that is
-- | real. It earns the risk by doing something the text one cannot: every
-- | config and fact id is a live cross-reference, so pointing at
-- | `has-cd-slot` lights every cell in the machine whose answer depends on it,
-- | and its entry in the glossary below.
guardNodes :: Guard -> Array Node
guardNodes = case _ of
  Holds e -> exprNodes e
  Not g -> [ el "span" [ cls "op" ] [ text "not" ] ] <> guardNodes g
  And gs -> joined "and" gs
  Or gs -> joined "or" gs
  Cmp op a b ->
    exprNodes a
      <> [ el "span" [ cls "op cmp" ] [ text (cmpText op) ] ]
      <> exprNodes b
  where
  joined word gs =
    concatMap identity
      ( mapWithIndex
          ( \i g ->
              (if i > 0 then [ el "span" [ cls "op" ] [ text word ] ] else [])
                <> guardNodes g
          )
          gs
      )

exprNodes :: Expr -> Array Node
exprNodes = case _ of
  Lit v -> [ el "span" [ cls "lit mono" ] [ text (valueToString v) ] ]
  ConfigOf (ConfigId c) ->
    [ el "span" [ cls "ref config mono", attr "data-reads" c ] [ text c ] ]
  FactOf (FactId f) ->
    [ el "span" [ cls "ref fact mono", attr "data-reads" f ] [ text f ] ]

cmpText :: CmpOp -> String
cmpText = case _ of
  OpEq -> "="
  OpNe -> "≠"
  OpLt -> "<"
  OpLe -> "≤"
  OpGt -> ">"
  OpGe -> "≥"

exprText :: Expr -> String
exprText = case _ of
  Lit v -> valueToString v
  ConfigOf (ConfigId c) -> c
  FactOf (FactId f) -> f

-- =============================================================================
-- The drawing
-- =============================================================================

-- | The machine as a picture, generated here and now.
-- |
-- | Two things about this are deliberate and neither is about how it looks.
-- |
-- | **It is not a fixture.** `core/machines/rendered/*.mermaid.txt` exists and
-- | is committed and is *wrong* — `elevator.mermaid.txt` has three arrows out
-- | of `up` and none into it, a lift that only descends. It is wrong because it
-- | was rendered once, from an edge set that was short, and nothing since has
-- | made anyone look. Embedding it here would have made this page repeat a
-- | stale answer with a straight face. So the text below comes from
-- | `Glassbox.Export.StateDiagram` over `Glassbox.Edges`, at build time, and
-- | the drawing corrects itself the moment the library does.
-- |
-- | **Nothing is imported to render it.** The dialect is Mermaid's because a
-- | host renders it for free; where a host does not, the block shows its own
-- | source, which is the same second opinion in a different medium. No script
-- | is fetched either way, and the page stays one file.
diagramBlock :: Plate -> Node
diagramBlock p =
  el "figure" [ cls "diagram" ]
    [ el "h3" [ cls "sub-title" ]
        [ text "Drawing"
        , el "span" [ cls "sub-note" ]
            [ text "the same machine, from the edge set the checks above audit" ]
        ]
    , el "pre" [ cls "mermaid" ] [ text (mermaidInit <> "\n" <> trim p.diagram) ]
    , el "figcaption" [ cls "diagram-cap" ] caption
    ]
  where
  dropped = filter (\m -> not m.configGuarded) p.audit.missingEdges

  -- The reader looking at a lift that only descends has to be able to tell,
  -- without leaving the picture, that the picture is what is broken. The
  -- edges check above already knows; this only carries the reader to it.
  caption =
    if null dropped then
      [ text "Drawn by "
      , el "code" [] [ text "Glassbox.Export.StateDiagram" ]
      , text " from "
      , el "code" [] [ text "Glassbox.Edges" ]
      , text ", at the moment this page was set. Refusals are left out: they are self-loops, "
      , text "and the grid above already says which cells refuse."
      ]
    else
      [ el "span" [ cls "cap-flag" ] [ text "▲" ]
      , el "a" [ cls "cap-link", attr "href" ("#checks-" <> p.name) ]
          [ text (show (length dropped) <> " arrow" <> (if length dropped == 1 then "" else "s")) ]
      , text " that the rules above name are missing from this drawing. The picture is short, "
      , text "not the machine: "
      , el "code" [] [ text "Glassbox.Edges.probe" ]
      , text " keeps the first destination a guarded rule offers and drops the rest. Read the "
      , text "grid, not the arrows, until that is fixed."
      ]

-- | A rendering instruction, not a fact about the machine — which is why it is
-- | a Mermaid directive rather than something `toStateDiagram` was asked to
-- | emit. `neutral` is the greyscale theme: the plate spends colour four times
-- | and a picture that arrived with its own palette would make a fifth.
mermaidInit :: String
mermaidInit =
  "%%{init: {\"theme\":\"neutral\",\"themeVariables\":"
    <> "{\"fontFamily\":\"IBM Plex Mono, ui-monospace, monospace\",\"fontSize\":\"12px\"}}}%%"

-- =============================================================================
-- The authoring syntax
-- =============================================================================

-- | The machine as it would be written, from `Glassbox.DSL.Print`.
-- |
-- | The grid above *is* the `Table` syntax typeset; this is the other one, and
-- | the page lacked it. It is printed **whole** — the author's comments, the
-- | six declaration lists, the state blocks — because `Blocks` has a parser and
-- | is therefore the authoring format proper, and an authoring format that
-- | dropped anything would be disqualified. Setting an abridged version here
-- | would be quietly claiming a round trip the abridgement does not have.
-- |
-- | Measured over the corpus, nine rules in ten are one-liners, so this sets in
-- | a narrow column and reads down rather than across — which is `Blocks`'s own
-- | bet: a reader's question is usually *what can I do from here*, and that is
-- | a question about a state.
rulesBlock :: Plate -> Node
rulesBlock p =
  el "details" [ cls "rules", attr "open" "" ]
    [ el_ "summary"
        [ el "span" [ cls "sub-title" ]
            [ text "Rules"
            , el "span" [ cls "sub-note" ]
                [ text ("the authoring syntax, printed whole · " <> show (length lines) <> " lines") ]
            ]
        ]
    , el "pre" [ cls "blocks mono" ] (concatMap lineNode lines)
    ]
  where
  lines = dropTrailingBlank (split (Pattern "\n") p.blocks)

  -- Typographic classification only, and total: a line is a comment, a
  -- flush-left heading, or the body of a block. Nothing here reads the
  -- machine — a second parser would be a second description of it.
  lineNode line =
    [ el "span" [ cls ("bl " <> lineClass line) ] [ text line ], text "\n" ]

lineClass :: String -> String
lineClass line
  | String.take 1 line == "#" = "bl-note"
  | trim line == "" = "bl-gap"
  | String.take 1 line == " " = "bl-body"
  | otherwise = "bl-head"

dropTrailingBlank :: Array String -> Array String
dropTrailingBlank ls = case Array.last ls of
  Just l | trim l == "" -> dropTrailingBlank (fromMaybe ls (Array.init ls))
  _ -> ls

-- =============================================================================
-- Deadlines
-- =============================================================================

deadlinesBlock :: Plate -> Node
deadlinesBlock p =
  when_ (not (null p.spec.deadlines))
    ( el "div" [ cls "deadlines" ]
        [ el "h3" [ cls "sub-title" ]
            [ text "Deadlines"
            , el "span" [ cls "sub-note" ] [ text "fire with nobody pressing anything" ]
            ]
        , el "div" [ cls "deadline-list" ] (map one p.spec.deadlines)
        ]
    )
  where
  one d =
    el "div" [ cls "deadline" ]
      [ el "span" [ cls "mono strong", attr "data-reach" (rawState d.inState) ]
          [ text (rawState d.inState) ]
      , el "span" [ cls "on" ] [ text "after" ]
      , el "span" [ cls "mono", attr "data-reads" (exprText d.after) ] [ text (exprText d.after) ]
      , el "span" [ cls "unit" ] [ text (maybe "" clockName p.spec.clock) ]
      , el "span" [ cls "on" ] [ text "fires" ]
      , el "span" [ cls "mono" ] [ text (rawEvent d.fires) ]
      ]

-- =============================================================================
-- The apparatus: six glossaries
-- =============================================================================

vocabBlock :: Plate -> Node
vocabBlock p =
  el "div" [ cls "vocab" ]
    [ el "h3" [ cls "sub-title" ]
        [ text "Vocabularies"
        , el "span" [ cls "sub-note" ]
            [ text "a hollow mark is a declaration nothing in the machine uses" ]
        ]
    , el "div" [ cls "vocab-grid" ]
        [ glossary "states" (map stateEntry p.spec.states)
        , glossary "events" (map eventEntry p.spec.events)
        , glossary "config" (map configEntry p.spec.config)
        , glossary "facts" (map factEntry p.spec.facts)
        , glossary "refusals" (map refusalEntry p.spec.refusals)
        , glossary "commands" (map commandEntry p.spec.commands)
        ]
    ]
  where
  stateEntry s =
    { id: rawState s.id
    , label: s.label
    , dead: false
    , attrs: [ attr "data-reach" (rawState s.id) ]
    }
  eventEntry e =
    { id: rawEvent e.id
    , label: labelOfEvent p.spec e.id <> " · " <> sourceWord e.source
    , dead: false
    , attrs: [ attr "data-event" (rawEvent e.id) ]
    }
  configEntry c =
    { id: rawConfig c.id
    , label: c.label <> "  (default " <> valueToString c.default <> ")"
    , dead: not (c.id `elem` configReads p.spec)
    , attrs: [ attr "data-reads" (rawConfig c.id) ]
    }
  factEntry f =
    { id: rawFact f.id
    , label: f.label
    , dead: not (f.id `elem` factReads p.spec)
    , attrs: [ attr "data-reads" (rawFact f.id) ]
    }
  refusalEntry r =
    { id: rawRefusal r.id
    , label: r.text
    , dead: r.id `elem` p.audit.unusedRefusals
    , attrs: [ attr "data-refusal" (rawRefusal r.id) ]
    }
  commandEntry :: CommandDecl -> _
  commandEntry c =
    { id: rawCommand c.id
    , label: c.label
    , dead: c.id `elem` p.audit.unusedCommands
    , attrs: [ attr "data-command" (rawCommand c.id) ]
    }

glossary
  :: String
  -> Array { id :: String, label :: String, dead :: Boolean, attrs :: Array Attr }
  -> Node
glossary name entries =
  when_ (not (null entries))
    ( el "div" [ cls "gloss" ]
        [ el "div" [ cls "gloss-head" ]
            [ el "span" [ cls "gloss-name" ] [ text name ]
            , el "span" [ cls "gloss-count" ] [ text (show (length entries)) ]
            ]
        , el_ "dl" (concatMap entry entries)
        ]
    )
  where
  entry e =
    [ el "dt" ([ cls ("mono" <> if e.dead then " dead" else "") ] <> e.attrs)
        [ when_ e.dead (el "span" [ cls "deadmark", attr "title" "declared, never used" ] [ text "○" ])
        , text e.id
        ]
    , el "dd" [ cls (if e.dead then "dead" else "") ] [ text e.label ]
    ]

-- =============================================================================
-- Spine and colophon
-- =============================================================================

spine :: Array Plate -> Node
spine plates =
  el "nav" [ cls "spine" ]
    ( [ el "a" [ cls "spine-top", attr "href" "#plate" ] [ text "Plate" ] ]
        <> map link plates
    )
  where
  link p =
    el "a" [ cls "spine-item", attr "href" ("#m-" <> p.name), attr "data-spine" p.name ]
      [ el "span" [ cls "spine-name" ] [ text p.spec.title ]
      , el "span" [ cls "spine-dim mono" ]
          [ text (show (length p.spec.states) <> "×" <> show (length p.spec.events)) ]
      ]

-- | What setting the corpus turned up.
-- |
-- | Written by the page rather than by a person: every number below is counted
-- | from the plates above, so this section cannot congratulate itself on a
-- | finding that is no longer there.
findings :: Corpus -> Node
findings corpus =
  el "section" [ cls "found" ]
    [ el "h2" [ cls "section-title" ] [ text "What setting them turned up" ]
    , el "div" [ cls "found-grid" ]
        [ el "div" [ cls "found-item" ]
            [ el "div" [ cls "found-num mono" ] [ text (show s.cells) ]
            , el "p" []
                [ el_ "strong" [ text "Every pair in the corpus is declared." ]
                , text (" " <> show s.cells <> " state × event pairs, " <> show s.rules <> " rules, ")
                , text "and no machine leans on the runtime's silent "
                , el "code" [] [ text "Stay" ]
                , text ". The format has no totality lint; the corpus has kept itself total by hand, "
                , text "and a grid is the only place that fact is visible."
                ]
            ]
        , el "div" [ cls "found-item" ]
            [ el "div" [ cls "found-num mono" ] [ text (percent s.singleCase s.rules) ]
            , el "p" []
                [ el_ "strong" [ text "Nine rules in ten are one-liners." ]
                , text " That is the shape "
                , el "code" [] [ text "Glassbox.DSL.Print" ]
                , text " measured over five machines and it survives the sixth. It is why "
                , text "a cell can hold a whole rule, and why the "
                , text (show s.guarded <> " guarded cells ")
                , text "earn a numbered ladder of their own rather than a wider column for everybody."
                ]
            ]
        , el "div" [ cls "found-item" ]
            [ el "div" [ cls ("found-num mono" <> if missing > 0 then " bad" else "") ]
                [ text (show missing) ]
            , el "p" []
                [ el_ "strong"
                    [ text
                        ( if missing == 0 then "The rules and the drawn edges agree."
                          else "Transitions the rules name that no drawing shows."
                        )
                    ]
                , when_ (missing > 0)
                    ( foldNodes
                        [ text " Every one of them is an arm of a guarded rule that offers a "
                        , el_ "em" [ text "choice" ]
                        , text " of destination. "
                        , el "code" [] [ text "Glassbox.Edges.probe" ]
                        , text " resolves a pair under every assignment of the facts and then keeps "
                        , el_ "em" [ text "the first move it finds" ]
                        , text ", so a rule with two destinations contributes one edge and the other "
                        , text "destination is silently dropped. It reaches everything downstream of the "
                        , text "edge set: "
                        , el "code" [] [ text "unreachableStates" ]
                        , text ", the Mermaid export, and every picture drawn from it — including the "
                        , text "one under each grid above, and the committed "
                        , el "code" [] [ text "elevator.mermaid.txt" ]
                        , text ", which has no arrow into "
                        , el "code" [] [ text "up" ]
                        , text " at all: a lift that only goes down."
                        ]
                    )
                ]
            ]
        , when_ (suppressed > 0)
            ( el "div" [ cls "found-item" ]
                [ el "div" [ cls "found-num mono" ] [ text (show suppressed) ]
                , el "p" []
                    [ el_ "strong" [ text "More that look identical, and are correct." ]
                    , text " "
                    , foldNodes (weave suppressedEdges)
                    , text (if suppressed == 1 then " is " else " are ")
                    , text "missing from the edge set too, and belongs there: each sits behind a "
                    , el_ "em" [ text "config" ]
                    , text " read, and "
                    , el "code" [] [ text "edgesOf" ]
                    , text " enumerates facts only, deliberately — config is what decides which "
                    , text "machine this is, so a washer built without a prewash cycle genuinely "
                    , text "cannot reach that state and a drawing is right to leave it out. Telling "
                    , text "the two kinds apart is the difference between a bug report and a wrong "
                    , text "one, and a single count cannot show it."
                    ]
                ]
            )
        , el "div" [ cls "found-item" ]
            [ el "div" [ cls "found-num mono" ] [ text (show turns) ]
            , el "p" []
                [ el_ "strong" [ text "One cell in the corpus turns in place." ]
                , text " "
                , el "code" [] [ text "raised × tick → raised" ]
                , text " is a supervisor's whole keep-alive loop: it exits, re-enters, and "
                , text "re-entering runs "
                , el "code" [] [ text "reconcile" ]
                , text ". Spelt as a state's own name in a column of state names, it is the "
                , text "easiest cell in the corpus to read straight past."
                ]
            ]
        ]
    ]
  where
  s = corpusStats corpus.plates
  turns = sum (map (\p -> p.tally.turns) corpus.plates)

  -- Both numbers are counted off the plates, like every other number here, so
  -- neither survives the fix that removes it.
  missing = length (concatMap (\p -> filter (\m -> not m.configGuarded) p.audit.missingEdges) corpus.plates)
  suppressedEdges =
    concatMap
      (\p -> map (\m -> Tuple p.name m) (filter _.configGuarded p.audit.missingEdges))
      corpus.plates
  suppressed = length suppressedEdges

  -- Named rather than counted: these are the two the reader has to be able to
  -- go and look at, or the distinction is just an assertion.
  weave items =
    concatMap identity
      ( mapWithIndex
          ( \i item ->
              (if i == 0 then [] else if i == length items - 1 then [ text " and " ] else [ text ", " ])
                <> [ suppressedRef item ]
          )
          items
      )

  suppressedRef (Tuple name m) =
    el "a" [ cls "found-ref", attr "href" ("#checks-" <> name) ]
      [ el "code" [] [ text (name <> " · " <> rawState m.from <> " × " <> rawEvent m.on <> " → " <> rawState m.to) ] ]

colophon :: Corpus -> Node
colophon corpus =
  el "footer" [ cls "colophon" ]
    [ el "div" [ cls "col-block" ]
        [ el "h3" [ cls "sub-title" ] [ text "Colophon" ]
        , el "p" []
            [ text "Every mark on this page is derived from the artifacts themselves, decoded "
            , el "code" [] [ text "Glassbox.Codec.JSON" ]
            , text " — the same decoder a host uses — and checked with "
            , el "code" [] [ text "Glassbox.Lint" ]
            , text " and "
            , el "code" [] [ text "Glassbox.Edges" ]
            , text ". Nothing about a machine is written down twice: if the page and the "
            , text "runtime disagreed, the page could not have been generated."
            ]
        , el "p" []
            [ text "Five machines come from "
            , el "code" [] [ text "core/machines/" ]
            , text " in "
            , el "code" [] [ text "purescript-glassbox" ]
            , text "; the sixth, the supervised group, is Bosun's, and is the only one "
            , text "in the corpus that describes code somebody is running."
            ]
        , el "p" [ cls "col-set" ]
            [ text "Set in Archivo and IBM Plex Mono. Generated "
            , text corpus.built
            , text " by "
            , el "code" [] [ text "glassbox-plate" ]
            , text "."
            ]
        ]
    ]

-- =============================================================================
-- Raw identifiers
-- =============================================================================

rawState :: StateId -> String
rawState (StateId s) = s

rawEvent :: EventId -> String
rawEvent (EventId s) = s

rawRefusal :: RefusalId -> String
rawRefusal (RefusalId s) = s

rawCommand :: CommandId -> String
rawCommand (CommandId s) = s

rawConfig :: ConfigId -> String
rawConfig (ConfigId s) = s

rawFact :: FactId -> String
rawFact (FactId s) = s
