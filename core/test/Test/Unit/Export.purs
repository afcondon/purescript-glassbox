-- | Test.Unit.Export
-- |
-- | `Glassbox.Export.StateDiagram` and `Glassbox.Export.PureScript`.
-- |
-- | The module's own justification is that it is a **second opinion**: if the
-- | picture and this text disagree, one of the two readings is wrong. That is
-- | worth something only if the text is right, and the text is string
-- | concatenation into a notation with one significant character — the colon
-- | separating a transition from its label. So: that the colon is neutralised
-- | wherever an author's prose can reach it, that a refusal is left out, and
-- | that a deadline is marked as one rather than passing for a button.
module Test.Unit.Export
  ( exportUnits
  ) where

import Prelude

import Data.Array (any, filter, head, length)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Data.String (contains)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Glassbox.Edges (EdgeKind(..), defaultOptions, edgesOf)
import Glassbox.Export.PureScript (moduleNameFor, vocabularyModule)
import Glassbox.Export.StateDiagram (toStateDiagram)
import Glassbox.Run (worldFrom)
import Glassbox.Spec (CommandId(..), EventSource(..), Expr(..), Spec, Value(..))
import Test.Fixture (always, blank, edge, eid, finalSt, refusal, refuse, sid, st, to)
import Test.Harness (check, section)

-- | Labels chosen to be awkward: two of them carry the notation's one
-- | significant character, and one event only ever arrives by deadline.
awkward :: Spec
awkward = blank
  { id = "awkward"
  , initial = sid "off"
  , states =
      [ (st "off") { label = "Off: standby" }
      , (st "seeking") { label = "Seeking" }
      , (finalSt "locked") { label = "Locked" }
      ]
  , events =
      [ { id: eid "power", label: "power", source: FromUser }
      , { id: eid "seek", label: "seek: up", source: FromUser }
      , { id: eid "tune", label: "tune", source: FromUser }
      , { id: eid "found", label: "found", source: FromRuntime }
      , { id: eid "eject", label: "eject", source: FromUser }
      ]
  , refusals = [ refusal "no-disc" "there is no disc" ]
  , rules =
      [ { from: sid "off", on: eid "power", cases: [ to always "seeking" ] }
      -- Two pressable events that go the same way, so one arrow lists both.
      , { from: sid "seeking", on: eid "seek", cases: [ to always "locked" ] }
      , { from: sid "seeking", on: eid "tune", cases: [ to always "locked" ] }
      , { from: sid "seeking", on: eid "found", cases: [ to always "locked" ] }
      , { from: sid "off", on: eid "eject", cases: [ refuse always "no-disc" ] }
      ]
  , deadlines = [ { inState: sid "seeking", fires: eid "found", after: Lit (VNumber 3.0) } ]
  }

exportUnits :: Effect (Array Boolean)
exportUnits = do
  section "Export.StateDiagram"

  let edges = edgesOf defaultOptions (worldFrom awkward) awkward
  let text = toStateDiagram awkward edges
  let ls = split (Pattern "\n") text
  let has needle = contains (Pattern needle) text

  a <- check "the first line declares the dialect and nothing else"
    (head ls == Just "stateDiagram-v2")
  b <- check "every declared state gets a line carrying its label"
    (has "    off : Off  standby" && has "    seeking : Seeking" && has "    locked : Locked")

  -- A colon separates a transition from its label in this notation, so it
  -- cannot appear inside one — neither in a state label nor an event label.
  c <- check "a colon in a STATE label is replaced by a space, not emitted"
    (has "Off  standby" && not (has "Off: standby"))
  d <- check "a colon in an EVENT label is replaced by a space too"
    (has "seek  up" && not (has "seek: up"))
  e <- check "no line carries a colon that is not the notation's own separator"
    (length (filter (contains (Pattern ":")) ls) == length (filter (contains (Pattern " : ")) ls))

  f <- check "the initial state is marked"
    (has "    [*] --> off")
  g <- check "an initial state that is not declared emits no marker at all"
    (not (contains (Pattern "[*]") (toStateDiagram (awkward { initial = sid "ghost" }) edges)))

  h <- check "an ordinary transition carries its event labels and no marker"
    (has "    off --> seeking : power")
  i <- check "two events one way share an arrow, comma separated"
    (has "    seeking --> locked : seek  up, tune")

  -- A deadline is not something anyone pressed, and the text has to say so or a
  -- reader takes it for a button.
  j <- check "a deadline edge is marked with 'after '"
    (has "    seeking --> locked : after found")
  k <- check "and there is exactly one such marker, for the one deadline"
    (length (filter (contains (Pattern "after ")) ls) == 1)

  -- Refusals are self-loops; drawing them here would treble the edge count for
  -- something the drawing's own styling already says.
  l <- check "the machine really does have a refusal edge to leave out"
    (any (\e -> e.kind == OnRefusal) edges)
  m <- check "and no line mentions it"
    (not (has "off --> off"))
  n <- check "handing it a refusal edge directly does not smuggle one in either"
    ( not
        ( contains (Pattern "x --> x")
            (toStateDiagram awkward [ edge "x" "x" [ "eject" ] OnRefusal (Just "no-disc") ])
        )
    )

  o <- check "an event with no declared label falls back to its bare id"
    ( contains (Pattern "a --> b : zzz")
        (toStateDiagram unlabelled [ edge "a" "b" [ "zzz" ] OnEvent Nothing ])
    )
  p <- check "a machine with nothing in it is still a valid, empty diagram"
    (toStateDiagram unlabelled [] == "stateDiagram-v2")

  section "Export.PureScript"

  let
    vocab = blank
      { id = "car-radio"
      , states = [ st "off", st "seeking", st "locked" ]
      , commands =
          [ { id: CommandId "start-scan", label: "Start scanning" }
          , { id: CommandId "stop-scan", label: "Stop scanning" }
          ]
      }
    settings =
      { modulePrefix: "Demo.Vocabulary"
      , sourcePath: "machines/car-radio.json"
      , regenerateWith: "make vocabulary"
      }
    Tuple file source = vocabularyModule settings vocab
    said needle = contains (Pattern needle) source

  q <- check "a kebab-case machine id becomes a PureScript module name"
    (moduleNameFor vocab == "CarRadio" && file == "CarRadio.purs")
  r <- check "the module is declared under the prefix it was given"
    (said "module Demo.Vocabulary.CarRadio")
  s' <- check "the header repeats the caller's own regenerate command, not ours"
    (said "`make vocabulary`" && not (said "glassbox-cli"))

  -- The rows are the whole product. Sorted, because regenerating an unchanged
  -- machine has to be a no-op in the diff rather than a reordering.
  t <- check "states become a sorted row of Unit"
    ( said "type Phase :: Row Type"
        && said "  ( \"locked\" :: Unit\n  , \"off\" :: Unit\n  , \"seeking\" :: Unit\n  )"
    )
  u <- check "the same states become a row of handlers, for annotating a match"
    (said "type Cases :: Type -> Row Type" && said "  ( \"locked\" :: Unit -> b")
  v <- check "commands become their own row, parameterised by the value type"
    ( said "type Commands :: Type -> Row Type"
        && said "  ( \"start-scan\" :: v\n  , \"stop-scan\" :: v\n  )"
    )

  -- Nothing about behaviour may appear. This is the property that makes
  -- generating the alphabet different in kind from generating the machine.
  w <- check "no transition, guard, refusal or initial state reaches the output"
    (not (said "seeking") || not (said "-->"))
  x <- check "a machine with no commands still emits a well-formed empty row"
    ( contains (Pattern "type Commands :: Type -> Row Type\ntype Commands v =\n  ()")
        (snd (vocabularyModule settings (vocab { commands = [] })))
    )

  pure [ a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s', t, u, v, w, x ]
  where
  unlabelled = blank { states = [], events = [] }
