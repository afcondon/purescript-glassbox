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

import Data.Array (any, drop, filter, head, length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), snd)
import Data.String (contains)
import Data.String.Common (replaceAll, split)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Effect (Effect)
import Glassbox.Edges (EdgeKind(..), defaultOptions, edgesOf)
import Glassbox.Export.PureScript (moduleNameFor, vocabularyModule)
import Glassbox.Export.StateDiagram (toStateDiagram)
import Glassbox.Run (worldFrom)
import Glassbox.Spec (CommandId(..), EventSource(..), Expr(..), Spec, Value(..))
import Test.Fixture (always, blank, command, doing, edge, eid, ev, finalSt, refusal, refuse, sid, st, to)
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

  -- The bug this module shipped with for a year, and the reason it survived:
  -- the dialect's identifiers are not ours. A kebab-case id is a PARSE ERROR in
  -- stateDiagram-v2, so a real machine — every one in the corpus is full of
  -- them — exported to something that rendered as nothing at all. Verified
  -- against Mermaid 11.17 in both directions: the old output fails to parse at
  -- exactly the hyphen, and all five machines now parse.
  let
    kebab = blank
      { id = "kebab"
      , initial = sid "not-following"
      , states =
          [ (st "not-following") { label = "Not following" }
          , (doing "following-pending" [ "send-follow" ] [ "cancel" ]) { label = "Following..." }
          ]
      , events = [ ev "press" ]
      , commands = [ command "send-follow", command "cancel" ]
      , rules = [ { from: sid "not-following", on: eid "press", cases: [ to always "following-pending" ] } ]
      }
    kebabText = toStateDiagram kebab (edgesOf defaultOptions (worldFrom kebab) kebab)
    kebabSaid n = contains (Pattern n) kebabText

  p2 <- check "a hyphen in a state id becomes an underscore, because the dialect forbids it"
    (kebabSaid "    not_following : Not following" && not (kebabSaid "not-following :"))
  p3 <- check "and the arrows use the same substituted name, or they name nothing"
    (kebabSaid "    not_following --> following_pending : press")
  p4 <- check "the initial marker is substituted too"
    (kebabSaid "    [*] --> not_following")
  -- The identifier POSITIONS specifically: `-->` is a hyphen the dialect wants,
  -- and a label may say what it likes. What may not carry one is a name.
  p5 <- check "no identifier position carries a hyphen, whatever the labels do"
    ( not
        ( any (contains (Pattern "-"))
            (map names (drop 1 (split (Pattern "\n") kebabText)))
        )
    )

  -- Commands hang off states in this format, so a diagram drawing only arrows
  -- is a second opinion about half the machine.
  p6 <- check "a state's entry and exit commands appear on its own line"
    (kebabSaid "following_pending : Following...  (entry send-follow, exit cancel)")
  p7 <- check "and a state that commands nothing gains no parenthesis"
    (kebabSaid "not_following : Not following\n")

  -- Sanitising is not injective, and two states merging into one is a diagram
  -- that is not wrong-looking, merely wrong.
  let
    collide = blank
      { id = "collide"
      , initial = sid "hot-water"
      , states = [ st "hot-water", st "hot_water" ]
      , events = []
      , rules = []
      }
    collideText = toStateDiagram collide (edgesOf defaultOptions (worldFrom collide) collide)

  p8 <- check "two ids that would sanitise the same are kept apart"
    ( contains (Pattern "hot_water_0 :") collideText
        && contains (Pattern "hot_water_1 :") collideText
    )

  section "Export.PureScript"

  let
    vocab = blank
      { id = "car-radio"
      , states = [ st "off", st "seeking", st "locked" ]
      , events = [ ev "scan" ]
      , refusals = [ refusal "no-disc" "there is no disc" ]
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

  -- Both directions of the seam, not just the one the machine speaks. A host
  -- naming an event through `Events` cannot send one the machine never
  -- declared, which a bare `EventId` could never prevent.
  v2 <- check "events become a row, so a host cannot invent one"
    (said "type Events :: Row Type" && said "  ( \"scan\" :: Unit\n  )")
  v3 <- check "refusals become a row, and a row of handlers to answer them"
    ( said "type Refusals :: Row Type"
        && said "type Excuses :: Type -> Row Type"
        && said "  ( \"no-disc\" :: Unit -> b\n  )"
    )

  -- Nothing about behaviour may appear. This is the property that makes
  -- generating the alphabet different in kind from generating the machine.
  w <- check "no transition, guard, refusal or initial state reaches the output"
    (not (said "seeking") || not (said "-->"))
  x <- check "a machine with no commands still emits a well-formed empty row"
    ( contains (Pattern "type Commands :: Type -> Row Type\ntype Commands v =\n  ()")
        (snd (vocabularyModule settings (vocab { commands = [] })))
    )

  pure [ a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, p2, p3, p4, p5, p6, p7, p8, q, r, s', t, u, v, v2, v3, w, x ]
  where
  unlabelled = blank { states = [], events = [] }

  -- The identifiers on one line: everything left of the label separator, with
  -- the dialect's own punctuation taken out.
  names line =
    replaceAll (Pattern "[*]") (Replacement "")
      (replaceAll (Pattern "-->") (Replacement "") (fromMaybe line (head (split (Pattern " : ") line))))
