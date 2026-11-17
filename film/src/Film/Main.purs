-- | Film.Main
-- |
-- | Writes the geometry the promotional film draws. Nothing in the film places
-- | a state or bends an arrow by hand: the edges come from `Glassbox.Edges`,
-- | the positions and curves from `DataViz.Layout.StateMachine`, once per
-- | setting of the machine's config, so the frame in which the CD slot is
-- | removed is the library's answer and not an animator's.
module Film.Main (main) where

import Prelude

import Data.Argonaut.Core (Json, fromArray, fromBoolean, fromNumber, fromObject, fromString, stringify)
import Data.Array (drop)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import DataViz.Layout.StateMachine.Layout (LayoutConfig, circularLayout, defaultConfig, layoutWithConfig)
import DataViz.Layout.StateMachine.Types (StateMachineLayout)
import Effect (Effect)
import Effect.Console (error, log)
import Foreign.Object as Object
import Glassbox.Codec.JSON (parseSpec)
import Glassbox.Draw (EdgeExtra, StateExtra, describe)
import Glassbox.Edges (Edge, EdgeKind(..), edgesOf, unreachableStates)
import Glassbox.Run (World, setConfig, worldFrom)
import Glassbox.Spec (ConfigId(..), Spec, StateId(..), Value(..))
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Node.Process (argv, setExitCode)

main :: Effect Unit
main = do
  args <- drop 2 <$> argv
  case args of
    [ machine, out ] -> do
      source <- readTextFile UTF8 machine
      case parseSpec source of
        Left err -> do
          error ("!! " <> machine <> ": " <> err)
          setExitCode 1
        Right spec -> do
          writeTextFile UTF8 out (stringify (document spec))
          log ("wrote " <> out)
    _ -> do
      error "glassbox-film <machine.json> <out.json>"
      setExitCode 1

document :: Spec -> Json
document spec = fromObject $ Object.fromFoldable
  [ Tuple "fitted" (picture fitted)
  , Tuple "withoutCdSlot" (picture withoutCdSlot)
  ]
  where
  fitted = worldFrom spec
  withoutCdSlot = setConfig (ConfigId "has-cd-slot") (VBoolean false) fitted

  picture world = fromObject $ Object.fromFoldable
    [ Tuple "layout" (encodeLayout (laidOut spec edges))
    , Tuple "unreachable" (fromArray (map (\(StateId s) -> fromString s) (unreachableStates spec edges)))
    ]
    where
    edges = movesUnder world spec

config :: LayoutConfig
config = defaultConfig
  { stateRadiusX = 64.0
  , stateRadiusY = 27.0
  , layoutRadius = 235.0
  , edgeCurvature = 0.14
  , parallelSeparation = 46.0
  , labelOffset = 15.0
  }

-- | Moves and deadlines only: the film draws a refusal where it happens and
-- | not as a standing arrow. Facts are left unknown, so every guarded arm that
-- | some world allows is in the picture; config is what the world says it is.
movesUnder :: World -> Spec -> Array Edge
movesUnder world spec = edgesOf { showRefusals: false, factsUnknown: true } world spec

-- | The ring. The tree strategy was tried and sets this machine's five states
-- | too close for their eleven arrows to be told apart.
laidOut :: Spec -> Array Edge -> StateMachineLayout StateExtra EdgeExtra
laidOut spec edges = layoutWithConfig config circularLayout (describe spec edges)

encodeLayout :: StateMachineLayout StateExtra EdgeExtra -> Json
encodeLayout l = fromObject $ Object.fromFoldable
  [ Tuple "originX" (fromNumber l.originX)
  , Tuple "originY" (fromNumber l.originY)
  , Tuple "width" (fromNumber l.width)
  , Tuple "height" (fromNumber l.height)
  , Tuple "initialArrow" $ fromObject $ Object.fromFoldable
      [ Tuple "x" (fromNumber l.initialArrow.x)
      , Tuple "y" (fromNumber l.initialArrow.y)
      , Tuple "angle" (fromNumber l.initialArrow.angle)
      ]
  , Tuple "states" (fromArray (map state l.states))
  , Tuple "transitions" (fromArray (map transition l.transitions))
  ]
  where
  state s = fromObject $ Object.fromFoldable
    [ Tuple "id" (fromString s.state.id)
    , Tuple "label" (fromString s.state.label)
    , Tuple "initial" (fromBoolean s.state.isInitial)
    , Tuple "entry" (fromArray (map fromString s.state.extra.entry))
    , Tuple "exit" (fromArray (map fromString s.state.extra.exit))
    , Tuple "cx" (fromNumber s.position.cx)
    , Tuple "cy" (fromNumber s.position.cy)
    , Tuple "rx" (fromNumber s.position.rx)
    , Tuple "ry" (fromNumber s.position.ry)
    ]

  transition t = fromObject $ Object.fromFoldable
    [ Tuple "from" (fromString t.transition.from)
    , Tuple "to" (fromString t.transition.to)
    , Tuple "label" (fromString t.transition.label)
    , Tuple "kind" $ fromString case t.transition.extra.kind of
        OnEvent -> "event"
        OnDeadline -> "deadline"
        OnRefusal -> "refusal"
    , Tuple "reason" case t.transition.extra.reason of
        Just r -> fromString r
        Nothing -> fromString ""
    , Tuple "startX" (fromNumber t.path.startX)
    , Tuple "startY" (fromNumber t.path.startY)
    , Tuple "controlX" (fromNumber t.path.controlX)
    , Tuple "controlY" (fromNumber t.path.controlY)
    , Tuple "endX" (fromNumber t.path.endX)
    , Tuple "endY" (fromNumber t.path.endY)
    , Tuple "labelX" (fromNumber t.path.labelX)
    , Tuple "labelY" (fromNumber t.path.labelY)
    , Tuple "angle" (fromNumber t.path.angle)
    , Tuple "selfLoop" (fromBoolean t.path.isSelfLoop)
    ]
