-- | Glassbox.Export.StateDiagram
-- |
-- | Emit a machine as state-diagram text.
-- |
-- | **Nothing is imported to do this.** The dialect is Mermaid's
-- | `stateDiagram-v2`, because GitHub and a hundred other things render it for
-- | free, but Mermaid is a *paste target* and not a dependency: what follows is
-- | string concatenation over our own value, and no external package is pulled
-- | in to produce it. The picture on screen is ours too — derived from the same
-- | edge set, laid out by `glassbox-draw`, and put on the page by HATS. This
-- | module is a convenience for getting a machine into somebody else's
-- | document, not a way of getting out of drawing it.
-- |
-- | It lives under `Export` rather than `Codec` because it only goes one way. A
-- | codec round-trips; this cannot be read back, and filing it beside
-- | `Glassbox.Codec.JSON` implied a symmetry it does not have.
-- |
-- | Its cheap value is a second opinion: if the picture and this text disagree,
-- | one of the two readings is wrong, and finding that out costs a glance.
module Glassbox.Export.StateDiagram
  ( toStateDiagram
  ) where

import Prelude

import Data.Array (filter, find, length, mapWithIndex, null)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Tuple (Tuple(..))
import Glassbox.Edges (Edge, EdgeKind(..))
import Glassbox.Spec (CommandId(..), Spec, StateDecl, StateId(..), labelOfEvent)

toStateDiagram :: Spec -> Array Edge -> String
toStateDiagram spec edges =
  joinWith "\n" $
    [ "stateDiagram-v2" ]
      <> map stateLine spec.states
      <> initialLine
      <> map edgeLine (filter drawn edges)
  where
  -- Refusals are self-loops that would treble the edge count without telling a
  -- reader anything the drawing's refusal styling does not already say.
  drawn edge = edge.kind /= OnRefusal

  raw (StateId s) = s

  -- The dialect's identifiers are not our identifiers, and this is where that
  -- was silently untrue for a year. A kebab-case id — `not-following`, which
  -- every machine in the corpus is full of — is a PARSE ERROR in
  -- `stateDiagram-v2`, so the export of a real machine rendered as nothing at
  -- all. Found by putting the output in front of a parser rather than in front
  -- of a reader, which is the only way this kind of bug is ever found.
  names = safeNames spec.states

  safe sid = fromMaybe (sanitise (raw sid)) (Map.lookup sid names)

  stateLine decl =
    "    " <> safe decl.id <> " : " <> clean decl.label <> ornament decl

  -- Commands hang off a state in this format, so a diagram that draws only the
  -- arrows is not a second opinion about the same machine — it is a second
  -- opinion about half of it.
  ornament decl =
    let
      parts =
        (if null decl.entry then [] else [ "entry " <> commandList decl.entry ])
          <> (if null decl.exit then [] else [ "exit " <> commandList decl.exit ])
    in
      if null parts then "" else "  (" <> joinWith ", " parts <> ")"

  commandList = joinWith " " <<< map (\(CommandId c) -> clean c)

  initialLine = case find (\d -> d.id == spec.initial) spec.states of
    Just initial -> [ "    [*] --> " <> safe initial.id ]
    Nothing -> []

  edgeLine edge =
    "    " <> safe edge.from <> " --> " <> safe edge.to
      <> " : " <> marker edge.kind
      <> joinWith ", " (map (clean <<< labelOfEvent spec) edge.events)

  marker = case _ of
    OnDeadline -> "after "
    _ -> ""

  -- A colon separates a transition from its label in this notation, so it
  -- cannot appear inside one.
  clean = replaceAll (Pattern ":") (Replacement " ")

-- | Assign each state an identifier the dialect will accept.
-- |
-- | Sanitising alone is not enough, because it is not injective: a machine
-- | declaring both `hot-water` and `hot_water` would give them the same name
-- | and merge two states into one — a diagram that is not wrong-looking, just
-- | wrong. So a name that would collide takes its index as a suffix, and the
-- | common case is left readable.
safeNames :: Array StateDecl -> Map StateId String
safeNames decls = Map.fromFoldable (mapWithIndex assign decls)
  where
  bare = map (\d -> sanitise (unId d.id)) decls

  assign i d =
    let
      s = sanitise (unId d.id)
    in
      Tuple d.id (if length (filter (eq s) bare) > 1 then s <> "_" <> show i else s)

  unId (StateId s) = s

-- | Letters, digits and underscore. Everything else becomes an underscore,
-- | which is what a hyphen has to be.
sanitise :: String -> String
sanitise = fromCharArray <<< map keep <<< toCharArray
  where
  keep c =
    if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then c
    else '_' 
