-- | Glassbox.Export.PureScript
-- |
-- | Emit a machine's **alphabet** as a PureScript module: the states it can be
-- | in and the commands it can ask its host to carry out, as rows.
-- |
-- | This is not codegen of the machine, and the distinction is the whole
-- | justification for it. `the-same-machine.md` rejects generating behaviour —
-- | two transition tables that can disagree are worse than one table with a
-- | hole — and that argument stands. What comes out of here describes no
-- | transitions at all, so it cannot contradict the artifact: it says nothing
-- | the artifact could contradict. The rules stay in the JSON, singular.
-- |
-- | What it buys is exhaustiveness at the host. A `case` over a `StateId` is a
-- | `case` over a string and can never be total; a record over a *row* is a
-- | product and cannot be partial. Given these rows a host can write
-- | `Variant.match` over the states and a handler record over the commands, and
-- | the compiler will hold it to both. The rows and the artifact are compared
-- | once, at load, by whatever the host uses for a vocabulary check — so a
-- | module left un-regenerated is a startup failure rather than a silent drift.
-- |
-- | It lives under `Export` beside `StateDiagram` for the same reason that one
-- | does: it only goes one way. Nothing reads a PureScript module back into a
-- | `Spec`.
-- |
-- | Pure, like every other printer here. Writing the file is the caller's job,
-- | which is what keeps `effect` and `node-fs` out of this package's manifest.
module Glassbox.Export.PureScript
  ( Vocabulary
  , moduleNameFor
  , vocabularyModule
  ) where

import Prelude

import Data.Array (filter, sort, uncons)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String.CodeUnits (singleton, toCharArray, uncons) as CU
import Data.String.Common (toUpper)
import Data.Tuple (Tuple(..))
import Glassbox.Spec (CommandId(..), Spec, StateId(..), commandIds, stateIds, versionFor)

-- | What the printer cannot work out for itself.
-- |
-- | `regenerateWith` is supplied rather than assumed: this module has no idea
-- | how it was invoked, and a generated file that lies about how to regenerate
-- | it is worse than one that says nothing.
type Vocabulary =
  { modulePrefix :: String
  , sourcePath :: String
  , regenerateWith :: String
  }

-- | `follow` becomes `Follow`; `car-radio` becomes `CarRadio`.
moduleNameFor :: Spec -> String
moduleNameFor spec = foldMap capitalise (wordsOf spec.id)
  where
  capitalise w = case CU.uncons w of
    Nothing -> w
    Just { head, tail } -> toUpper (CU.singleton head) <> tail

-- | Split on anything that cannot appear in a PureScript module name.
wordsOf :: String -> Array String
wordsOf = filter (_ /= "") <<< go [] "" <<< CU.toCharArray
  where
  go acc current chars = case uncons chars of
    Nothing -> acc <> [ current ]
    Just { head, tail }
      | alphanumeric head -> go acc (current <> CU.singleton head) tail
      | otherwise -> go (acc <> [ current ]) "" tail

  alphanumeric c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

-- | The module's name and its contents.
vocabularyModule :: Vocabulary -> Spec -> Tuple String String
vocabularyModule v spec = Tuple (name <> ".purs") contents
  where
  name = moduleNameFor spec
  states = map (\(StateId s) -> s) (stateIds spec)
  commands = map (\(CommandId c) -> c) (commandIds spec)

  contents = joinWith "\n"
    [ "-- | GENERATED FILE — do not edit."
    , "-- |"
    , "-- | Regenerate with `" <> v.regenerateWith <> "`."
    , "-- | Source: `" <> v.sourcePath <> "` (glassbox format " <> show (versionFor spec) <> ")."
    , "-- |"
    , "-- | This module is the machine's **alphabet**, and deliberately nothing else."
    , "-- | It declares which states exist and which commands exist; it says nothing"
    , "-- | about which state leads to which, so it cannot disagree with the artifact"
    , "-- | about behaviour. The transitions stay in the JSON, singular."
    , "-- |"
    , "-- | Regenerating is not what makes this safe — the host's vocabulary check"
    , "-- | compares these names with the artifact at boot, so a module left stale is a"
    , "-- | startup failure rather than a silent drift."
    , "module " <> v.modulePrefix <> "." <> name
    , "  ( Phase"
    , "  , Cases"
    , "  , Commands"
    , "  , phases"
    , "  , commands"
    , "  ) where"
    , ""
    , "import Prelude (Unit)"
    , ""
    , "import Type.Proxy (Proxy(..))"
    , ""
    , "-- | The states this machine can be in."
    , "-- |"
    , "-- | `Unit` because a Glassbox state is an opaque identifier and carries"
    , "-- | nothing with it."
    , "type Phase :: Row Type"
    , "type Phase ="
    , row states "Unit"
    , ""
    , "-- | The same states as a row of **handlers**."
    , "-- |"
    , "-- | This is what a renderer annotates its `Variant.match` record with, and the"
    , "-- | annotation is what makes a forgotten state say `PropertyIsMissing` — which"
    , "-- | names the label — instead of a row-unification failure that does not."
    , "type Cases :: Type -> Row Type"
    , "type Cases b ="
    , row states "Unit -> b"
    , ""
    , "-- | The commands this machine can ask its host to carry out."
    , "type Commands :: Type -> Row Type"
    , "type Commands v ="
    , row commands "v"
    , ""
    , "phases :: Proxy Phase"
    , "phases = Proxy"
    , ""
    , "commands :: Proxy (Commands Unit)"
    , "commands = Proxy"
    , ""
    ]

-- | A row literal, leading-comma, sorted so that regenerating an unchanged
-- | machine is a no-op in the diff.
row :: Array String -> String -> String
row labels ty = case uncons (sort labels) of
  Nothing -> "  ()"
  Just { head, tail } ->
    joinWith "\n" $
      [ "  ( " <> show head <> " :: " <> ty ]
        <> map (\l -> "  , " <> show l <> " :: " <> ty) tail
        <> [ "  )" ]
