-- | Glassbox.CLI
-- |
-- | The runner that sits beside the printers.
-- |
-- | Everything Glassbox knows how to say about a machine is said by a **pure
-- | function** — `DSL.Print.renderAll`, `Export.StateDiagram.toStateDiagram`,
-- | `Export.PureScript.vocabularyModule`. None of them can write a file, and
-- | that is deliberate: the `glassbox` package must not depend on `effect` or
-- | `node-fs`, because a headless conformance runner and a non-JS backend both
-- | have to be able to run a machine without dragging a filesystem into the
-- | manifest.
-- |
-- | So the IO lives here, in a package nobody depends on, and it is as thin as
-- | it can be: read, parse, hand to a printer, write. Any judgement about what
-- | the output should look like belongs upstream of this module.
module Glassbox.CLI (main) where

import Prelude

import Data.Array (drop, filter, mapMaybe, sort)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, stripSuffix)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (error, log)
import Glassbox.Codec.JSON (parseSpec)
import Glassbox.DSL.Print (renderAll)
import Glassbox.Export.PureScript (vocabularyModule)
import Glassbox.Spec (Spec)
import Node.Encoding (Encoding(..))
import Node.FS.Perms (permsAll)
import Node.FS.Sync (mkdir', readTextFile, readdir, writeTextFile)
import Node.Path (FilePath, concat)
import Node.Process (argv, setExitCode)

main :: Effect Unit
main = do
  args <- argv
  case drop2 args of
    [ "render-all", inDir, outDir ] -> renderAllCmd inDir outDir
    [ "vocabulary", inDir, outDir, prefix ] ->
      vocabularyCmd inDir outDir prefix defaultRegenerate
    [ "vocabulary", inDir, outDir, prefix, how ] ->
      vocabularyCmd inDir outDir prefix how
    _ -> usage

usage :: Effect Unit
usage = do
  error "glassbox-cli — write out what Glassbox prints of a machine"
  error ""
  error "  render-all <machines-dir> <out-dir>"
  error "      every machine in every candidate syntax (Blocks, Table)"
  error ""
  error "  vocabulary <machines-dir> <out-dir> <module-prefix> [regen-command]"
  error "      each machine's alphabet as a PureScript module of rows."
  error "      regen-command is written into the generated header; give the"
  error "      caller's own command for it, not this one."
  error ""
  error "Invoked as: spago run -p glassbox-cli -- <subcommand> ..."
  setExitCode 1

-- | `node` and the script path occupy the first two slots of argv.
drop2 :: Array String -> Array String
drop2 = drop 2

-- ---------------------------------------------------------------------------

renderAllCmd :: FilePath -> FilePath -> Effect Unit
renderAllCmd inDir outDir = do
  machines <- readMachines inDir
  ensureDir outDir
  for_ (renderAll (map (\{ name, spec } -> Tuple name spec) machines)) \(Tuple file content) -> do
    writeTextFile UTF8 (concat [ outDir, file ]) content
    log $ file <> "  " <> show (lines content) <> " lines"

defaultRegenerate :: String
defaultRegenerate = "spago run -p glassbox-cli -- vocabulary <machines> <out> <prefix>"

vocabularyCmd :: FilePath -> FilePath -> String -> String -> Effect Unit
vocabularyCmd inDir outDir prefix how = do
  machines <- readMachines inDir
  ensureDir outDir
  for_ machines \{ name, spec } -> do
    let
      settings =
        { modulePrefix: prefix
        -- The last segment only. A generated file is committed, so it must not
        -- record where on somebody's disk the tool happened to be run from.
        , sourcePath: lastSegment inDir <> "/" <> name <> ".json"
        , regenerateWith: how
        }
      Tuple file content = vocabularyModule settings spec
    writeTextFile UTF8 (concat [ outDir, file ]) content
    log $ name <> ".json  ->  " <> concat [ outDir, file ]

-- ---------------------------------------------------------------------------

type Machine = { name :: String, spec :: Spec }

-- | Every `*.json` in a directory, decoded through the real codec.
-- |
-- | Decoding rather than reading the JSON directly is the point: a tool that
-- | parsed the file its own way could describe a machine the runtime would
-- | reject. This one can only emit what Glassbox itself accepts.
readMachines :: FilePath -> Effect (Array Machine)
readMachines dir = do
  entries <- readdir dir
  let names = sort (filter (_ /= "") (map basename entries))
  results <- traverse decode names
  pure (concatJust results)
  where
  basename entry = case stripSuffix (Pattern ".json") entry of
    Just n -> n
    Nothing -> ""

  decode name = do
    source <- readTextFile UTF8 (concat [ dir, name <> ".json" ])
    case parseSpec source of
      Left err -> do
        error $ "!! " <> name <> ".json: " <> err
        setExitCode 1
        pure Nothing
      Right spec -> pure (Just { name, spec })

concatJust :: forall a. Array (Maybe a) -> Array a
concatJust = mapMaybe identity

lastSegment :: FilePath -> String
lastSegment path = case Array.last (filter (_ /= "") (split (Pattern "/") path)) of
  Just seg -> seg
  Nothing -> path

ensureDir :: FilePath -> Effect Unit
ensureDir dir = mkdir' dir { recursive: true, mode: permsAll }

lines :: String -> Int
lines s = Array.length (split (Pattern "\n") s) - 1
