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

import Data.Array (drop, filter, length, mapMaybe, null, sort)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_, or)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, stripSuffix)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (error, log)
import Glassbox.Codec.JSON (parseSpec)
import Glassbox.Canonical (canonicalBytes)
import Glassbox.DSL.Print (renderAll)
import Glassbox.Edges (defaultOptions, edgesOf)
import Glassbox.Export.StateDiagram (toStateDiagram)
import Glassbox.Run (worldFrom)
import Glassbox.Export.PureScript (vocabularyModule)
import Glassbox.Lint (explain, lint, rejections, warnings)
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
    [ "lint", inDir ] -> lintCmd inDir
    [ "canonical", inDir ] -> canonicalCmd inDir
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
  error "  lint <machines-dir>"
  error "      the correctness lints. Exit 1 if anything is rejected."
  error ""
  error "  canonical <machines-dir>"
  error "      each machine's canonical form, the text a machine hash is taken over."
  error "      Byte-identical on every runtime by construction; that is the point."
  error ""
  error "  render-all <machines-dir> <out-dir>"
  error "      every machine in every candidate syntax (Blocks, Table) and as a state diagram"
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

-- | Report every finding for every machine, and fail the process if any
-- | artifact would be refused. Nothing is silent: a machine that passes says
-- | so, because "no output" and "not run" look identical in a build log.
lintCmd :: FilePath -> Effect Unit
lintCmd inDir = do
  machines <- readMachines inDir
  bad <- traverse report machines
  when (or bad) (setExitCode 1)
  where
  report { name, spec } = do
    let found = lint spec
        no = rejections found
        yes = warnings found
    if null found then do
      log $ name <> ".json  clean"
      pure false
    else do
      log $ name <> ".json  " <> show (length no) <> " rejected, "
        <> show (length yes) <> " warned"
      log (explain found)
      pure (not (null no))

-- | The canonical form of every machine, on stdout, each under its own banner.
-- |
-- | Printed rather than written to files because its consumers are a hash and a
-- | diff between two runtimes, neither of which wants a directory.
canonicalCmd :: FilePath -> Effect Unit
canonicalCmd inDir = do
  machines <- readMachines inDir
  for_ machines \{ name, spec } -> do
    log ("=== " <> name <> " ===")
    log (canonicalBytes spec)

renderAllCmd :: FilePath -> FilePath -> Effect Unit
renderAllCmd inDir outDir = do
  machines <- readMachines inDir
  ensureDir outDir
  let
    -- The state diagram is emitted HERE rather than by `Print.renderAll`
    -- because it is derived from the edge set rather than from the Spec, and
    -- because it is an export rather than a syntax — it does not read back.
    --
    -- Until now it was emitted nowhere at all, and that is the whole reason a
    -- fatal bug in it survived: the module's justification is being a second
    -- opinion, and nothing was ever in a position to disagree with it. A
    -- printer with no output on disk is a printer nobody checks.
    diagrams = map
      ( \{ name, spec } -> Tuple (name <> ".mermaid.txt")
          (toStateDiagram spec (edgesOf defaultOptions (worldFrom spec) spec))
      )
      machines
  for_ (renderAll (map (\{ name, spec } -> Tuple name spec) machines) <> diagrams) \(Tuple file content) -> do
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
