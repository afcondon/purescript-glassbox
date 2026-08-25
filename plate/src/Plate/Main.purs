-- | Plate.Main
-- |
-- | Read machines, read the stylesheet and the script, write one file.
-- |
-- | The output is a **single self-contained page**: the CSS and the JS are
-- | inlined rather than linked. That is not a packaging convenience, it is what
-- | the thing is for — a plate you can mail to somebody, drop in a ticket or
-- | open from a USB stick is a plate that gets looked at, and one that needs a
-- | server is one that gets described instead.
-- |
-- | Machines are decoded through `Glassbox.Codec.JSON`, exactly as
-- | `Glassbox.CLI` does and for the same reason: a tool that read the JSON its
-- | own way could typeset a machine the runtime would reject.
module Plate.Main (main) where

import Prelude

import Data.Array (drop, filter, length, mapMaybe, null, sort, uncons)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), stripSuffix)
import Data.String as String
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (error, log)
import Glassbox.Codec.JSON (parseSpec)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, readdir, writeTextFile)
import Node.Path (FilePath, concat)
import Node.Process (argv, lookupEnv, setExitCode)
import Plate.Model (Plate, plateOf)
import Plate.Render (renderDocument, renderFragment)

main :: Effect Unit
main = do
  args <- drop 2 <$> argv
  case uncons args of
    Just { head: out, tail: dirs } | not (null dirs) -> build out dirs
    _ -> usage

usage :: Effect Unit
usage = do
  error "glassbox-plate — typeset a corpus of machines as one self-contained page"
  error ""
  error "  <out.html> <machines-dir> [machines-dir ...]"
  error ""
  error "  PLATE_ASSETS  where plate.css and plate.js live (default: plate/assets)"
  error "  PLATE_BUILT   the build label written into the colophon"
  error "  PLATE_FRAGMENT=1  emit no html/head/body, for a host that supplies them"
  error ""
  error "Invoked as: spago run -p glassbox-plate -- site/index.html core/machines plate/machines"
  setExitCode 1

build :: FilePath -> Array FilePath -> Effect Unit
build out dirs = do
  assets <- fromMaybe "plate/assets" <$> lookupEnv "PLATE_ASSETS"
  built <- fromMaybe "from the corpus as it stood" <$> lookupEnv "PLATE_BUILT"
  style <- readTextFile UTF8 (concat [ assets, "plate.css" ])
  script <- readTextFile UTF8 (concat [ assets, "plate.js" ])
  loaded <- traverse readMachines dirs
  let plates = join loaded
  if null plates then do
    error "no machines found"
    setExitCode 1
  else do
    fragment <- lookupEnv "PLATE_FRAGMENT"
    let
      corpus = { plates, style, script, built }
      page = case fragment of
        Just "1" -> renderFragment corpus
        _ -> renderDocument corpus
    writeTextFile UTF8 out page
    log $ out <> "  " <> show (length plates) <> " machines, "
      <> show (String.length page)
      <> " characters"

-- | Every `*.json` in a directory, decoded through the real codec.
readMachines :: FilePath -> Effect (Array Plate)
readMachines dir = do
  entries <- readdir dir
  let names = sort (filter (_ /= "") (map basename entries))
  results <- traverse decode names
  pure (mapMaybe identity results)
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
      Right spec -> pure (Just (plateOf name spec))
