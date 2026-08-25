-- | Probe: every machine's canonical form, printed.
-- |
-- | Deliberately free of `node-fs` — the machines are string literals compiled
-- | in, so what this proves is about `Glassbox.Canonical` and the codec rather
-- | than about whether a backend has filesystem foreigns yet.
module Probe.Main where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Glassbox.Canonical (canonicalBytes)
import Glassbox.Codec.JSON (parseSpec)
import Probe.Fixture (machines)

main :: Effect Unit
main = for_ machines \(Tuple name json) -> do
  log ("=== " <> name <> " ===")
  case parseSpec json of
    Left err -> log ("DECODE FAILED: " <> err)
    Right spec -> log (canonicalBytes spec)
