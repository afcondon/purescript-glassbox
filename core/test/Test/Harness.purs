-- | Test.Harness
-- |
-- | The plain-`Effect` runner the corpus tests already used, lifted out so the
-- | library tests can share it, plus one addition: a way to run a QuickCheck
-- | property that reports as a **single assertion** in the same tally.
-- |
-- | One run gives one verdict. A property that is green is one `ok` line with
-- | its case count; a property that is red is one `FAIL` line with the first
-- | counterexample's message and the seed that produced it.
-- |
-- | The seed is fixed rather than random. A property suite whose failures are
-- | not reproducible from the recorded output is a property suite that reports
-- | flakiness instead of defects; `Random.LCG.mkSeed` is right there if a
-- | shotgun run is ever wanted.
module Test.Harness
  ( check
  , section
  , prop
  , propWith
  , defaultSeed
  , defaultCases
  ) where

import Prelude

import Data.List as List
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (class Testable, Seed, checkResults, mkSeed, quickCheckPure', unSeed)
import Test.QuickCheck.Gen (Gen)

check :: String -> Boolean -> Effect Boolean
check name ok = do
  log ((if ok then "  ok   " else "  FAIL ") <> name)
  pure ok

section :: String -> Effect Unit
section name = log ("\n" <> name)

-- | Fixed, so a counterexample printed by CI can be reproduced locally.
defaultSeed :: Seed
defaultSeed = mkSeed 20260823

-- | Enough to find a shape bug, few enough that the cross-product properties
-- | stay under a second. The generators cap machines at five states and four
-- | events for the same reason.
defaultCases :: Int
defaultCases = 200

prop :: forall p. Testable p => String -> Gen p -> Effect Boolean
prop = propWith defaultCases

propWith :: forall p. Testable p => Int -> String -> Gen p -> Effect Boolean
propWith cases name gen = do
  let summary = checkResults (quickCheckPure' defaultSeed cases gen)
  ok <- check (name <> "  (" <> show summary.successes <> "/" <> show summary.total <> " cases)")
    (List.null summary.failures)
  -- `checkResults` accumulates failures with `Cons`, so the list runs newest
  -- first and the FIRST counterexample — the one worth looking at — is at the
  -- far end of it.
  case List.last summary.failures of
    Just f | not ok -> do
      log ("    first counterexample, case " <> show f.index <> ", seed " <> show (unSeed f.seed) <> ": " <> f.message)
    _ -> pure unit
  pure ok
