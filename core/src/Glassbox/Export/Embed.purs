-- | Glassbox.Export.Embed
-- |
-- | The artifact's own bytes, as a PureScript module.
-- |
-- | ### Why this is not codegen
-- |
-- | `the-same-machine.md` rejects generating *behaviour* — two transition
-- | tables that can disagree are worse than one with a hole — and that argument
-- | stands. What comes out of here is not a second description of the machine;
-- | it is the machine, byte for byte, with a `module` line in front of it. There
-- | is nothing for it to disagree with, because re-parsing it yields the same
-- | `Spec` the file does.
-- |
-- | ### What it is for
-- |
-- | A host that cannot read a file. Two real ones: a conformance harness that
-- | must be I/O-free so it can be transpiled and run under another backend, and
-- | any host on a runtime with no filesystem at all. Both would otherwise have
-- | to hand-write a `Spec`, which IS a second description and is exactly what
-- | the format exists to prevent.
-- |
-- | It is emphatically **not** the way an ordinary host loads a machine. The
-- | acceptance criterion is that a new machine requires zero lines of
-- | PureScript, and that survives only if the normal path reads the artifact at
-- | runtime. Compiling it in trades that for the ability to run somewhere a
-- | file cannot be opened, and a host that reaches for this when it could have
-- | read the file has quietly become codegen with extra steps.
module Glassbox.Export.Embed
  ( embedModule
  ) where

import Prelude

import Data.String (joinWith)
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))

-- | A module exporting one machine's artifact text verbatim.
-- |
-- | Takes the bytes rather than a `Spec` deliberately: round-tripping through
-- | the codec first would embed *what this version of the decoder understood*,
-- | so a field the codec silently dropped would be dropped here too and the
-- | embedded copy would no longer be the artifact. The file is the source.
embedModule :: { moduleName :: String, source :: String, regenerate :: String, artifact :: String } -> String
embedModule { moduleName, source, regenerate, artifact } = joinWith "\n"
  [ "-- | GENERATED FILE — do not edit."
  , "-- |"
  , "-- | Regenerate with `" <> regenerate <> "`."
  , "-- | Source: `" <> source <> "`."
  , "-- |"
  , "-- | The artifact itself, verbatim, for a host that cannot open a file —"
  , "-- | a conformance harness that must stay I/O-free, or a runtime with no"
  , "-- | filesystem. Decode it exactly as you would decode the file."
  , "-- |"
  , "-- | An ordinary host should NOT use this. Reading the artifact at runtime"
  , "-- | is what makes a new machine cost zero lines of PureScript."
  , "module " <> moduleName
  , "  ( artifactJson"
  , "  ) where"
  , ""
  , "artifactJson :: String"
  , "artifactJson = \"" <> escaped artifact <> "\""
  , ""
  ]
  where
  -- The backslash first, or it escapes the escapes. Carriage return and tab are
  -- spelled out because a literal one inside a PureScript string is legal and
  -- unreadable, and because the file may well have been written on Windows.
  escaped =
    replaceAll (Pattern "\\") (Replacement "\\\\")
      >>> replaceAll (Pattern "\"") (Replacement "\\\"")
      >>> replaceAll (Pattern "\n") (Replacement "\\n")
      >>> replaceAll (Pattern "\r") (Replacement "\\r")
      >>> replaceAll (Pattern "\t") (Replacement "\\t")
