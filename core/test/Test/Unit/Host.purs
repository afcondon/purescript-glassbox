-- | Test.Unit.Host
-- |
-- | `Glassbox.Host` is the half of the seam the compiler can check, and the
-- | thing worth pinning is the **division of labour** rather than any one
-- | function. The compiler checks the host against its own declaration; the
-- | reports check that declaration against the artifact. Neither half is
-- | sufficient, and a host that skips the second has a type-checked lie.
-- |
-- | So the rows below are written out by hand, exactly as a generated
-- | vocabulary module would emit them, and the tests drive them against a spec
-- | that deliberately does not match.
module Test.Unit.Host
  ( hostUnits
  ) where

import Prelude

import Data.Array (sort)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Data.Variant as V
import Effect (Effect)
import Glassbox.Host
  ( agrees
  , dispatch
  , event
  , reifyTag
  , reportCommands
  , reportEvents
  , reportRefusals
  , reportStates
  , vocabularyOf
  )
import Glassbox.Spec (CommandId(..), EventId(..), Spec, commandIds)
import Test.Fixture (blank, command, ev, refusal, st)
import Test.Harness (check, section)
import Type.Proxy (Proxy(..))

-- | What a generated vocabulary module looks like, written by hand.
type Phase :: Row Type
type Phase = ("off" :: Unit, "on" :: Unit)

type Commands :: Type -> Row Type
type Commands v = ("light-lamp" :: v, "snuff-lamp" :: v)

type Events :: Row Type
type Events = ("flip" :: Unit)

phases :: Proxy Phase
phases = Proxy

events :: Proxy Events
events = Proxy

commands :: forall v. Proxy (Commands v)
commands = Proxy

-- | A machine that agrees with the rows above about states, and disagrees
-- | about everything else — one command it does not have a word for, one it
-- | has a spare word for, and a refusal nobody answers.
spec :: Spec
spec = blank
  { id = "lamp"
  , states = [ st "off", st "on" ]
  , events = [ ev "flip", ev "wobble" ]
  , commands = [ command "light-lamp", command "hum" ]
  , refusals = [ refusal "no-bulb" "there is no bulb" ]
  }

hostUnits :: Effect (Array Boolean)
hostUnits = do
  section "Host — what the compiler checks, and what it cannot"

  a <- check "a row reflects to its own labels"
    (sort (vocabularyOf phases) == [ "off", "on" ])

  -- The report a host runs at load, and the reason it runs it: `missing` is a
  -- command the artifact asks for that this host has no word for, which is the
  -- hole a catch-all used to hide.
  let cmds = reportCommands (commands :: Proxy (Commands Unit)) spec
  b <- check "a command the machine asks for and the host lacks is missing"
    (cmds.missing == [ "hum" ])
  c <- check "a word the host has and this machine never uses is unused, not fatal"
    (cmds.unused == [ "snuff-lamp" ])
  d <- check "and missing is what decides whether a host may proceed"
    (not (agrees cmds) && agrees (reportStates phases spec))

  e <- check "the same comparison, pointed at states, agrees with this machine"
    (reportStates phases spec == { missing: [], unused: [] })
  f <- check "an event the host never sends is reported, so its absence is a decision"
    ((reportEvents events spec).missing == [ "wobble" ])
  g <- check "a refusal nobody has an answer for is reported too"
    ((reportRefusals (Proxy :: Proxy ()) spec).missing == [ "no-bulb" ])

  -- Route 2: a record is a product, so the handler set cannot be partial. The
  -- annotation is what does the work — without it the record is just a record
  -- and the check is cosmetic.
  let
    implementations :: Record (Commands String)
    implementations = { "light-lamp": "lit", "snuff-lamp": "snuffed" }

  h <- check "dispatch finds a command the host implements"
    (dispatch implementations (CommandId "light-lamp") == Just "lit")
  i <- check "and answers Nothing for one it does not, rather than guessing"
    (dispatch implementations (CommandId "hum") == (Nothing :: Maybe String))

  -- Route 3: a Variant is a sum, so the renderer cannot forget a state. This
  -- is the one boundary where a string becomes a member of a row and can fail.
  let
    render :: Variant Phase -> String
    render = V.match { "off": \_ -> "dark", "on": \_ -> "bright" }

  j <- check "a declared state reifies into the row and matches exhaustively"
    (map render (reifyTag "on" :: Maybe (Variant Phase)) == Just "bright")
  k <- check "and a string that is not in the row fails here rather than later"
    (map render (reifyTag "flickering" :: Maybe (Variant Phase)) == Nothing)

  -- The direction the reports cannot help with. They compare sets at load;
  -- this checks a single USE at compile time, which is what a typo needs.
  l <- check "naming an event through the row yields exactly that event"
    (event @"flip" events == EventId "flip")

  -- The load-time check the host is expected to run, spelled out: this is
  -- literally all a boot gate does with the report.
  m <- check "a machine whose commands the host does implement is accepted"
    ( agrees
        ( reportCommands (commands :: Proxy (Commands Unit))
            (spec { commands = [ command "light-lamp" ] })
        )
    )
  n <- check "and the spec's own command list is what it is compared against"
    (map (\(CommandId raw) -> raw) (commandIds spec) == [ "light-lamp", "hum" ])

  pure [ a, b, c, d, e, f, g, h, i, j, k, l, m, n ]
