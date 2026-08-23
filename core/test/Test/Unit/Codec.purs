-- | Test.Unit.Codec
-- |
-- | `Glassbox.Codec.JSON`, by the *shape* of what arrives.
-- |
-- | The plan calls this module an FFI boundary: "narrow, explicit, versioned,
-- | checked rather than trusted, and loud on failure". A boundary that is only
-- | ever tested with well-formed input is a boundary that is trusted. So the
-- | bulk of this file is malformed artifacts, one per way of being malformed.
-- |
-- | The second half is more interesting and less comfortable. Several kinds of
-- | nonsense **decode perfectly well today** — a transition to a state that does
-- | not exist, a guard reading a fact nobody declared, a (state, event) pair
-- | with no rule. Those are the correctness lints of phase 2, and they are not
-- | written yet. They are tested here as *present behaviour*, marked, so that
-- | when the lints land these tests fail loudly and are inverted deliberately
-- | rather than a hole being discovered in production.
module Test.Unit.Codec
  ( codecUnits
  ) where

import Prelude

import Data.Array (length)
import Data.Either (Either(..), either, hush, isLeft, isRight)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Glassbox.Codec.JSON (decodeSpec, encodeSpec, parseSpec, printSpec)
import Glassbox.Run (resolve, worldFrom, dueAt)
import Glassbox.Spec
  ( CmpOp(..)
  , ConfigId(..)
  , EventId(..)
  , EventSource(..)
  , Expr(..)
  , FactId(..)
  , Guard(..)
  , Outcome(..)
  , Spec
  , StateId(..)
  , Value(..)
  )
import Test.Fixture (always, blank, cfg, eid, ev, fact, finalSt, num, refusal, refuse, sid, st, stay, str, to)
import Test.Harness (check, section)

-- =============================================================================
-- Helpers
-- =============================================================================

rejects :: String -> Boolean
rejects = isLeft <<< parseSpec

accepts :: String -> Boolean
accepts = isRight <<< parseSpec

specOf :: String -> Maybe Spec
specOf = hush <<< parseSpec

versionOf :: String -> Maybe Int
versionOf = map _.version <<< specOf

errorOf :: String -> String
errorOf = either identity (const "<accepted>") <<< parseSpec

mentions :: String -> String -> Boolean
mentions needle = contains (Pattern needle)

-- | The smallest artifact that decodes, as a prefix other fixtures extend.
minimal :: String
minimal = """{"glassbox":1,"id":"m","initial":"a","states":[{"id":"a"}]}"""

-- =============================================================================
-- The envelope
-- =============================================================================

envelopeUnits :: Effect (Array Boolean)
envelopeUnits = do
  section "Codec — the envelope"

  a <- check "text that is not JSON is refused"
    (rejects "{not json")
  b <- check "the empty string is refused"
    (rejects "")
  c <- check "a JSON array at the top level is refused"
    (rejects "[]")
  d <- check "a JSON string at the top level is refused"
    (rejects "\"a machine\"")
  e <- check "JSON null is refused"
    (rejects "null")
  f <- check "a number at the top level is refused"
    (rejects "42")

  g <- check "no version field at all is refused"
    (rejects """{"id":"m","initial":"a","states":[{"id":"a"}]}""")
  h <- check "a version that is a string is refused"
    (rejects """{"glassbox":"1","id":"m","initial":"a","states":[]}""")
  i <- check "a version that is not a whole number is refused"
    (rejects """{"glassbox":1.5,"id":"m","initial":"a","states":[]}""")
  j <- check "one version beyond this build is refused"
    (rejects """{"glassbox":3,"id":"m","initial":"a","states":[]}""")
  k <- check "the refusal says which version it saw and which it reads"
    ( mentions "3" (errorOf """{"glassbox":3,"id":"m","initial":"a","states":[]}""")
        && mentions "glassbox" (errorOf """{"glassbox":3,"id":"m","initial":"a","states":[]}""")
    )

  -- Only *future* versions are refused, and the rest are normalised UPWARD to
  -- the version they actually need. Version 0 and negatives used to sail
  -- through and keep a number no reader could act on; they now read as the 1
  -- they are. This was one of the format's recorded oddities and the effects
  -- fork closed it as a side effect of having to compute a version at all.
  l <- check "a version BELOW this build is read as the version it actually needs"
    (versionOf """{"glassbox":0,"id":"m","initial":"a","states":[{"id":"a"}]}""" == Just 1)
  m <- check "and so is a negative one"
    (versionOf """{"glassbox":-3,"id":"m","initial":"a","states":[{"id":"a"}]}""" == Just 1)

  n <- check "a missing id is refused"
    (rejects """{"glassbox":1,"initial":"a","states":[{"id":"a"}]}""")
  o <- check "an id that is not a string is refused"
    (rejects """{"glassbox":1,"id":7,"initial":"a","states":[{"id":"a"}]}""")
  p <- check "a missing initial state is refused"
    (rejects """{"glassbox":1,"id":"m","states":[{"id":"a"}]}""")

  -- An unknown top-level key is silently ignored, which sits awkwardly beside
  -- the decoder's own comment about not silently ignoring fields we do not
  -- understand. Present behaviour, recorded.
  q <- check "an unknown top-level key is silently ignored (documented)"
    (accepts """{"glassbox":1,"id":"m","initial":"a","states":[{"id":"a"}],"effects":["beep"]}""")
  r <- check "an unknown key inside a state declaration is ignored too (documented)"
    (accepts """{"glassbox":1,"id":"m","initial":"a","states":[{"id":"a","colour":"red"}]}""")

  pure [ a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r ]

-- =============================================================================
-- Declarations
-- =============================================================================

declarationUnits :: Effect (Array Boolean)
declarationUnits = do
  section "Codec — declarations, and what may be left out"

  a <- check "states must be an array, not an object"
    (rejects """{"glassbox":1,"id":"m","initial":"a","states":{"a":{}}}""")
  b <- check "a state that is not an object is refused"
    (rejects """{"glassbox":1,"id":"m","initial":"a","states":[3]}""")
  c <- check "a state with no id is refused"
    (rejects """{"glassbox":1,"id":"m","initial":"a","states":[{"label":"A"}]}""")
  d <- check "a state whose id is not a string is refused"
    (rejects """{"glassbox":1,"id":"m","initial":"a","states":[{"id":3}]}""")
  e <- check "the error names the list the bad member was in"
    (mentions "states" (errorOf """{"glassbox":1,"id":"m","initial":"a","states":[{"id":3}]}"""))

  f <- check "every optional list may be left out entirely"
    ( maybe false
        ( \s -> length s.events == 0 && length s.config == 0 && length s.facts == 0
            && length s.refusals == 0
            && length s.rules == 0
            && length s.deadlines == 0
        )
        (specOf minimal)
    )
  g <- check "an absent title falls back to the machine's id"
    (map _.title (specOf minimal) == Just "m")
  h <- check "an absent state label falls back to the state's id"
    (map (map _.label <<< _.states) (specOf minimal) == Just [ "a" ])
  i <- check "an absent final flag is false, and a declared one is kept"
    ( map (map _.final <<< _.states)
        (specOf """{"glassbox":1,"id":"m","initial":"a","states":[{"id":"a"},{"id":"b","final":true}]}""")
        == Just [ false, true ]
    )
  j <- check "a non-boolean final flag reads as false rather than failing (documented)"
    ( map (map _.final <<< _.states)
        (specOf """{"glassbox":1,"id":"m","initial":"a","states":[{"id":"a","final":"yes"}]}""")
        == Just [ false ]
    )

  k <- check "an absent event source means the person pressed it"
    ( map (map _.source <<< _.events)
        (specOf """{"glassbox":1,"id":"m","initial":"a","states":[],"events":[{"id":"e"}]}""")
        == Just [ FromUser ]
    )
  l <- check "runtime is the other one"
    ( map (map _.source <<< _.events)
        (specOf """{"glassbox":1,"id":"m","initial":"a","states":[],"events":[{"id":"e","source":"runtime"}]}""")
        == Just [ FromRuntime ]
    )
  m <- check "any other source is refused — the vocabulary is closed"
    (rejects """{"glassbox":1,"id":"m","initial":"a","states":[],"events":[{"id":"e","source":"keyboard"}]}""")

  n <- check "a config declaration with no default is refused"
    (rejects """{"glassbox":1,"id":"m","initial":"a","states":[],"config":[{"id":"c"}]}""")
  o <- check "a config default of null is refused"
    (rejects """{"glassbox":1,"id":"m","initial":"a","states":[],"config":[{"id":"c","default":null}]}""")
  p <- check "a config default that is an array is refused"
    (rejects """{"glassbox":1,"id":"m","initial":"a","states":[],"config":[{"id":"c","default":[1]}]}""")
  q <- check "a config default that is an object is refused"
    (rejects """{"glassbox":1,"id":"m","initial":"a","states":[],"config":[{"id":"c","default":{"n":1}}]}""")
  r <- check "all three value kinds are accepted bare, as an author would write them"
    ( map (map _.default <<< _.config)
        ( specOf
            """{"glassbox":1,"id":"m","initial":"a","states":[],"config":[{"id":"n","default":4},{"id":"b","default":true},{"id":"s","default":"fm"}]}"""
        ) == Just [ VNumber 4.0, VBoolean true, VString "fm" ]
    )

  pure [ a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r ]

-- =============================================================================
-- Rules, cases and guards
-- =============================================================================

ruleUnits :: Effect (Array Boolean)
ruleUnits = do
  section "Codec — rules, cases and guards"

  a <- check "a rule with no from is refused"
    (rejects (withRules """[{"on":"e","cases":[{"stay":true}]}]"""))
  b <- check "a rule with no on is refused"
    (rejects (withRules """[{"from":"a","cases":[{"stay":true}]}]"""))
  c <- check "a rule with no cases key is refused"
    (rejects (withRules """[{"from":"a","on":"e"}]"""))
  d <- check "a case with no outcome is refused"
    (rejects (withRules """[{"from":"a","on":"e","cases":[{}]}]"""))
  e <- check "the refusal explains that a case needs exactly one outcome"
    (mentions "exactly one" (errorOf (withRules """[{"from":"a","on":"e","cases":[{}]}]""")))
  f <- check "a case whose to is not a string is refused"
    (rejects (withRules """[{"from":"a","on":"e","cases":[{"to":3}]}]"""))
  g <- check "an empty cases array is accepted, and resolves to Stay"
    ( maybe false (\s -> resolve s (worldFrom s) (sid "a") (eid "e") == Stay)
        (specOf (withRules """[{"from":"a","on":"e","cases":[]}]"""))
    )

  -- "Exactly one outcome key must be present", says the doc comment, but two
  -- are accepted and `to` quietly wins. An author who edits `{"to":"b"}` into
  -- `{"stay":true}` without deleting the old key gets the old behaviour and no
  -- complaint. Recorded as present behaviour.
  h <- check "a case with BOTH to and stay is accepted, and to wins (documented)"
    ( maybe false (\s -> resolve s (worldFrom s) (sid "a") (eid "e") == Move (StateId "b"))
        (specOf (withRules """[{"from":"a","on":"e","cases":[{"to":"b","stay":true}]}]"""))
    )
  i <- check "stay beats refuse in the same way (documented)"
    ( maybe false (\s -> resolve s (worldFrom s) (sid "a") (eid "e") == Stay)
        (specOf (withRules """[{"from":"a","on":"e","cases":[{"stay":true,"refuse":"r"}]}]"""))
    )

  j <- check "an unknown comparison operator is refused"
    (rejects (withRules """[{"from":"a","on":"e","cases":[{"when":{"op":"gtt","left":1,"right":2},"stay":true}]}]"""))
  k <- check "a comparison missing its left operand is refused"
    (rejects (withRules """[{"from":"a","on":"e","cases":[{"when":{"op":"gt","right":2},"stay":true}]}]"""))
  l <- check "a comparison missing its right operand is refused"
    (rejects (withRules """[{"from":"a","on":"e","cases":[{"when":{"op":"gt","left":1},"stay":true}]}]"""))
  m <- check "all of a non-array is refused"
    (rejects (withRules """[{"from":"a","on":"e","cases":[{"when":{"all":3},"stay":true}]}]"""))
  n <- check "an empty all is accepted — the identity of conjunction"
    ( map (map _.cases <<< _.rules) (specOf (withRules """[{"from":"a","on":"e","cases":[{"when":{"all":[]},"stay":true}]}]"""))
        == Just [ [ { when: Just (And []), outcome: Stay } ] ]
    )
  o <- check "an empty any is accepted — the identity of disjunction"
    ( map (map _.cases <<< _.rules) (specOf (withRules """[{"from":"a","on":"e","cases":[{"when":{"any":[]},"stay":true}]}]"""))
        == Just [ [ { when: Just (Or []), outcome: Stay } ] ]
    )
  p <- check "a nested not around a fact decodes as written"
    ( map (map _.cases <<< _.rules)
        (specOf (withRules """[{"from":"a","on":"e","cases":[{"when":{"not":{"not":{"fact":"f"}}},"stay":true}]}]"""))
        == Just [ [ { when: Just (Not (Not (Holds (FactOf (FactId "f"))))), outcome: Stay } ] ]
    )
  q <- check "a config reference whose name is not a string is refused"
    (rejects (withRules """[{"from":"a","on":"e","cases":[{"when":{"config":3},"stay":true}]}]"""))
  r <- check "a bare literal is a guard, and reads as Holds"
    ( map (map _.cases <<< _.rules) (specOf (withRules """[{"from":"a","on":"e","cases":[{"when":true,"stay":true}]}]"""))
        == Just [ [ { when: Just (Holds (Lit (VBoolean true))), outcome: Stay } ] ]
    )

  s <- check "a deadline with no in is refused"
    (rejects (withDeadlines """[{"fires":"e","after":2}]"""))
  t <- check "a deadline with no fires is refused"
    (rejects (withDeadlines """[{"in":"a","after":2}]"""))
  u <- check "a deadline with no after is refused"
    (rejects (withDeadlines """[{"in":"a","fires":"e"}]"""))
  v <- check "a deadline whose after is a string decodes, and then never falls due (documented)"
    ( maybe false (\sp -> dueAt sp (worldFrom sp) (sid "a") 0.0 == Nothing)
        (specOf (withDeadlines """[{"in":"a","fires":"e","after":"soon"}]"""))
    )

  pure [ a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v ]
  where
  withRules rules =
    """{"glassbox":1,"id":"m","initial":"a","states":[{"id":"a"},{"id":"b"}],"events":[{"id":"e"}],"refusals":[{"id":"r","text":"no"}],"rules":"""
      <> rules
      <> "}"
  withDeadlines ds =
    """{"glassbox":1,"id":"m","initial":"a","states":[{"id":"a"}],"events":[{"id":"e"}],"deadlines":"""
      <> ds
      <> "}"

-- =============================================================================
-- What the codec does NOT check
-- =============================================================================

-- | Phase 2 of the plan is five correctness lints, and none of them exist yet.
-- | Each of these artifacts is exactly the kind of hole the plan says must be
-- | "rejected, by name, with a message that points at the offending pair". Each
-- | one currently loads.
-- |
-- | They are asserted as ACCEPTED on purpose. When a lint lands, the
-- | corresponding line here goes red, and inverting it is then a deliberate act
-- | recorded in a diff rather than a silent change of contract.
unwrittenLintUnits :: Effect (Array Boolean)
unwrittenLintUnits = do
  section "Codec — the five correctness lints are NOT written yet (present behaviour)"

  a <- check "CLOSURE: a transition to a state that does not exist decodes fine"
    ( accepts
        """{"glassbox":1,"id":"m","initial":"a","states":[{"id":"a"}],"events":[{"id":"e"}],"rules":[{"from":"a","on":"e","cases":[{"to":"nowhere"}]}]}"""
    )
  b <- check "CLOSURE: and it runs, moving the machine to a state nothing declares"
    ( maybe false (\s -> resolve s (worldFrom s) (sid "a") (eid "e") == Move (StateId "nowhere"))
        ( specOf
            """{"glassbox":1,"id":"m","initial":"a","states":[{"id":"a"}],"events":[{"id":"e"}],"rules":[{"from":"a","on":"e","cases":[{"to":"nowhere"}]}]}"""
        )
    )
  c <- check "CLOSURE: an initial state that is not declared decodes fine"
    (accepts """{"glassbox":1,"id":"m","initial":"ghost","states":[{"id":"a"}]}""")
  d <- check "CLOSURE: a rule whose from is not a declared state decodes fine"
    ( accepts
        """{"glassbox":1,"id":"m","initial":"a","states":[{"id":"a"}],"events":[{"id":"e"}],"rules":[{"from":"ghost","on":"e","cases":[{"stay":true}]}]}"""
    )
  e <- check "CLOSURE: a rule on an event nobody declared decodes fine"
    ( accepts
        """{"glassbox":1,"id":"m","initial":"a","states":[{"id":"a"}],"rules":[{"from":"a","on":"phantom","cases":[{"stay":true}]}]}"""
    )
  f <- check "CLOSURE: a refusal tag that is not in the declared list decodes fine"
    ( accepts
        """{"glassbox":1,"id":"m","initial":"a","states":[{"id":"a"}],"events":[{"id":"e"}],"rules":[{"from":"a","on":"e","cases":[{"refuse":"untagged"}]}]}"""
    )
  g <- check "VOCABULARY: a guard reading a fact nobody declared decodes fine"
    ( accepts
        """{"glassbox":1,"id":"m","initial":"a","states":[{"id":"a"}],"events":[{"id":"e"}],"rules":[{"from":"a","on":"e","cases":[{"when":{"fact":"undeclared"},"stay":true}]}]}"""
    )
  h <- check "VOCABULARY: a guard reading a config nobody declared decodes fine"
    ( accepts
        """{"glassbox":1,"id":"m","initial":"a","states":[{"id":"a"}],"events":[{"id":"e"}],"rules":[{"from":"a","on":"e","cases":[{"when":{"config":"undeclared"},"stay":true}]}]}"""
    )
  i <- check "TOTALITY: a (state, event) pair with no rule decodes fine and silently stays"
    ( maybe false (\s -> resolve s (worldFrom s) (sid "a") (eid "e") == Stay)
        (specOf """{"glassbox":1,"id":"m","initial":"a","states":[{"id":"a"}],"events":[{"id":"e"}]}""")
    )
  j <- check "DETERMINISM: two rules for the same (from, on) decode fine"
    ( accepts
        """{"glassbox":1,"id":"m","initial":"a","states":[{"id":"a"},{"id":"b"}],"events":[{"id":"e"}],"rules":[{"from":"a","on":"e","cases":[{"to":"b"}]},{"from":"a","on":"e","cases":[{"stay":true}]}]}"""
    )
  k <- check "DETERMINISM: a case shadowed by an earlier catch-all decodes fine"
    ( accepts
        """{"glassbox":1,"id":"m","initial":"a","states":[{"id":"a"},{"id":"b"}],"events":[{"id":"e"}],"rules":[{"from":"a","on":"e","cases":[{"stay":true},{"to":"b"}]}]}"""
    )
  l <- check "duplicate state ids decode fine"
    (accepts """{"glassbox":1,"id":"m","initial":"a","states":[{"id":"a"},{"id":"a","label":"again"}]}""")
  m <- check "an empty machine — no states at all — decodes fine"
    (accepts """{"glassbox":1,"id":"m","initial":"a","states":[]}""")

  pure [ a, b, c, d, e, f, g, h, i, j, k, l, m ]

-- =============================================================================
-- Round trip, on a spec built in PureScript rather than read from disk
-- =============================================================================

-- | Every part of the format at once, including the parts the corpus does not
-- | happen to use.
everything :: Spec
everything = blank
  { version = 1
  , id = "everything"
  , title = "Everything: at once"
  , initial = sid "a"
  , states = [ st "a", finalSt "b" ]
  , events = [ ev "press", { id: EventId "tick", label: "tick", source: FromRuntime } ]
  , config = [ cfg "n" (num 3.0), cfg "on" (VBoolean true), cfg "band" (str "fm") ]
  , facts = [ fact "held" ]
  , refusals = [ refusal "busy" "something else has it" ]
  , rules =
      [ { from: sid "a"
        , on: eid "press"
        , cases:
            [ refuse (Just (Holds (FactOf (FactId "held")))) "busy"
            , to (Just (And [ Or [], Not (Cmp OpGe (ConfigOf (ConfigId "n")) (Lit (VNumber 1.0))) ])) "b"
            , stay always
            ]
        }
      , { from: sid "a", on: eid "tick", cases: [ to always "b" ] }
      ]
  , deadlines = [ { inState: sid "a", fires: eid "tick", after: ConfigOf (ConfigId "n") } ]
  }

roundTripUnits :: Effect (Array Boolean)
roundTripUnits = do
  section "Codec — the round trip carries every part of the format"

  a <- check "encode then decode is the identity on a spec using every construct"
    (decodeSpec (encodeSpec everything) == Right everything)
  b <- check "print then parse is too, so nothing is lost in the text"
    (parseSpec (printSpec everything) == Right everything)
  c <- check "and that includes the final flag, which is only written when set"
    ( map (map _.final <<< _.states) (hush (parseSpec (printSpec everything)))
        == Just [ false, true ]
    )
  d <- check "and the runtime event source, which is only meaningful when declared"
    ( map (map _.source <<< _.events) (hush (parseSpec (printSpec everything)))
        == Just [ FromUser, FromRuntime ]
    )
  e <- check "a title containing a colon survives the round trip untouched"
    (map _.title (hush (parseSpec (printSpec everything))) == Just "Everything: at once")
  f <- check "the re-encoded artifact is what printSpecPretty would show, so it parses"
    (isJust (specOf (printSpec everything)))
  g <- check "an empty refusal tag list still round-trips"
    (parseSpec (printSpec (everything { refusals = [], rules = [] })) == Right (everything { refusals = [], rules = [] }))

  pure [ a, b, c, d, e, f, g ]

-- | Prose is content, not a comment the codec may discard.
-- |
-- | Until 2026-08-23 the corpus carried seventy lines of design commentary in a
-- | `_note` key that `Spec` did not model, so every decode silently dropped it —
-- | and the behavioural round-trip test passed anyway, because it compares
-- | outcomes rather than bytes. A format meant to be authored and re-saved
-- | cannot lose the reasoning behind the thing it describes.
notesUnits :: Effect (Array Boolean)
notesUnits = do
  section "Codec.notes — prose survives a round trip"

  let withProse = blank { notes = [ "why this exists", "and a second: with a colon" ] }

  a <- check "notes decode from the wire"
    ( case parseSpec """{"glassbox":1,"id":"x","initial":"a","states":[{"id":"a"}],"notes":["hello"]}""" of
        Right sp -> sp.notes == [ "hello" ]
        Left _ -> false
    )
  b <- check "notes survive encode then decode"
    ( case decodeSpec (encodeSpec withProse) of
        Right back -> back.notes == withProse.notes
        Left _ -> false
    )
  c <- check "a machine with nothing to say emits no notes key at all"
    (not (contains (Pattern "notes") (printSpec blank)))
  d <- check "absent notes decode as empty rather than failing"
    ( case parseSpec """{"glassbox":1,"id":"x","initial":"a","states":[{"id":"a"}]}""" of
        Right sp -> sp.notes == []
        Left _ -> false
    )
  pure [ a, b, c, d ]

codecUnits :: Effect (Array Boolean)
codecUnits = do
  a <- envelopeUnits
  b <- declarationUnits
  c <- ruleUnits
  d <- unwrittenLintUnits
  e <- roundTripUnits
  f <- notesUnits
  pure (a <> b <> c <> d <> e <> f)
