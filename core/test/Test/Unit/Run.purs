-- | Test.Unit.Run
-- |
-- | `Glassbox.Run`, one function at a time.
-- |
-- | The corpus tests drive the runtime through five real machines, which proves
-- | the whole thing works and says nothing about which part of it is load
-- | bearing. These pin the individual decisions the module's comments claim —
-- | that a missing fact is not a false fact, that a number used as a condition
-- | is a mistake rather than a truthy value, that ordering across kinds is false
-- | rather than constructor order — because each of those is a decision a second
-- | implementation has to copy, and a decision nobody tested is a decision
-- | nobody has to keep.
module Test.Unit.Run
  ( runUnits
  ) where

import Prelude

import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Glassbox.Run
  ( World
  , alarmFor
  , dueAt
  , evalExpr
  , evalGuard
  , initialStep
  , lookupConfig
  , lookupFact
  , resolve
  , setConfig
  , setFact
  , step
  , worldFrom
  )
import Glassbox.Spec
  ( Clock(..)
  , clockFromName
  , clockName
  , everyClock
  , millisecondsPer
  , CmpOp(..)
  , ConfigId(..)
  , EventId(..)
  , Expr(..)
  , FactId(..)
  , Guard(..)
  , Outcome(..)
  , RefusalId(..)
  , Spec
  , StateId(..)
  , Value(..)
  )
import Test.Fixture
  ( always
  , blank
  , bool
  , cfg
  , eid
  , ev
  , fact
  , num
  , refusal
  , refuse
  , sid
  , st
  , stay
  , str
  , to
  )
import Test.Harness (check, section)

-- =============================================================================
-- The world a guard reads
-- =============================================================================

-- | One config and one fact, both present; everything else deliberately absent
-- | so that a lookup of it is a genuine miss.
world :: World
world =
  setFact (FactId "loaded") (VBoolean true)
    (setConfig (ConfigId "bands") (VNumber 3.0) (worldFrom blank))

evalExprUnits :: Effect (Array Boolean)
evalExprUnits = do
  section "Run.evalExpr — a miss is a miss, never a default"

  a <- check "a literal evaluates to itself"
    (evalExpr world (Lit (VNumber 4.0)) == Just (VNumber 4.0))
  b <- check "a config that is set evaluates to its value"
    (evalExpr world (ConfigOf (ConfigId "bands")) == Just (VNumber 3.0))
  c <- check "a fact that is set evaluates to its value"
    (evalExpr world (FactOf (FactId "loaded")) == Just (VBoolean true))

  -- The two that matter. `Nothing` is not `Just (VBoolean false)`, and the
  -- module comment says why: a machine that reads a missing fact as false takes
  -- a branch nobody authored.
  d <- check "a config nobody set is Nothing, not a default"
    (evalExpr world (ConfigOf (ConfigId "nope")) == Nothing)
  e <- check "a fact nobody reported is Nothing, not false"
    (evalExpr world (FactOf (FactId "nope")) == Nothing)

  let declared = worldFrom (blank { config = [ cfg "bands" (num 2.0) ] })
  f <- check "worldFrom seeds every declared config with its declared default"
    (lookupConfig declared (ConfigId "bands") == Just (VNumber 2.0))
  g <- check "worldFrom reports no facts at all"
    (lookupFact declared (FactId "loaded") == Nothing)
  h <- check "config and facts are separate halves that never see each other"
    (lookupFact world (FactId "bands") == Nothing && lookupConfig world (ConfigId "loaded") == Nothing)

  pure [ a, b, c, d, e, f, g, h ]

-- =============================================================================
-- Guards
-- =============================================================================

holdsUnits :: Effect (Array Boolean)
holdsUnits = do
  section "Run.evalGuard — Holds is boolean or nothing"

  a <- check "Holds on a true boolean holds"
    (evalGuard world (Holds (Lit (VBoolean true))))
  b <- check "Holds on a false boolean does not"
    (not (evalGuard world (Holds (Lit (VBoolean false)))))

  -- A number or a string in a condition is an artifact bug, and reading it as
  -- false is what leaves something for the vocabulary lint to catch.
  c <- check "Holds on a non-zero number is false, not truthy"
    (not (evalGuard world (Holds (Lit (VNumber 1.0)))))
  d <- check "Holds on zero is false too — not the C rule, no rule"
    (not (evalGuard world (Holds (Lit (VNumber 0.0)))))
  e <- check "Holds on the string \"true\" is false"
    (not (evalGuard world (Holds (Lit (VString "true")))))
  f <- check "Holds on the empty string is false"
    (not (evalGuard world (Holds (Lit (VString "")))))

  g <- check "Holds on a missing fact is false"
    (not (evalGuard world (Holds (FactOf (FactId "nope")))))
  h <- check "Holds on a missing config is false"
    (not (evalGuard world (Holds (ConfigOf (ConfigId "nope")))))
  i <- check "Holds on a fact that is set reads it"
    (evalGuard world (Holds (FactOf (FactId "loaded"))))
  j <- check "Holds on a config holding a number is false, however set it is"
    (not (evalGuard world (Holds (ConfigOf (ConfigId "bands")))))

  pure [ a, b, c, d, e, f, g, h, i, j ]

connectiveUnits :: Effect (Array Boolean)
connectiveUnits = do
  section "Run.evalGuard — Not, And, Or, including the empty ones"

  let yes = Holds (Lit (VBoolean true))
  let no = Holds (Lit (VBoolean false))

  a <- check "Not inverts a true guard" (not (evalGuard world (Not yes)))
  b <- check "Not inverts a false guard" (evalGuard world (Not no))
  c <- check "Not is an involution" (evalGuard world (Not (Not yes)))

  -- The identities. An empty conjunction is true and an empty disjunction is
  -- false; anything else and a branch that generates no operands changes the
  -- meaning of the branch above it.
  d <- check "And of nothing is true — the identity of conjunction"
    (evalGuard world (And []))
  e <- check "Or of nothing is false — the identity of disjunction"
    (not (evalGuard world (Or [])))

  f <- check "And of all true holds" (evalGuard world (And [ yes, yes, yes ]))
  g <- check "And with one false does not" (not (evalGuard world (And [ yes, no, yes ])))
  h <- check "And of a single false does not" (not (evalGuard world (And [ no ])))
  i <- check "Or with one true holds" (evalGuard world (Or [ no, yes, no ]))
  j <- check "Or of all false does not" (not (evalGuard world (Or [ no, no ])))
  k <- check "Or of a single true holds" (evalGuard world (Or [ yes ]))

  l <- check "nesting composes as written"
    (evalGuard world (And [ Or [ no, yes ], Not (And [ yes, no ]) ]))
  m <- check "Not of an empty And is false, and Not of an empty Or is true"
    (not (evalGuard world (Not (And []))) && evalGuard world (Not (Or [])))

  pure [ a, b, c, d, e, f, g, h, i, j, k, l, m ]

comparisonUnits :: Effect (Array Boolean)
comparisonUnits = do
  section "Run.evalGuard — all six comparisons"

  let numE x = Lit (VNumber x)
  let strE x = Lit (VString x)
  let boolE x = Lit (VBoolean x)
  let cmp op l r = evalGuard world (Cmp op l r)

  a <- check "eq on equal numbers"
    (cmp OpEq (numE 3.0) (numE 3.0) && not (cmp OpEq (numE 3.0) (numE 4.0)))
  b <- check "ne on unequal numbers"
    (cmp OpNe (numE 3.0) (numE 4.0) && not (cmp OpNe (numE 3.0) (numE 3.0)))
  c <- check "lt is strict"
    ( cmp OpLt (numE 3.0) (numE 4.0)
        && not (cmp OpLt (numE 3.0) (numE 3.0))
        && not (cmp OpLt (numE 4.0) (numE 3.0))
    )
  d <- check "le admits equality"
    ( cmp OpLe (numE 3.0) (numE 4.0)
        && cmp OpLe (numE 3.0) (numE 3.0)
        && not (cmp OpLe (numE 4.0) (numE 3.0))
    )
  e <- check "gt is strict"
    ( cmp OpGt (numE 4.0) (numE 3.0)
        && not (cmp OpGt (numE 3.0) (numE 3.0))
        && not (cmp OpGt (numE 3.0) (numE 4.0))
    )
  f <- check "ge admits equality"
    ( cmp OpGe (numE 4.0) (numE 3.0)
        && cmp OpGe (numE 3.0) (numE 3.0)
        && not (cmp OpGe (numE 3.0) (numE 4.0))
    )

  g <- check "strings order lexicographically"
    (cmp OpLt (strE "am") (strE "fm") && cmp OpGt (strE "fm") (strE "am") && cmp OpEq (strE "fm") (strE "fm"))
  h <- check "booleans order false before true"
    (cmp OpLt (boolE false) (boolE true) && cmp OpGe (boolE true) (boolE true))

  -- Equality is structural, so it is defined across kinds and answers false.
  i <- check "eq across kinds is false, not an error"
    (not (cmp OpEq (numE 1.0) (strE "1")) && not (cmp OpEq (boolE true) (strE "true")))
  j <- check "ne across kinds is true, because they really are not equal"
    (cmp OpNe (numE 1.0) (strE "1"))

  -- Ordering is not. `Value` derives `Ord`, so `VNumber _ < VString _` is a
  -- perfectly well-defined answer to a question nobody asked; the module
  -- refuses to give it, and BOTH directions must be false or the refusal is
  -- just an unwritten preference for one side.
  k <- check "lt across kinds is false, not constructor order"
    (not (cmp OpLt (numE 1.0) (strE "1")))
  l <- check "gt across kinds is false in the other direction too"
    ( not (cmp OpGt (numE 1.0) (strE "1"))
        && not (cmp OpGt (strE "1") (numE 1.0))
        && not (cmp OpLt (strE "1") (numE 1.0))
    )
  m <- check "le and ge across kinds are false as well"
    (not (cmp OpLe (boolE true) (numE 1.0)) && not (cmp OpGe (boolE true) (numE 1.0)))

  -- A missing operand makes the whole comparison false, and that includes `ne`
  -- — "a fact nobody reported differs from 3" is not a claim this can make.
  n <- check "a comparison with a missing left operand is false"
    (not (cmp OpEq (FactOf (FactId "nope")) (numE 3.0)))
  o <- check "so is one with a missing right operand"
    (not (cmp OpLt (numE 3.0) (ConfigOf (ConfigId "nope"))))
  p <- check "ne with a missing operand is false too, not vacuously true"
    (not (cmp OpNe (FactOf (FactId "nope")) (numE 3.0)))

  q <- check "a comparison reads config and facts"
    (cmp OpGt (ConfigOf (ConfigId "bands")) (numE 2.0) && cmp OpEq (FactOf (FactId "loaded")) (boolE true))

  pure [ a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q ]

-- =============================================================================
-- resolve
-- =============================================================================

-- | Three states and every case ordering that matters.
ordered :: Spec
ordered = blank
  { states = [ st "a", st "b", st "c" ]
  , events = [ ev "go", ev "orphan" ]
  , facts = [ fact "flag" ]
  , config = [ cfg "on" (bool true) ]
  , refusals = [ refusal "nope" "no" ]
  , rules =
      [ { from: sid "a"
        , on: eid "go"
        , cases:
            [ to (Just (Holds (FactOf (FactId "flag")))) "b"
            , to always "c"
            ]
        }
      -- Every case is guarded, and none of them need hold.
      , { from: sid "b"
        , on: eid "go"
        , cases: [ to (Just (Holds (FactOf (FactId "flag")))) "c" ]
        }
      , { from: sid "c"
        , on: eid "go"
        , cases:
            [ refuse (Just (Not (Holds (ConfigOf (ConfigId "on"))))) "nope"
            , stay always
            ]
        }
      ]
  }

resolveUnits :: Effect (Array Boolean)
resolveUnits = do
  section "Run.resolve — first match wins, and everything else is Stay"

  let base = worldFrom ordered
  let flagged = setFact (FactId "flag") (VBoolean true) base

  a <- check "the first case whose guard holds wins, not the most specific"
    (resolve ordered flagged (sid "a") (eid "go") == Move (StateId "b"))
  b <- check "when it does not hold, the catch-all behind it is taken"
    (resolve ordered base (sid "a") (eid "go") == Move (StateId "c"))

  -- Order is the determinism mechanism, so a later case can be shadowed by an
  -- earlier one. That is a lint's problem, not a soundness problem.
  c <- check "the two orderings really do give different answers"
    ( resolve ordered flagged (sid "a") (eid "go")
        /= resolve ordered base (sid "a") (eid "go")
    )

  d <- check "a rule whose every guard fails yields Stay"
    (resolve ordered base (sid "b") (eid "go") == Stay)
  e <- check "the same rule fires once the guard holds"
    (resolve ordered flagged (sid "b") (eid "go") == Move (StateId "c"))

  f <- check "a (state, event) with no rule at all yields Stay"
    (resolve ordered base (sid "a") (eid "orphan") == Stay)
  g <- check "a state that does not exist yields Stay rather than failing"
    (resolve ordered base (sid "ghost") (eid "go") == Stay)
  h <- check "an event that does not exist yields Stay rather than failing"
    (resolve ordered base (sid "a") (eid "ghost") == Stay)
  i <- check "a rule with no cases at all yields Stay"
    (resolve (blank { rules = [ { from: sid "a", on: eid "go", cases: [] } ] }) base (sid "a") (eid "go") == Stay)

  j <- check "a guard reading config decides the outcome"
    (resolve ordered base (sid "c") (eid "go") == Stay)
  k <- check "and flipping that config changes it to a refusal"
    ( resolve ordered (setConfig (ConfigId "on") (VBoolean false) base) (sid "c") (eid "go")
        == Refuse (RefusalId "nope")
    )

  -- `ruleFor` is `find`, so a duplicated (from, on) silently resolves through
  -- the first. Not a defect — the same determinism-by-ordering the cases use —
  -- but undeclared, and an implementation backed by a map would take the last.
  let dup = ordered { rules = ordered.rules <> [ { from: sid "a", on: eid "go", cases: [ stay always ] } ] }
  l <- check "a duplicated (from, on) resolves through the FIRST rule, not the last"
    (resolve dup base (sid "a") (eid "go") == Move (StateId "c"))

  pure [ a, b, c, d, e, f, g, h, i, j, k, l ]

-- =============================================================================
-- step, initialStep, dueAt
-- =============================================================================

stepUnits :: Effect (Array Boolean)
stepUnits = do
  section "Run.step and Run.dueAt"

  let base = worldFrom ordered
  let flagged = setFact (FactId "flag") (VBoolean true) base

  let Tuple moved rec1 = step ordered flagged (sid "a") (eid "go")
  a <- check "a move carries the machine and records where it came from"
    ( moved == StateId "b"
        && rec1 ==
          { from: StateId "a"
          , event: EventId "go"
          , outcome: Move (StateId "b")
          , current: StateId "b"
          , commands: []
          }
    )

  let Tuple stayed rec2 = step ordered base (sid "b") (eid "go")
  b <- check "a Stay leaves the machine where it was"
    (stayed == StateId "b" && rec2.current == StateId "b" && rec2.outcome == Stay)

  let Tuple refused rec3 = step ordered (setConfig (ConfigId "on") (VBoolean false) base) (sid "c") (eid "go")
  c <- check "a refusal also leaves it where it was, but says so in the outcome"
    (refused == StateId "c" && rec3.outcome == Refuse (RefusalId "nope"))

  d <- check "initialStep sits at the initial state having done nothing"
    ( initialStep ordered (eid "go")
        ==
          { from: StateId "a"
          , event: EventId "go"
          , outcome: Stay
          , current: StateId "a"
          , commands: []
          }
    )

  let timed = ordered
        { config = ordered.config <> [ cfg "wait" (num 5.0), cfg "wordy" (str "soon") ]
        , deadlines =
            [ { inState: sid "a", fires: eid "go", after: Lit (VNumber 2.0) }
            , { inState: sid "b", fires: eid "go", after: ConfigOf (ConfigId "wait") }
            , { inState: sid "c", fires: eid "go", after: ConfigOf (ConfigId "wordy") }
            ]
        }
  let tw = worldFrom timed

  e <- check "a literal deadline falls due relative to when the state was entered"
    (dueAt timed tw (sid "a") 10.0 == Just { fires: EventId "go", at: 12.0 })
  f <- check "a config-driven deadline reads the world"
    (dueAt timed tw (sid "b") 0.0 == Just { fires: EventId "go", at: 5.0 })
  g <- check "changing that config moves the deadline"
    ( dueAt timed (setConfig (ConfigId "wait") (VNumber 1.0) tw) (sid "b") 0.0
        == Just { fires: EventId "go", at: 1.0 }
    )
  h <- check "a deadline whose quantity is not a number is Nothing"
    (dueAt timed tw (sid "c") 0.0 == Nothing)
  i <- check "a state with no deadline is Nothing"
    (dueAt ordered base (sid "a") 0.0 == Nothing)
  j <- check "a state that does not exist is Nothing"
    (dueAt timed tw (sid "ghost") 0.0 == Nothing)

  -- alarmFor is dueAt's replacement at the seam, and the difference is the
  -- whole point: it hands back HOW LONG and IN WHAT, so the conversion into a
  -- host's own clock happens once, where the unit is still in scope, instead
  -- of being assumed at both ends of an addition.
  let ticking = timed { clock = Just Seconds }

  k <- check "an alarm is relative and carries the machine's clock"
    ( alarmFor ticking tw (sid "a")
        == Just { fires: EventId "go", after: 2.0, clock: Just Seconds }
    )
  l <- check "and reads the world for its quantity, exactly as dueAt does"
    ( map _.after (alarmFor ticking (setConfig (ConfigId "wait") (VNumber 1.0) tw) (sid "b"))
        == Just 1.0
    )
  m <- check "a state that asks for nothing asks for no alarm"
    (alarmFor ordered base (sid "a") == Nothing)

  -- The two clocks that do NOT convert are the honest part of the design: a
  -- beat is not a duration until somebody says what the tempo is, and that is
  -- not the machine's to know.
  n <- check "wall-clock units convert to milliseconds; musical and display ones do not"
    ( map millisecondsPer [ Milliseconds, Seconds, Minutes, Beats, Frames ]
        == [ Just 1.0, Just 1000.0, Just 60000.0, Nothing, Nothing ]
    )
  o <- check "every clock the format knows is in everyClock, and round-trips its name"
    ( length everyClock == 5
        && map clockFromName (map clockName everyClock) == map Just everyClock
    )

  pure [ a, b, c, d, e, f, g, h, i, j, k, l, m, n, o ]

runUnits :: Effect (Array Boolean)
runUnits = do
  a <- evalExprUnits
  b <- holdsUnits
  c <- connectiveUnits
  d <- comparisonUnits
  e <- resolveUnits
  f <- stepUnits
  pure (a <> b <> c <> d <> e <> f)
