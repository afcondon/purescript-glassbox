-- | Glassbox.DSL.Parse
-- |
-- | Read the Blocks syntax back into a `Spec`.
-- |
-- | The other half of `Glassbox.DSL.Print`. Blocks is the **authoring** format —
-- | the thing a person opens and edits — while JSON stays the wire format that
-- | hashes, that a second runtime agrees to, and that the conformance test
-- | drives. Both decode to the same value, which is the only reason it is safe
-- | to have two.
-- |
-- | ### What round-trips, and what does not
-- |
-- | `parse (print s)` recovers a machine that **behaves** identically to `s` and
-- | carries the same declarations, prose and deadlines. It does **not**
-- | necessarily recover `s.rules` in the same array order, because the printer
-- | groups events that share a case list and grouping is not order-preserving
-- | across interleaved rules. That is harmless — rule order is behaviourally
-- | inert except for duplicate `(from, on)` pairs, which the format should
-- | reject rather than resolve — and the property that actually matters holds:
-- | printing is **idempotent through a parse**, so `print (parse (print s))`
-- | equals `print s` and an author's file is stable under a round trip.
-- |
-- | ### Two passes in one
-- |
-- | Config and facts share one notation inside a guard, because
-- | `converter-held` reads and `fact:converter-held` does not. So a bare name is
-- | resolved against the declarations, which is why the block body is parsed
-- | with the declarations already in hand rather than in a separate sweep. A
-- | name declared as both is printed qualified and parsed qualified; a name
-- | declared as neither is a parse error rather than a guess. An `entry` or
-- | `exit` line naming an undeclared command is refused on the same principle.
module Glassbox.DSL.Parse
  ( parseBlocks
  , parseBlocksWith
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Data.Array (concat, elem, many, snoc, some)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (any, foldl)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Number as Number
import Data.String (Pattern(..), stripPrefix)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Glassbox.Spec
  ( Case
  , Clock
  , clockFromName
  , CmpOp(..)
  , CommandDecl
  , CommandId(..)
  , ConfigDecl
  , ConfigId(..)
  , Deadline
  , EventDecl
  , EventId(..)
  , EventSource(..)
  , Expr(..)
  , FactDecl
  , FactId(..)
  , Guard(..)
  , Outcome(..)
  , RefusalDecl
  , RefusalId(..)
  , Rule
  , Spec
  , StateDecl
  , StateId(..)
  , Value(..)
  )
import Parsing (ParseError, Parser, Position(..), fail, parseErrorMessage, parseErrorPosition, runParser)
import Parsing.Combinators (between, lookAhead, notFollowedBy, option, optionMaybe, sepBy1, try)
import Parsing.String (char, satisfy, string)
import Parsing.String.Basic (oneOf)

-- | Parse a Blocks document. `Left` carries a message with a line and column.
parseBlocks :: String -> Either String Spec
parseBlocks input = lmap render (parseBlocksWith input)
  where
  render e = case parseErrorPosition e of
    Position p -> show p.line <> ":" <> show p.column <> " " <> parseErrorMessage e

parseBlocksWith :: String -> Either ParseError Spec
parseBlocksWith input = runParser input document

-- =============================================================================
-- Whitespace, which is load-bearing here
-- =============================================================================

-- | Horizontal space only. Newlines are structure in this format and must never
-- | be eaten by a lexeme.
hspace :: Parser String Unit
hspace = void (many (oneOf [ ' ', '\t' ]))

lexeme :: forall a. Parser String a -> Parser String a
lexeme p = p <* hspace

symbol :: String -> Parser String Unit
symbol s = void (lexeme (string s))

eol :: Parser String Unit
eol = void (char '\n') <|> pure unit <* lookAhead endOfInput

endOfInput :: Parser String Unit
endOfInput = notFollowedBy (satisfy (const true))

-- | A line that is entirely blank.
blankLine :: Parser String Unit
blankLine = try (hspace *> char '\n' $> unit)

blanks :: Parser String Unit
blanks = void (many blankLine)

-- =============================================================================
-- Tokens
-- =============================================================================

identChar :: Parser String Char
identChar = satisfy \c ->
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
    || c == '-'
    || c == '_'
    || c == '.'

-- | Kebab-case names, as everything in the corpus uses.
ident :: Parser String String
ident = lexeme (fromCharArray <$> some identChar)

-- | An identifier that is not one of the words the grammar reserves in this
-- | position. Without this, `stop` as an event name would be fine but `stay` as
-- | one would silently parse as the outcome keyword.
identBut :: Array String -> Parser String String
identBut reserved = try do
  s <- ident
  if elem s reserved then fail ("unexpected keyword " <> s) else pure s

keyword :: String -> Parser String Unit
keyword s = void (try (lexeme (string s <* notFollowedBy identChar)))

-- | Quoted, with backslash escapes, matching what `Print.quote` emits.
quoted :: Parser String String
quoted = lexeme (between (char '"') (char '"') (fromCharArray <$> many stringChar))
  where
  stringChar = escaped <|> satisfy (\c -> c /= '"' && c /= '\\')
  escaped = char '\\' *> (char '"' <|> char '\\')

number :: Parser String Number
number = do
  raw <- lexeme (fromCharArray <$> some (satisfy \c -> (c >= '0' && c <= '9') || c == '-' || c == '.' || c == 'e' || c == '+'))
  case Number.fromString raw of
    Just n -> pure n
    Nothing -> fail ("not a number: " <> raw)

int :: Parser String Int
int = do
  n <- number
  case Int.fromNumber n of
    Just i -> pure i
    Nothing -> fail "not a whole number"

-- | Everything to the end of the line, trimmed of nothing — comments keep their
-- | own spacing so re-printing is stable.
restOfLine :: Parser String String
restOfLine = fromCharArray <$> many (satisfy (_ /= '\n'))

-- =============================================================================
-- The document
-- =============================================================================

type Decls =
  { states :: Array StateDecl
  , events :: Array EventDecl
  , config :: Array ConfigDecl
  , facts :: Array FactDecl
  , refusals :: Array RefusalDecl
  , commands :: Array CommandDecl
  }

emptyDecls :: Decls
emptyDecls = { states: [], events: [], config: [], facts: [], refusals: [], commands: [] }

document :: Parser String Spec
document = do
  blanks
  notes <- many commentLine
  blanks
  Tuple id title <- machineLine
  version <- option 1 formatLine
  clock <- optionMaybe clockLine
  initial <- initialLine
  decls <- declarationSections
  blocks <- many (stateBlock decls)
  blanks
  endOfInput
  pure
    { version
    , id
    , notes
    , title: fromMaybe id title
    , initial
    , states: map (fromBlocks blocks) decls.states
    , events: decls.events
    , config: decls.config
    , facts: decls.facts
    , refusals: decls.refusals
    , commands: decls.commands
    , rules: concat (map _.rules blocks)
    , deadlines: concat (map _.deadlines blocks)
    , clock
    }

-- | Fold what a block says about its own state back onto the declaration.
-- |
-- | `final`, `entry` and `exit` are all written on the block rather than in the
-- | `states` section, so that a reader learns everything about a state where
-- | they are already looking and the two places can never contradict each
-- | other. The cost is this fold. Commands from several blocks naming the same
-- | state accumulate in block order, which is the only reading that does not
-- | silently discard one of them.
fromBlocks :: Array Block -> StateDecl -> StateDecl
fromBlocks blocks decl = decl
  { final = decl.final || any (\b -> b.from == decl.id && b.final) blocks
  , entry = decl.entry <> concat (map _.entry (mine))
  , exit = decl.exit <> concat (map _.exit (mine))
  }
  where
  mine = Array.filter (\b -> b.from == decl.id) blocks

-- | `#` to the end of the line. One space after the hash is the printer's
-- | doing, so it comes back off; anything further in is the author's.
commentLine :: Parser String String
commentLine = try do
  hspace
  void (char '#')
  body <- restOfLine
  eol
  -- The printer writes "# " before an author's text; one space comes back off
  -- so that a comment survives print → parse → print unchanged.
  pure (fromMaybe body (stripPrefix (Pattern " ") body))

machineLine :: Parser String (Tuple String (Maybe String))
machineLine = do
  hspace
  keyword "machine"
  id <- ident
  title <- optionMaybe quoted
  eol
  blanks
  pure (Tuple id title)

formatLine :: Parser String Int
formatLine = try do
  hspace
  keyword "format"
  v <- int
  eol
  blanks
  pure v

-- | `clock seconds`. Sits with the header rather than beside the deadlines
-- | because it governs all of them, and a per-deadline unit is exactly the
-- | freedom that let the corpus mix three of them.
clockLine :: Parser String Clock
clockLine = try do
  hspace
  keyword "clock"
  name <- ident
  c <- maybe (fail ("not a clock this format knows: " <> name)) pure (clockFromName name)
  eol
  blanks
  pure c

initialLine :: Parser String StateId
initialLine = do
  hspace
  keyword "initial"
  s <- ident
  eol
  blanks
  pure (StateId s)

-- | The five declaration sections, in any order and any of them absent.
declarationSections :: Parser String Decls
declarationSections = foldl merge emptyDecls <$> many section
  where
  section =
    (sectionOf "states" stateRow <#> \xs -> emptyDecls { states = xs })
      <|> (sectionOf "events" eventRow <#> \xs -> emptyDecls { events = xs })
      <|> (sectionOf "config" configRow <#> \xs -> emptyDecls { config = xs })
      <|> (sectionOf "facts" factRow <#> \xs -> emptyDecls { facts = xs })
      <|> (sectionOf "refusals" refusalRow <#> \xs -> emptyDecls { refusals = xs })
      <|> (sectionOf "commands" commandRow <#> \xs -> emptyDecls { commands = xs })

  merge acc d = acc
    { states = acc.states <> d.states
    , events = acc.events <> d.events
    , config = acc.config <> d.config
    , facts = acc.facts <> d.facts
    , refusals = acc.refusals <> d.refusals
    , commands = acc.commands <> d.commands
    }

sectionOf :: forall a. String -> Parser String a -> Parser String (Array a)
sectionOf name row = try do
  hspace
  keyword name
  eol
  rows <- many (try (indented row))
  blanks
  pure rows

-- | A row inside a section: indented, one per line.
indented :: forall a. Parser String a -> Parser String a
indented p = do
  void (some (oneOf [ ' ', '\t' ]))
  a <- p
  eol
  pure a

-- | A label equal to the id prints as nothing, so an absent label means the id.
-- | `final`, `entry` and `exit` are absent here on purpose: they are written on
-- | the state's own block and folded back on by `fromBlocks`.
stateRow :: Parser String StateDecl
stateRow = do
  id <- ident
  label <- optionMaybe quoted
  pure { id: StateId id, label: fromMaybe id label, final: false, entry: [], exit: [] }

eventRow :: Parser String EventDecl
eventRow = do
  id <- ident
  src <- option FromUser (keyword "user" $> FromUser <|> keyword "runtime" $> FromRuntime)
  label <- optionMaybe quoted
  pure { id: EventId id, label: fromMaybe id label, source: src }

configRow :: Parser String ConfigDecl
configRow = do
  id <- ident
  symbol "="
  def <- literal
  label <- optionMaybe quoted
  pure { id: ConfigId id, label: fromMaybe id label, default: def }

factRow :: Parser String FactDecl
factRow = do
  id <- ident
  label <- optionMaybe quoted
  pure { id: FactId id, label: fromMaybe id label }

refusalRow :: Parser String RefusalDecl
refusalRow = do
  id <- ident
  text <- optionMaybe quoted
  pure { id: RefusalId id, text: fromMaybe id text }

commandRow :: Parser String CommandDecl
commandRow = do
  id <- ident
  label <- optionMaybe quoted
  pure { id: CommandId id, label: fromMaybe id label }

-- =============================================================================
-- State blocks
-- =============================================================================

-- | `final` is declared on the state's block header rather than in the `states`
-- | section — saying it once is what keeps the two from contradicting each
-- | other — so a block has to carry it back out to be folded into the
-- | declaration it belongs to.
type Block =
  { from :: StateId
  , final :: Boolean
  , entry :: Array CommandId
  , exit :: Array CommandId
  , rules :: Array Rule
  , deadlines :: Array Deadline
  }

-- | One line inside a block. Four shapes, and the fourth is a line the printer
-- | wrote to complain about the artifact, which is read back as nothing at all.
data BodyLine
  = ARule Group
  | AnEntry (Array CommandId)
  | AnExit (Array CommandId)
  | Ignored

stateBlock :: Decls -> Parser String Block
stateBlock decls = do
  blanks
  hspace
  from <- identBut [ "states", "events", "config", "facts", "refusals", "commands" ]
  void (char ':')
  hspace
  mods <- blockModifiers decls
  eol
  lines <- many bodyLine
  blanks
  pure
    { from: StateId from
    , final: mods.final
    , entry: concat (Array.mapMaybe entryOfLine lines)
    , exit: concat (Array.mapMaybe exitOfLine lines)
    , rules: concat (map (toRules (StateId from)) (Array.mapMaybe groupOfLine lines))
    , deadlines: map (\d -> d { inState = StateId from }) mods.deadlines
    }
  where
  toRules from group = map (\e -> { from, on: e, cases: group.cases }) group.events

  groupOfLine = case _ of
    ARule g -> Just g
    _ -> Nothing

  entryOfLine = case _ of
    AnEntry cs -> Just cs
    _ -> Nothing

  exitOfLine = case _ of
    AnExit cs -> Just cs
    _ -> Nothing

  -- The printer emits `!! …` inside a block for things the format tolerates
  -- but should not — a state with no rules, a group with no cases. Reading
  -- them back as nothing is what stops a round trip destroying the artifact
  -- the marker was warning about.
  bodyLine =
    (AnEntry <$> try (hookLine "entry"))
      <|> (AnExit <$> try (hookLine "exit"))
      <|> (ARule <$> try (groupLine decls))
      <|> (Ignored <$ try bodyMarker)

  -- `entry` and `exit` name commands, which are declared, so an undeclared name
  -- here is refused rather than invented — the same rule a bare name in a guard
  -- obeys, and for the same reason.
  hookLine word = do
    void (some (oneOf [ ' ', '\t' ]))
    keyword word
    cs <- commandList decls
    eol
    pure cs

  bodyMarker = do
    void (some (oneOf [ ' ', '\t' ]))
    void (string "!!")
    void restOfLine
    eol

commandList :: Decls -> Parser String (Array CommandId)
commandList decls = do
  first <- name
  rest <- many (try (symbol "," *> name))
  pure ([ first ] <> rest)
  where
  name = do
    n <- ident
    if isJust (Array.find (\c -> c.id == CommandId n) decls.commands) then pure (CommandId n)
    else fail (n <> " is not a declared command")

type Modifiers =
  { final :: Boolean
  , deadlines :: Array Deadline
  }

-- | `final`, any number of `after … fires …`, and the printer's `!! not
-- | declared` marker, comma-separated on the header line.
blockModifiers :: Decls -> Parser String Modifiers
blockModifiers decls =
  option { final: false, deadlines: [] } (foldl add { final: false, deadlines: [] } <$> mods)
  where
  mods = Array.fromFoldable <$> sepBy1 modifier (symbol ",")

  modifier =
    (keyword "final" $> { final: true, deadline: Nothing })
      <|> (marker $> { final: false, deadline: Nothing })
      <|> (deadline decls <#> \d -> { final: false, deadline: Just d })

  -- The printer emits `!! …` for things the format tolerates but should not.
  -- Reading them back keeps a round trip from destroying the very artifact the
  -- markers were warning about.
  marker = try (symbol "!!" *> void restOfLine)

  add acc m = acc
    { final = acc.final || m.final
    , deadlines = maybe acc.deadlines (snoc acc.deadlines) m.deadline
    }

deadline :: Decls -> Parser String Deadline
deadline decls = do
  keyword "after"
  after <- expr decls
  keyword "fires"
  ev <- ident
  pure { inState: StateId "", fires: EventId ev, after }

type Group =
  { events :: Array EventId
  , cases :: Array Case
  }

-- | One group: an event list, then either a single unguarded outcome on the
-- | same line or a `when` / `elif` / `else` alternation continuing on the next.
groupLine :: Decls -> Parser String Group
groupLine decls = do
  void (some (oneOf [ ' ', '\t' ]))
  notFollowedBy (string "!!")
  events <- eventList
  cases <- guardedCases decls <|> (singleOutcome <#> \o -> [ { when: Nothing, outcome: o } ])
  pure { events, cases }

eventList :: Parser String (Array EventId)
eventList = do
  first <- name
  rest <- many (try (symbol "," *> name))
  pure (map EventId ([ first ] <> rest))
  where
  -- `entry` and `exit` open a command line, so they cannot also open an event
  -- list. The cost is two event names; the alternative is a line whose meaning
  -- depends on whether a command of that name happens to be declared.
  name = identBut [ "when", "elif", "else", "entry", "exit" ]

singleOutcome :: Parser String Outcome
singleOutcome = do
  o <- outcome
  eol
  pure o

-- | The alternation. `when` opens it, `elif` continues, `else` closes it with
-- | the catch-all — which is what makes an ordering mistake catchable here
-- | rather than silent.
guardedCases :: Decls -> Parser String (Array Case)
guardedCases decls = do
  firstG <- try (keyword "when" *> guard decls)
  firstO <- outcome
  eol
  rest <- many (try (continuation decls))
  pure ([ { when: Just firstG, outcome: firstO } ] <> rest)

continuation :: Decls -> Parser String Case
continuation decls = do
  void (some (oneOf [ ' ', '\t' ]))
  c <- elifCase <|> elseCase
  eol
  pure c
  where
  elifCase = do
    g <- try (keyword "elif" *> guard decls)
    o <- outcome
    pure { when: Just g, outcome: o }
  elseCase = do
    keyword "else"
    o <- outcome
    pure { when: Nothing, outcome: o }

outcome :: Parser String Outcome
outcome = do
  symbol "->"
  (keyword "stay" $> Stay)
    <|> (keyword "refuse" *> (Refuse <<< RefusalId <$> ident))
    <|> (Move <<< StateId <$> ident)

-- =============================================================================
-- Guards
-- =============================================================================

-- | The precedence ladder, as mutually recursive top-level functions.
-- |
-- | Not a `where` block: the ladder closes on itself, and PureScript allows a
-- | cycle between *function* bindings but not between value ones. Written as
-- | functions of `decls` it is both legal and easier to read a level at a time.
-- |
-- | The two re-entrant references are wrapped in `defer`. PureScript is strict,
-- | so a parser that mentions itself on the right-hand side builds itself
-- | forever at *construction* time — the recursion has to be delayed even
-- | though, once built, it cannot loop while parsing (the only way back to the
-- | bottom of the ladder is behind an opening bracket that must be consumed).
guard :: Decls -> Parser String Guard
guard = orLevel

orLevel :: Decls -> Parser String Guard
orLevel decls = do
  first <- andLevel decls
  rest <- many (try (keyword "or" *> andLevel decls))
  pure (if Array.null rest then first else Or ([ first ] <> rest))

andLevel :: Decls -> Parser String Guard
andLevel decls = do
  first <- notLevel decls
  rest <- many (try (keyword "and" *> notLevel decls))
  pure (if Array.null rest then first else And ([ first ] <> rest))

notLevel :: Decls -> Parser String Guard
notLevel decls = (keyword "not" *> (Not <$> defer \_ -> notLevel decls)) <|> atom decls

-- | `always` and `never` are how the printer renders the empty connectives, so
-- | they have to come back as those and not as facts of the same name.
atom :: Decls -> Parser String Guard
atom decls =
  between (symbol "(") (symbol ")") (defer \_ -> orLevel decls)
    <|> (keyword "always" $> And [])
    <|> (keyword "never" $> Or [])
    <|> comparison decls

comparison :: Decls -> Parser String Guard
comparison decls = do
  l <- expr decls
  option (Holds l) do
    op <- cmpOp
    r <- expr decls
    pure (Cmp op l r)

cmpOp :: Parser String CmpOp
cmpOp =
  (symbol "==" $> OpEq)
    <|> (symbol "!=" $> OpNe)
    <|> try (symbol "<=" $> OpLe)
    <|> try (symbol ">=" $> OpGe)
    <|> (symbol "<" $> OpLt)
    <|> (symbol ">" $> OpGt)

-- | A bare name is resolved against the declarations, which is the whole reason
-- | the block body is parsed with them in hand. A name that is neither a config
-- | nor a fact is an error rather than a guess: silently inventing a fact is how
-- | a guard comes to be false forever for no visible reason.
expr :: Decls -> Parser String Expr
expr decls =
  (Lit <$> literal)
    <|> qualified
    <|> bare
  where
  qualified =
    try (keyword "config" *> char ':' *> hspace *> (ConfigOf <<< ConfigId <$> ident))
      <|> try (keyword "fact" *> char ':' *> hspace *> (FactOf <<< FactId <$> ident))

  bare = do
    n <- identBut [ "and", "or", "not", "always", "never" ]
    if isConfig n then pure (ConfigOf (ConfigId n))
    else if isFact n then pure (FactOf (FactId n))
    else fail (n <> " is neither a declared config nor a declared fact")

  isConfig n = isJust (Array.find (\c -> c.id == ConfigId n) decls.config)
  isFact n = isJust (Array.find (\f -> f.id == FactId n) decls.facts)

literal :: Parser String Value
literal =
  (VString <$> quoted)
    <|> try (keyword "true" $> VBoolean true)
    <|> try (keyword "false" $> VBoolean false)
    <|> (VNumber <$> number)
