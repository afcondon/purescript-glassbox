# Glassbox

A state machine as a **data artifact** — authored as a file, run at
runtime, drawn from the same value it is running.

```
glassbox        the machine: data model, runtime, codec, edge set, text export
glassbox-draw   the picture: an adapter onto DataViz.Layout.StateMachine
```

## The claim

> Given the same sequence of input events, the artifact reaches the same
> state, emits the same effects in the same order, and refuses with the
> same tag.

Not more than that. It does **not** claim that two applications *behave*
the same — behaviour also depends on what feeds the machine events and
what the host does with them — nor anything about timing. Those two
seams are named, small, and outside the claim.

The claim is a property rather than an intention because it is
checkable: enumerate every (state × event × guard-assignment) the
artifact admits and drive each implementation through all of them.

## The acceptance criterion

> **A new machine requires zero lines of PureScript.**

`core/machines/car-radio.json` is the witness: no constructor names its
states, nothing was recompiled to add it, and no module here knows what
a car radio is. If that ever stops being true the thesis has failed and
everything else is decoration.

## Why three packages

The core has no dependency on any drawing library. Two consumers need it
that way: a headless conformance runner comparing two implementations of
the format, and any host on a backend where a layout library is not
available or not wanted. Running a machine and drawing one are different
jobs — which is the whole point, since the same machine is meant to run
under an arbitrary UI.

`glassbox-cli` exists for the same reason one step further out. Every
printer in the core is a **pure function** from a `Spec` to a string;
none of them can write a file, so `effect` and `node-fs` stay out of the
library's manifest. The CLI is where that IO lives, and it is as thin as
reading, parsing, and writing allows:

```sh
spago run -p glassbox-cli -- lint core/machines
spago run -p glassbox-cli -- render-all core/machines core/machines/rendered
spago run -p glassbox-cli -- vocabulary <machines> <out-dir> <module-prefix> [regen-command]
```

`render-all` dumps every machine in every candidate syntax **and as a
state diagram**; `vocabulary` emits each machine's alphabet as a
PureScript module of rows. Both read their input **through the codec**, so
a tool can only describe a machine the runtime would itself accept.

The diagram is written out for a reason worth naming: it had no output
anywhere, so nothing was in a position to disagree with the module whose
whole justification is being a second opinion — and it spent a year
emitting kebab-case identifiers, which are a parse error in the dialect it
targets. A printer nobody can see is a printer nobody checks.

## The modules

| Module | |
|---|---|
| `Glassbox.Spec` | the decoded machine — all data, no functions |
| `Glassbox.Run` | the runtime: a pure function of (world, state, event) |
| `Glassbox.Drive` | the driver: what a host does with that function. Runs in `Monad m` and nothing else |
| `Glassbox.Host` | the half of the seam a compiler can check — rows, reports, and the one place a string becomes a member of one |
| `Glassbox.Drive` | the driver: what a host does with that function. Runs in `Monad m` and nothing else |
| `Glassbox.Host` | the half of the seam a compiler can check — rows, reports, and the one place a string becomes a member of one |
| `Glassbox.Edges` | the edge set, derived by resolving every pair under a world |
| `Glassbox.Codec.JSON` | the wire format: one bidirectional codec, versioned |
| `Glassbox.DSL.Print` / `Glassbox.DSL.Parse` | the Blocks authoring syntax, printed and read back. `Table` is a second, read-only rendering |
| `Glassbox.Export.StateDiagram` | one-way text export. Emits Mermaid's dialect because things render it for free; imports nothing and draws nothing |
| `Glassbox.Lint` | the correctness lints that replace the compiler at the boundary. Rejections gate the boot; warnings do not |
| `Glassbox.Export.PureScript` | one-way export of a machine's **alphabet** — its states and commands as rows — so a host can be exhaustive over them. Emits no transitions, so it cannot disagree with the artifact about behaviour |
| `Glassbox.Draw` | *(glassbox-draw)* edges → `DataViz.Layout.StateMachine` |

## Running one: the six answers

`Glassbox.Drive` asks a host six questions and imposes `Monad m` and
nothing else — no `MonadState`, no effect system, no UI:

```purescript
type Wiring m =
  { spec    :: Spec
  , phase   :: m StateId            -- where you keep it
  , move    :: StateId -> m Unit
  , world   :: m World              -- what you can tell it
  , perform :: CommandId -> m (Maybe EventId)
  , refused :: RefusalId -> m Unit
  , fork    :: m Unit -> m Unit     -- how not to block
  , arm     :: Maybe Alarm -> m Unit
  }
```

They are fields rather than constraints because the point of the artifact
is that the *same* machine runs in places that do not resemble each other,
and a driver that only ran under one effect system would concede the
argument at the last step. `Test.Unit.Drive` drives a machine in `State`
with `fork = identity`; that module existing is the proof.

`perform` answers with an **event, not a result** — completion is an event,
which is what keeps a failure arm in the enumeration where a lint can see
it, instead of in a callback where it can be forgotten. `fork` is what makes
an update optimistic and is visibly the host's choice. `arm` is called on
every state change, so *leaving a state cancels its deadline* is a rule of
the driver rather than something each host has to remember.

## The five vocabularies

Owned by different people, and the distinction is load-bearing.

**States and events** are opaque identifiers owned by the machine's
author; no runtime interprets them. **Config and facts** are the two
halves of the world a guard may read — config is declared by the
artifact and *reshapes* the machine, facts are supplied by the host and
describe the world. **Refusals** are a closed list the artifact
declares, so a refusal carries a tag rather than prose. **Commands** are
a closed list too, and they are the contract with the host: it can read
them at load time and say up front which it cannot honour.

Config versus parameter has a mechanical test, not a judgement call:
*does the diagram change when you change it?* That test is also why the
derived edge set enumerates every assignment of the **facts** and takes
config at its declared value: config decides which machine this is, and a
fact is transient. Assuming an unreported fact false instead used to drop
guarded transitions out of the picture entirely. That test is why the derived
edge set enumerates every assignment of the **facts** and takes config at
its declared value: config decides which machine this is, and a fact is
transient. Assuming an unreported fact false instead used to drop guarded
transitions out of the picture entirely.

## The rule a designer has to know

**A machine has no memory beyond the state it is in.** Every count,
index, position and history lives in the host and arrives as a fact.
Three machines in the corpus forced this independently — the elevator's
floor number, the washing machine's rinse count, and pause/resume, which
wants a history state the format cannot express. Nothing in the format
tells an author this, and each has an obvious wrong answer the format
accepts without complaint: a state per floor, a state per rinse, a
paused-state per step.

## Commands hang off states, not arrows

A machine drives something as well as describing it, and the commands
belong to a **state**, on entry and on exit:

```
recording:
  entry  start-recorder
  exit   stop-recorder
  record, play  -> playing
  stop          -> stopped
```

*Entering Recording starts the recorder* is written once and holds for
every inbound edge, including the one somebody adds next month. Under
the rejected shape — a command on each arrow — the fifth edge is the one
that forgets, with the compiler perfectly happy.

Three rules, each a decision rather than a consequence:

- **A refusal commands nothing.** A refused press that nonetheless does
  something is the bug the `Stay`/`Refuse` distinction exists to expose.
- **`Stay` commands nothing** — it is a statechart *internal*
  transition, so nothing is exited and nothing entered.
- **`Move` to the state you are already in exits and re-enters.** That
  is the *external* self-transition, and re-entering Recording genuinely
  restarts the recorder. Statecharts need an explicit annotation for
  this; the format gets it free from a field it has for other reasons.

Commands carry no arguments. The machine emits an intent and the host,
which holds all the memory, resolves the details.

**Prefer `entry`; reach for `exit` rarely.** The corpus taught this and
it was not predicted. An entry command says *what must now be true*, so
it is safe however you arrived; an exit command is only safe when no
neighbouring state could want the thing kept. The elevator's obvious
`doors-open exit close-doors` is wrong, because `doors-held` is next
door. The washing machine has twelve states, no exit commands, and reads
better for it.

## What is deliberately absent

**Edge-specific commands.** A command that depends on *which event you
arrived by* cannot be said. Shipping both shapes at once is how a format
becomes unreadable; this one waits for an application that needs it.

**Composition.** A real boiler serves heating and hot water as
independent circuits. Two machines sharing a resource cannot be said in
this format.

**A history state.** A machine has no memory, so a state cannot return
whence it came. Three independent machines have now wanted it — the
looper's pause/resume, and Bosun's `reloading`, which cannot know whether
it was entered from `held` or `raised`. Each time the honest workaround
has been to narrow the machine rather than widen the format, and each time
that has been defensible; it will not always be.

## Three surfaces, one value

**Blocks** is what a person edits, **JSON** is what hashes and what a
second runtime agrees to, and **Table** and **state-diagram text** are
read-only renderings. All decode to or derive from the same `Spec`,
which is the only reason it is safe to have more than one.

Blocks round-trips a machine's *meaning* rather than its byte layout:
`parse (print s)` behaves identically to `s` on the full (state × event)
cross product and carries the same declarations, prose and deadlines,
but need not recover `s.rules` in the same array order, because the
printer groups events that share a case list. What holds instead, and is
what matters for a file in git, is that printing is **idempotent through
a parse** — an author's file is stable once written.

## Timing

`after` is a bare quantity, and the machine declares the **clock** it is
counted in — `milliseconds`, `seconds`, `minutes`, `beats`, `frames`.
Before that existed, the only record of what the number meant was the
spelling of a config key (`boost-minutes`, `count-in-beats`), and the
corpus was already running three units across four machines on that basis.
The timing lint refuses a machine with deadlines and no clock, and refuses
two deadlines on one state — the runtime runs the first and the printer
prints them all.

`millisecondsPer` converts the three wall-clock units and answers `Nothing`
for `beats` and `frames`, which is the honest answer: a beat is not a
duration until somebody says what the tempo is. The question parked behind
that `Nothing` — when the tempo changes while a state is pending, does the
deadline move? — is deliberately open until a musical host needs it.

## Two runtimes

`rust/glassbox-rs` is the same format in Rust — `spec`, `codec`, `run`,
`drive`, `canonical`, transcribed decision for decision — for a host that
cannot run JavaScript; the looper daemon is the first. It is held to this
runtime by data: `make vectors` writes every (state × event ×
guard-assignment) of every machine and what `Glassbox.Run.step` answers to
`conformance/vectors/`, and `cargo test` there replays the lot. Schema in
`conformance/README.md`; the story, the numbers and the open questions in
`docs/rust-runtime.md`.

## Tests

```sh
spago test -p glassbox
```

Drives the five-machine corpus in `core/machines/`: round-trip equality
on the full (state × event) cross product, behaviour parity with the
PureScript machine this replaced, config genuinely reshaping the derived
edge set, every arrival at a state running that state's entry commands
whichever edge it came along, and the boundary refusing a future
version, a malformed case and non-JSON.

Plan: `docs/kb/plans/the-same-machine.md` in the afc-work docs repo.
