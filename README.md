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

## Why two packages

The core has no dependency on any drawing library. Two consumers need it
that way: a headless conformance runner comparing two implementations of
the format, and any host on a backend where a layout library is not
available or not wanted. Running a machine and drawing one are different
jobs — which is the whole point, since the same machine is meant to run
under an arbitrary UI.

## The modules

| Module | |
|---|---|
| `Glassbox.Spec` | the decoded machine — all data, no functions |
| `Glassbox.Run` | the runtime: a pure function of (world, state, event) |
| `Glassbox.Edges` | the edge set, derived by resolving every pair under a world |
| `Glassbox.Codec.JSON` | the wire format: one bidirectional codec, versioned |
| `Glassbox.DSL.Print` / `Glassbox.DSL.Parse` | the Blocks authoring syntax, printed and read back. `Table` is a second, read-only rendering |
| `Glassbox.Export.StateDiagram` | one-way text export. Emits Mermaid's dialect because things render it for free; imports nothing and draws nothing |
| `Glassbox.Draw` | *(glassbox-draw)* edges → `DataViz.Layout.StateMachine` |

## The three vocabularies

Owned by different people, and the distinction is load-bearing.

**States and events** are opaque identifiers owned by the machine's
author; no runtime interprets them. **Config and facts** are the two
halves of the world a guard may read — config is declared by the
artifact and *reshapes* the machine, facts are supplied by the host and
describe the world. **Refusals** are a closed list the artifact
declares, so a refusal carries a tag rather than prose.

Config versus parameter has a mechanical test, not a judgement call:
*does the diagram change when you change it?*

## The rule a designer has to know

**A machine has no memory beyond the state it is in.** Every count,
index, position and history lives in the host and arrives as a fact.
Three machines in the corpus forced this independently — the elevator's
floor number, the washing machine's rinse count, and pause/resume, which
wants a history state the format cannot express. Nothing in the format
tells an author this, and each has an obvious wrong answer the format
accepts without complaint: a state per floor, a state per rinse, a
paused-state per step.

## What is deliberately absent

**Effects.** A transition can move, stay or refuse; it cannot yet ask
the host to *do* anything. Whether commands hang off edges or off state
entry and exit is an open fork, and the version field exists so it can
be settled without breaking artifacts written today.

**Correctness lints.** Making the machine data gave up the
exhaustiveness a `case` expression provided; nothing has yet bought it
back. A `loop.json` missing a (state, event) pair loads and silently
stays put.

**Composition.** A real boiler serves heating and hot water as
independent circuits. Two machines sharing a resource cannot be said in
this format.

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

## Tests

```sh
spago test -p glassbox
```

Drives the five-machine corpus in `core/machines/`: round-trip equality
on the full (state × event) cross product, behaviour parity with the
PureScript machine this replaced, config genuinely reshaping the derived
edge set, and the boundary refusing a future version, a malformed case
and non-JSON.

Plan: `docs/kb/plans/the-same-machine.md` in the afc-work docs repo.
