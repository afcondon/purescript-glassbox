# Plate — a specimen for Glassbox

> Typeset a corpus of Glassbox machines as one self-contained page, so that
> the rules, and especially the transition grid, can be **checked by looking**.

A machine in `core/machines/` is infrastructure. One wrong cell in its
transition table is a production bug, and the format has no totality lint to
catch it. So the brief for this page is narrow and it is not "display the data
prettily": **make a wrong cell pop out.**

```sh
make            # site/index.html  — open it, no server needed
make artifact   # site/artifact.html — the same page with no html/head/body,
                #                      for a host that supplies the skeleton
make open       # build and open
```

## What it is

`glassbox-plate` is a PureScript package that reads `*.json` machines through
**`Glassbox.Codec.JSON` — the same decoder a host uses** — and writes one HTML
file with the stylesheet and the script inlined. Nothing about a machine is
described twice: the grid comes from `spec.rules`, the reachability from
`Glassbox.Edges`, the findings from `Glassbox.Lint`. If the page and the
runtime disagreed about an artifact, the page could not have been generated.

Each plate carries the machine **three ways**, all of them out of the library:

| | |
|---|---|
| the grid | `spec.rules` set as a state × event table — `Table`'s bet, typeset |
| the drawing | `Glassbox.Export.StateDiagram` over `Glassbox.Edges`, generated at build time |
| the rules | `Glassbox.DSL.Print.print Blocks`, printed whole |

They are there to be *compared*. Where two of them disagree, one of them is
wrong about code somebody is running, and the point of setting all three on one
page is that the disagreement becomes something you can see rather than
something you have to go and check.

**The drawing is generated, never embedded.** `core/machines/rendered/*.mermaid.txt`
is committed and is wrong — `elevator.mermaid.txt` has three arrows out of `up`
and none into it. It is wrong because it was rendered once and nothing since
made anyone look. Calling `toStateDiagram` here means the picture corrects
itself the moment `Glassbox.Edges` does, with no regeneration step and no stale
fixture to forget.

The corpus is the five machines in `core/machines/` plus
`plate/machines/supervise-group.json`, which is Bosun's — the lifecycle of one
`bosun supervise` group, and the only machine here that describes code somebody
is running.

| | |
|---|---|
| `src/Plate/Model.purs` | the derivation: cell kinds, the case ladders, the checks |
| `src/Plate/Render.purs` | the typesetting decisions |
| `src/Plate/Html.purs` | a markup value, and one function that renders it |
| `src/Plate/Main.purs` | read machines, read assets, write one file |
| `assets/plate.css` | the design, and the reasoning for it |
| `assets/plate.js` | the reading aids — crosshair, reach, traces |

## Why not Specimen

[Specimen](../../../../code-typography/specimen) is the sibling this borrows
its sensibility from — Swiss grid, hairlines, marginalia, alignment as
meaning — but **not its engine**. Specimen's pipeline is `preprocess → block
classify → signature analysis`, and every stage of it is about the surface
syntax of a `.purs` file: where a type breaks, which `::` columns align, which
comments float to the margin. A Glassbox machine has no surface syntax to
parse. It arrives as a decoded `Spec`, already structured, and the whole
problem is choosing what to *do* with structure that is already there.

So: the aesthetic, a fresh engine.

## The six cell kinds

`Glassbox.DSL.Print`'s `Table` syntax is this page's ancestor and its bet is
right — the 92% of rules that are one-liners belong in a cell, and a grid shows
a missing pair in a way a list cannot. What a monospace grid cannot do is spend
anything but *width*: every distinction becomes another glyph in the same
column. A page has weight, colour and ground, so the kinds are separated by
**ground**, and a grid reads as a shape before it reads as a table.

| | |
|---|---|
| **moves** | the target's name, in full ink on the plate's own ground. The substance. |
| **turns** | a `Move` to the state it is already in. Exits, re-enters, runs entry again. `Table` spells this identically to any other move; it is the least visible cell in the corpus and it is a supervisor's whole keep-alive loop. |
| **stays** | a centred dot on a bedded-down ground. Two cells in five. |
| **refuses** | the tag, on a faint signal wash. A band of them is a shape. |
| **guarded** | a numbered chip escaping to a ladder below the grid. |
| **no rule** | hatched. Nobody wrote this pair down; the runtime answers `Stay`. |

Three departures from the text rendering, each deliberate: **the grid comes
first** (a reader checking a cell should not reach it last); **the key is
stated once**, at the top of the document rather than under every machine; and
**`Turns` is a kind of its own**.

## The checks

Every plate carries them, and they report whether or not they find anything —
a check with no output looks exactly like a check that was never run.

- **totality** — pairs declared, out of states × events.
- **reach** — states with no inbound edge, from `Glassbox.Edges`.
- **edges** — transitions the *rules* name that the derived *edge set* does
  not contain, split into the two kinds that look identical and are not: an arm
  behind a **fact** guard has been dropped and that is a defect; an arm behind a
  **config** guard is correctly absent, because `edgesOf` enumerates facts and
  holds config at its defaults on purpose — config is what decides which machine
  this is.
- **vocabulary** — commands, refusals, facts and config declared and never
  used. (Config read only by a deadline's `after` counts as used: a looper's
  `count-in-beats` appears in no guard at all.)
- **lint** — `Glassbox.Lint`, in full.

## What it found

Written up on the page itself, from counts it takes off the plates. The one
worth repeating here:

> **11 transitions the rules name that no drawing shows.** Every one is an arm
> of a guarded rule offering a *choice* of destination.
> `Glassbox.Edges.probe` resolves a pair under every assignment of the facts
> and then keeps **the first move it finds**, so a rule with two destinations
> contributes one edge and the other is silently dropped. It reaches everything
> downstream of the edge set — `unreachableStates`, the Mermaid export, and
> every picture drawn from it, including the ones on this page. The committed
> `elevator.mermaid.txt` has no arrow into `up` at all: a lift that only goes
> down.
>
> The count is 11 and not 13, and the two it excludes are the useful part.
> `loop`'s `empty × record → armed` and `washing-machine`'s
> `locking × step-done → prewashing` are missing from the edge set too and
> *belong* there: each sits behind a config read. A washer built without a
> prewash cycle genuinely cannot reach `prewashing`, so a drawing is right to
> leave it out. Two absences that look identical, one a bug and one correct —
> which is exactly the sort of thing a page like this exists to make visible,
> and why the plates label them differently.
