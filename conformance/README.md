# Conformance vectors

The README's claim, as data:

> Given the same sequence of input events, the artifact reaches the same
> state, emits the same effects in the same order, and refuses with the same
> tag.

`vectors/<machine-id>.json` is that claim spelled out for one machine —
every (state × event × guard-assignment) the artifact admits, and what the
PureScript runtime (`Glassbox.Run.step`) answers for each. A second
implementation of the format replays the file and must answer identically.
The Rust runtime in `rust/glassbox-rs` does exactly that in `cargo test`.

The files are **generated and committed**. Regenerate after any change to a
machine in `core/machines` or to the runtime, and read the diff: a changed
line is a changed answer.

```sh
make vectors          # spago run -p glassbox-cli -- vectors core/machines conformance/vectors
make conformance      # cd rust/glassbox-rs && cargo test
```

## What is and is not covered

Covered: the outcome of every step (`move` / `stay` / `refuse` with tag),
the state after it, the ordered command list the host owes (exit of the
old state, then entry of the new, per `Glassbox.Run.commandsFor`), the
commands owed on start, the alarm a host is told to set on entering a
state with a deadline, and the canonical text of the machine.

Not covered, deliberately: **timing** (whether a host fires an alarm when
it should) and **the host** (what a command does when carried out). Both
are outside the claim, per the README. A deadline appears here in two
halves that together are all the format says about it: the `alarms` entry
for the state, and the ordinary vector for the event it fires.

## The enumeration

For every (state, event) pair in the declarations — including pairs with
no rule, whose answer is `stay` with no commands — the table varies **only
what that pair's guards read**:

- each **fact** the rule names, over `false` and `true`;
- each **config** the rule names, over its declared default, the negation
  of a boolean, and every literal any guard in the machine compares it
  against (so `count-in-beats > 0` is tried at 4 and at 0).

Config the pair does not read is at its default; facts the pair does not
read are unreported. Both are part of the contract: a replayer builds the
world from the machine's defaults (`worldFrom`) and applies the vector's
`config` and `facts` on top. Declaration order is kept and the last key
varies fastest.

This is smaller than `Glassbox.Edges.assignments`, which varies every
declared fact for every pair. That is right for a drawing and redundant
for a table: a fact no guard on the rule names cannot change the answer.

## Schema (`"glassbox-vectors": 1`)

```json
{
  "glassbox-vectors": 1,          // version of THIS file's shape
  "machine": "car-radio",         // the artifact's id
  "glassbox": 2,                  // the artifact's format version, as decoded
  "canonical": "glassbox-canonical 1\n...",   // Glassbox.Canonical.canonicalBytes
  "start": ["power-down"],        // commands owed on entering the initial state
  "vectors": [
    { "from": "radio",            // state before
      "event": "source",          // event delivered
      "config": {"has-cd-slot": true},     // config this pair reads, with values
      "facts": {"disc-loaded": false},     // facts this pair reads, with values
      "outcome": "refuse",        // "move" | "stay" | "refuse"
      "refusal": "no-disc",       // present only when outcome is "refuse"
      "current": "radio",         // state after (== from for stay and refuse)
      "commands": [] }            // exit commands of `from` then entry of `current`; [] unless move
  ],
  "alarms": [
    { "state": "seeking",
      "config": {"seek-seconds": 8}, "facts": {},   // what `after` reads
      "alarm": {"fires": "giveup", "after": 8, "clock": "seconds"} }   // or null
  ]
}
```

`commands` for a `move` to the state the machine is already in lists that
state's exit commands followed by its entry commands: the external
self-transition re-enters. A `stay` or a `refuse` lists nothing.

Values follow the artifact's own rule: a number, boolean or string is
written as its bare JSON self.

## Replaying

For each vector: decode the machine, build `worldFrom`, set the listed
config and facts, run `step`, and compare `outcome`, `refusal`, `current`
and `commands` exactly. For each alarm: compute the state's alarm under the
same world and compare `fires`, `after` and `clock`. Compare `start` with
the initial state's entry commands and `canonical` with your own canonical
text. A failure should print the machine, the state, the event and the
assignment, because that is the reproducing case.
