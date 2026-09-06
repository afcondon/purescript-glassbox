# The Rust runtime

*2026-09-06. Written alongside `rust/glassbox-rs`, the conformance vectors
and `core/machines/itajara-loop.json`. The decision it follows: a
PureScript-to-Rust compiler is off the table (memory models), so the second
runtime is hand-written and held to the first by data.*

## What it is

`rust/glassbox-rs` (Cargo package `glassbox`) is a second implementation of
the version-2 artifact format: `spec`, `codec`, `run`, `drive` and
`canonical`, each a transcription of the PureScript module of the same name,
decision for decision, with the same doc comments where the decision is the
same. Dependencies are `serde` and `serde_json`; no async, no threads, no
allocation in `step` beyond the `Vec<CommandId>` it hands back. The
intended first consumer is the Itajara looper daemon.

It claims nothing of its own. The README's claim —

> Given the same sequence of input events, the artifact reaches the same
> state, emits the same effects in the same order, and refuses with the same
> tag

— is established for it by `conformance/vectors/*.json`, which the
PureScript runtime writes and `cargo test` replays. The vectors are the
"headless conformance runner comparing two implementations of the format"
the README anticipated.

## What the conformance claim covers, and does not

Covered, for every machine in `core/machines`, for every (state × event)
pair and every assignment of the facts and config that pair's guards read:
the outcome (`move` / `stay` / `refuse`), the refusal tag, the state after,
and the ordered command list the host owes (exit of the old state, then
entry of the new; nothing for `stay` or `refuse`; exit-then-entry of the
same state for a `Move` to where it already is). Also: the commands owed on
start, the alarm a state asks for on entry, and the canonical text of the
machine — which is the "same machine" clause of the claim, and which until
now was established by inspection.

Not covered, and deliberately, per the README: **timing** and **the host**.
Whether a host fires an alarm when it should, and what a command does when
carried out, are outside the claim on both sides. A deadline reaches the
table as two facts — the alarm on entry, and the ordinary vector for the
event it fires — and that is all the format says about it.

Also not ported: the lints, the printers, the drawing. A Rust host loads an
artifact the PureScript lints have accepted; the committed machines are
linted (`make lint`) and the vectors are generated from them. A Rust host
that wants to accept arbitrary files should port `Glassbox.Lint` first, or
pin the one machine it was built against by its canonical text.

## Numbers

Six machines, 622 vectors, 5 alarms, all replayed identically; every
machine's canonical text byte-identical between the two runtimes; every
machine round-trips through the Rust codec (compact and indented).

```
car-radio          34 vectors  1 alarm
elevator           67 vectors  1 alarm
heating            56 vectors  1 alarm
itajara-loop      353 vectors  0 alarms
loop               33 vectors  2 alarms
washing-machine    79 vectors  0 alarms
```

## Regenerating and running

```sh
make vectors        # PureScript writes conformance/vectors/*.json — commit the diff
make conformance    # cd rust/glassbox-rs && cargo test
spago test -p glassbox
```

The vectors are committed on purpose. A change to a machine or to the
PureScript runtime shows up as a diff in what it *answers* before it shows
up as a failure on the other side, and the diff is one line per changed
answer. A machine with no vectors file fails `cargo test` rather than being
skipped.

The schema is in `conformance/README.md`. The enumeration is per rule —
only the facts and config a pair's guards read are varied — which is
exactly complete and keeps a machine with twelve facts at a few hundred
lines rather than a few hundred thousand. Config is tried at its default,
the negation of a boolean, and every literal any guard compares it against.

## How the daemon would adopt it

The review of 2026-09-05 wants `Phase` as an enum with one `transition`
function that is the only writer of `state`, with the qualifiers inside the
constructors. `itajara-loop.json` is that enum as data, and the crate is the
`transition`. Adoption, in the order it would go:

1. **Embed and pin.** `include_str!` the artifact, `parse_spec` it at
   startup, compute `canonical_bytes` and compare with the hash the daemon
   was built against; refuse anything else. The daemon runs one machine and
   should say so.

2. **Decide on the command thread, never in the audio callback.** `step`
   allocates and clones strings; it is cheap, and it is not audio-safe. Every
   press already arrives on a command thread through `dispatch`, and that is
   where the machine runs: build the `World` from the atomics (the twelve
   facts are `input_claimed`, `plain()`, the pendulum/Revox test, the layer
   ceiling, `level_arm`, `quant`, `one_pass`, `n_layers == 1`, `loop_len > 0`,
   `n_layers > 0`, `threaded`, `grid().is_some()`), call `step`, and act on
   the `Step`. `dispatch` currently runs on three threads with no lock; the
   phase read-modify-write has to be serialised (the review's step 7, one
   control lane) or the machine's determinism buys nothing.

3. **Keep the callback as the stamper, not the decider.** The callback's
   job is to flip the byte at the exact frame and stamp `origin` / `rec_from`.
   Under the machine that is the *performance* of `request-at-boundary` and
   `start-take`, not a decision: when the command thread files a request it
   asks the machine what the request will move to (`step` from
   `armed-for-grid` on `boundary` — the facts it reads are settled by then)
   and stores that answer in the request. The callback consumes the request,
   flips the byte to the pre-decided phase at the frame, and reports
   `boundary` (or `sound`, from the input callback) back to the command
   thread, which feeds it to the machine so the phase the machine holds and
   the byte the callback holds agree. Same for the closer thread and
   `closed`. One writer of the decision; one stamper of the frame.

4. **Map the vocabularies.** `StateId` to the `u8` the callback reads (a
   table, written once at `move_to`; `is_recording` becomes "one of the four
   recording states or multiplying"), `CommandId` to the functions that exist
   (`zero_layer`, `thread_blank`, the arm branch of the callback, `commit`,
   `multiply_end`), `RefusalId` to the ack strings the surfaces already parse
   — the tag chooses the sentence, so nothing on the wire changes. Report at
   load which commands the host has no function for; that is
   `Host.reportCommands` and there is no reason the Rust side should skip it.

5. **Hold the adapter to the same table.** The daemon's own test replays
   `conformance/vectors/itajara-loop.json` through its `World`-builder and
   its `step` call, the way `check-verbs.py` and `check-snapshot.py` already
   hold the two sides together from outside. The crate's test proves the
   runtime; the daemon's test proves the wiring.

   **Done first, and before steps 1–4 (2026-09-06, the daemon's step 5b):**
   `daemon/src/engine/conformance.rs` replays the vectors through the
   engine *as it is* — the byte and the flags read into an artifact state
   by one function, each event delivered the way the daemon delivers it —
   and holds every outcome, state and refusal tag. 339 of 353 replayed;
   the 14 skipped say `has-layers` and not `has-length`, which no loop can
   be. Every mismatch the first run found is reconciled: the daemon
   gained the guards this artifact was prescriptive about and a cancelled
   arm that returns to what the loop held; the artifact gained
   `armed-by-sound` and a `no-grid` refusal for `len` on material. The
   table is now what the daemon is tested against.

6. **The pedalboard reads the same file.** `Socket.phaseOf` and
   `Machine.perform`'s guards are the PureScript half of the same machine;
   once the snapshot's `state` carries the `StateId`, they run the artifact
   directly and the two sides agree by construction rather than by care.

## Open questions, every one

Format and codec:

- **`stay` is any value.** `{"stay": false}` is a `Stay`, because the
  PureScript decoder tests for the key's presence. Mirrored; worth deciding
  whether the format means that.
- **A `to` that is not a string falls through** to `stay` and then `refuse`,
  and a `label` or `text` that is not a string falls back to the id; `final`
  that is not a boolean is false. All mirrored, none of them obviously what
  an author meant. The lints do not see these because the codec has already
  forgiven them.
- **`glassbox: 2.0` is accepted** (codec-argonaut's `int` accepts an integral
  float); `2.5` is refused. Mirrored.
- **String ordering is UTF-16.** PureScript's `compare` on `String` is
  JavaScript's `<`, which orders by UTF-16 code unit; Rust's `Ord` on `str`
  is byte-wise UTF-8, and the two disagree on any pair straddling the
  surrogate range. The Rust runtime orders by UTF-16 in guards and in the
  canonical sort. No machine in the corpus compares strings, so this is
  argued rather than measured; a test in `run.rs` pins the choice.
- **Fractional numbers in the canonical form** go through JavaScript's
  `Number.prototype.toString`, which the Rust side emulates (`js_number`).
  The corpus is all integers and booleans, so the emulation is measured by
  nothing yet — the same gap the Go probe names, and the same fix: a
  synthetic machine with a fractional default.
- **Encoded key order.** The Rust encoder writes fields in the PureScript
  encoder's order, but `serde_json::to_value` sorts keys; only
  `print_spec` / `print_spec_pretty` keep the order. An artifact hash must
  use the canonical form, never the encoding — which is what
  `Glassbox.Canonical` already argues.

The vectors:

- **Config compared against a fact, or against another config,** gets only
  its default: candidates come from literals. A machine that did that would
  need the enumeration extended.
- **A deadline whose `after` reads a fact** cannot be enumerated: facts are
  varied over booleans, so `alarmFor` answers `null` under every assignment.
  No machine does this; the vectors would say `null` and the Rust side would
  agree, which is conformance without coverage.
- **Duplicate rules or deadlines** for one key: the first wins on both sides
  and the vectors record it. The overlap lint warns.
- **Twelve facts is the `Edges.assignments` cap.** `itajara-loop` declares
  exactly twelve. The conformance enumeration is per rule and has no cap,
  but the drawing enumerates every fact for every pair, and a thirteenth
  fact would make `edgesOf` fall back to the bare world and silently drop
  every guarded arrow from the diagram — the exact failure the option
  `factsUnknown` was added to prevent. Either the drawing should enumerate
  per rule too, or the cap should reject rather than degrade.

The Rust API:

- **`Host::world()` returns an owned `World`** (a clone), mirroring
  `m World`. A daemon host might prefer to hand out a reference; the trait
  could take `&World` from a field. Left as the PureScript shape.
- **`fork` defaults to inline**, which is the `identity` host. A host that
  forks to another thread makes `fire`'s recursion cross threads; the
  PureScript driver has the same property and the same advice — a machine
  whose commands answer with events must reach a state with no entry
  commands.
- **`clippy` was not run**: the installed `clippy-driver` is from a different
  toolchain than `rustc` and fails to load `serde`. `cargo build` and
  `cargo test` are warning-free.

`itajara-loop` specifically (the artifact's `notes` carry the reasoning;
these are what it could not settle):

- **Exit commands carry the commit.** Every way out of a recording state runs
  `close-take` / `close-layer` / `close-multiply`, including `clear` and a
  lost device, where the daemon today zeroes rather than commits. The host's
  `close-take` has to know it is closing into a wipe or after an outage.
  This is the edge-specific-command case the README holds back, met again:
  the third machine to want it, after the looper's handover and Bosun's
  `reloading`.
- **Arguments are not the machine's.** `fix13`, `len4`, `blank13` store a
  number the host keeps; `undo` on a loop with layers to spare, `fix` on a
  loop with material and `len` on a loop with material change host memory
  and leave the phase alone, so the machine answers `stay` and the verb does
  its bookkeeping regardless. That is consistent with "the host holds the
  memory" and it means a `stay` is not "nothing happened" at the verb level
  — only at the phase level.
- **It was prescriptive** where the daemon had no guard: `u`, `x`, `z` and
  `blank` on an armed loop, `u` and `z` on a recording one, all refuse
  `still-recording`. Adopted 2026-09-06: the daemon guards every one of
  them (`guards::still_recording`), and the ack says which wait it is.
- **`lost` on a listening loop** stays listening; on a loop waiting for the
  bar it returns to where it was, because the outage drops every request.
  Measured 2026-09-06: right for both, once the third armed state below
  existed — a found sound waiting for the bar loses its request and is
  listening again, `armed-by-sound --lost--> armed-by-level`.
- **Level-arm and quantise together** wait for the sound and then for the
  bar. This was `armed-by-level --sound--> armed-for-grid`, and the replay
  showed the two waits are not one state: the loop that found its sound
  still holds the input, a second press cancels it (`armed-for-grid`
  shrugs a press off), and an outage leaves it listening. So it is
  `armed-by-sound`, the twelfth state; and since only an empty or sized
  loop ever waits for the bar without a sound, `armed-for-grid`'s exits
  read `has-length` alone.
- **`len` on material asks for a bar** as it does on an empty loop, unless
  the loop is itself the bar (which is `has-grid` too): `playing --size-->`
  and `tape --size-->` refuse `no-grid` without one. Found by the replay.
- **The one-pass overdub does not wait for the loop's zero**, per the
  daemon's 2026-09 decision; the artifact follows it. If that decision is
  revisited, `overdubbing-one-pass` gains an armed predecessor.
