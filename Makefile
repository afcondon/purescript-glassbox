# Glassbox — the targets that write files.
#
# Every printer in `core` is a pure function; `glassbox-cli` is where the IO
# lives, and these are the invocations worth remembering.

MACHINES := core/machines
CLI      := spago run -p glassbox-cli --quiet --

.PHONY: build test lint vectors render conformance

build:
	spago build

test:
	spago test -p glassbox

lint:
	$(CLI) lint $(MACHINES)

## The conformance suites: every (state x event x assignment) of every
## machine and what the PureScript runtime answers. Committed; regenerate
## after any change to a machine or to the runtime, and read the diff.
vectors:
	$(CLI) vectors $(MACHINES) conformance/vectors

render:
	$(CLI) render-all $(MACHINES) $(MACHINES)/rendered

## The Rust runtime, replaying the suites above.
conformance:
	cd rust/glassbox-rs && cargo test
