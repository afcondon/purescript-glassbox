#!/usr/bin/env bash
# Is the canonical form the same text on both runtimes?
#
# `Glassbox.Canonical` exists because it must be, and "must be" is a claim
# rather than a measurement until something runs this. The whole reason the
# module hand-writes every field instead of reusing `Codec.JSON` is that Go's
# `encoding/json` sorts object keys where `JSON.stringify` preserves insertion
# order — so a machine hash taken over the JSON encoding would differ between
# the two halves of the same program, and a pinned Go binary would reject the
# artifact the node one accepts.
#
# This transpiles the probe with backend-go, builds it, runs both columns over
# the same seven machines and diffs. Byte-identical or it fails.
#
# The seventh machine is synthetic and is the point of the exercise: the real
# corpus is entirely integers and booleans, and `Canonical.number` writes those
# itself. Only a fractional default reaches the backend's own float formatting,
# which is the single construct here whose agreement is measured rather than
# guaranteed by construction.
#
# NOTE: bash, not zsh — `backend-go/run_conformance.sh` next door is zsh and
# fails silently with exit 0 under bash. Do not cross the two.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BG="${BACKEND_GO:-/Users/afc/work/afc-work/purescript-backends/purescript-go}"
OUT="$(mktemp -d)"
trap 'rm -rf "$OUT"' EXIT

[ -d "$BG/backend-go" ] || { echo "no backend-go at $BG — set BACKEND_GO"; exit 1; }

echo "==> regenerating the fixture"
( cd "$HERE" && python3 probe/gen-fixture.py )

echo "==> node column"
( cd "$HERE" && spago build -p glassbox-probe >/dev/null 2>&1 )
( cd "$HERE" && spago run -p glassbox-probe --quiet 2>/dev/null ) | awk '/^=== /{f=1} f' > "$OUT/node.txt"

echo "==> transpiling with backend-go"
( cd "$BG/backend-go" && spago run -- \
    --corefn-dir "$HERE/output" --output-dir "$OUT/go" \
    --main Probe.Main --foreign-dir "$BG/backend-go/foreign" >/dev/null 2>&1 )
cp "$BG/backend-go/runtime.go" "$OUT/go/runtime.go"
( cd "$OUT/go" && go build -o ./probe_go ./*.go )
"$OUT/go/probe_go" > "$OUT/go.txt"

if diff -q "$OUT/node.txt" "$OUT/go.txt" >/dev/null; then
  echo "=== node == Go, byte-identical over $(grep -c '^=== ' "$OUT/node.txt") machines, $(wc -l < "$OUT/node.txt" | tr -d ' ') lines ==="
else
  echo "=== DIVERGED ==="
  diff "$OUT/node.txt" "$OUT/go.txt" | head -40
  exit 1
fi
