// render-all.mjs
//
// Dump every machine in core/machines/ in every candidate syntax.
//
//   node core/src/Glassbox/DSL/render-all.mjs        (from the repo root)
//
// This is a node script rather than a `spago run` entry point on purpose. The
// `glassbox` package does not depend on `effect` or `node-fs` — printing is a
// pure function and should not drag a runtime into the manifest for the sake of
// a one-off dump. `Glassbox.DSL.Print.renderAll` returns filename/content pairs;
// the only thing left is to write them, which node already knows how to do.
//
// It reads the compiled output, so `spago build -p glassbox` has to have run.

import { readFileSync, readdirSync, writeFileSync, mkdirSync } from "node:fs";
import { join, dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const repo = resolve(here, "../../../..");
const machines = join(repo, "core/machines");
const outDir = join(machines, "rendered");

const Codec = await import(
  join(repo, "output/Glassbox.Codec.JSON/index.js")
);
const Print = await import(
  join(repo, "output/Glassbox.DSL.Print/index.js")
);
const Tuple = (await import(join(repo, "output/Data.Tuple/index.js"))).Tuple;

const inputs = [];
for (const file of readdirSync(machines).sort()) {
  if (!file.endsWith(".json")) continue;
  const name = file.replace(/\.json$/, "");
  const parsed = Codec.parseSpec(readFileSync(join(machines, file), "utf8"));
  if (parsed.constructor.name === "Left") {
    console.error(`!! ${file}: ${parsed.value0}`);
    process.exitCode = 1;
    continue;
  }
  inputs.push(Tuple.create(name)(parsed.value0));
}

mkdirSync(outDir, { recursive: true });
for (const pair of Print.renderAll(inputs)) {
  writeFileSync(join(outDir, pair.value0), pair.value1);
  console.log(`${pair.value0}  ${pair.value1.split("\n").length - 1} lines`);
}
