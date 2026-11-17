// Assemble out/film.html: one page, every input inlined, stepped by render.mjs.
//
// Nothing about the machine is written in this directory. The geometry is
// data/geometry.json (spago run -p glassbox-film), the outcomes are the
// committed conformance vectors, and the text is the Blocks printer's output.
import { readFileSync, writeFileSync, mkdirSync, readdirSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import * as timeline from './timeline.mjs';

const here = dirname(fileURLToPath(import.meta.url));
const repo = join(here, '..');
const read = (p) => readFileSync(join(repo, p), 'utf8');

const geometry = JSON.parse(read('film/data/geometry.json'));
const spec = JSON.parse(read('core/machines/car-radio.json'));
const vectorDir = 'conformance/vectors';
const suites = readdirSync(join(repo, vectorDir)).filter((f) => f.endsWith('.json')).sort()
  .map((f) => JSON.parse(read(join(vectorDir, f))));
const radio = suites.find((s) => s.machine === 'car-radio');

// The rule blocks of the printed machine: everything from the first state on.
const printed = read('core/machines/rendered/car-radio.blocks.txt').split('\n');
const firstDeclaration = printed.findIndex((l) => l.startsWith('machine '));
const firstRule = printed.findIndex((l) => l === `${spec.initial}:`);
const trim = (ls) => { while (ls.length && !ls[ls.length - 1].trim()) ls.pop(); return ls; };

const data = {
  timeline: { FPS: timeline.FPS, scenes: timeline.scenes, captions: timeline.captions, subtitles: timeline.subtitles, steps: timeline.steps },
  geometry,
  spec: {
    states: spec.states.map((s) => s.id),
    events: spec.events.map((e) => e.id),
    eventLabels: Object.fromEntries(spec.events.map((e) => [e.id, e.label])),
    config: spec.config, facts: spec.facts,
    refusals: Object.fromEntries(spec.refusals.map((r) => [r.id, r.text])),
    initial: spec.initial,
  },
  start: radio.start,
  alarms: radio.alarms,
  vectors: radio.vectors,
  whole: trim(printed.slice(firstDeclaration)),
  rules: trim(printed.slice(firstRule)),
  suites: suites.map((s) => ({ machine: s.machine, cases: s.vectors.length })),
};

const html = `<!doctype html>
<html lang="en"><head><meta charset="utf-8"><title>Glassbox</title>
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Archivo:wght@400;500;600;700;800&family=IBM+Plex+Mono:wght@400;500;600&display=swap">
<style>${readFileSync(join(here, 'film.css'), 'utf8')}</style></head>
<body><div id="stage"></div>
<script>window.DATA = ${JSON.stringify(data)};</script>
<script>${readFileSync(join(here, 'film.js'), 'utf8')}</script>
</body></html>`;
mkdirSync(join(here, 'out'), { recursive: true });
writeFileSync(join(here, 'out/film.html'), html);
const total = data.suites.reduce((n, s) => n + s.cases, 0);
console.log(`out/film.html  ${timeline.scenes.end.to} s, ${data.suites.length} suites, ${total} cases`);
