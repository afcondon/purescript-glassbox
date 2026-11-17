// Step out/film.html frame by frame in headless Chrome and encode it.
//
//   node render.mjs                 the whole film  -> out/glassbox-film.mp4 (silent)
//   node render.mjs still 12 40.5   single frames   -> out/still-012.000.png ...
//   node render.mjs 42 60           a passage       -> out/glassbox-film-42-60.mp4
import puppeteer from 'puppeteer-core';
import { spawn } from 'node:child_process';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { FPS, scenes } from './timeline.mjs';

const here = dirname(fileURLToPath(import.meta.url));
const chrome = process.env.CHROME || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
const args = process.argv.slice(2);

const browser = await puppeteer.launch({ executablePath: chrome, headless: true, args: ['--hide-scrollbars', '--force-color-profile=srgb'] });
const page = await browser.newPage();
await page.setViewport({ width: 1920, height: 1080, deviceScaleFactor: 1 });
page.on('pageerror', (e) => { console.error('page error:', e.message); process.exitCode = 1; });
await page.goto('file://' + join(here, 'out/film.html'), { waitUntil: 'networkidle0' });
await page.evaluate(() => document.fonts.ready);
const faces = await page.evaluate(() => ['600 20px Archivo', '400 20px "IBM Plex Mono"'].map((f) => document.fonts.check(f)));
if (faces.includes(false)) { console.error('fonts did not load; refusing to render in a fallback face'); await browser.close(); process.exit(1); }

const shoot = async (t, opts) => { await page.evaluate((x) => window.seek(x), t); return page.screenshot({ type: 'png', optimizeForSpeed: true, ...opts }); };

if (args[0] === 'still') {
  for (const a of args.slice(1)) {
    const t = Number(a), path = join(here, `out/still-${t.toFixed(3).padStart(7, '0')}.png`);
    await shoot(t, { path });
    console.log(path);
  }
} else {
  const from = args.length === 2 ? Number(args[0]) : 0;
  const to = args.length === 2 ? Number(args[1]) : scenes.end.to;
  const out = join(here, args.length === 2 ? `out/glassbox-film-${from}-${to}.mp4` : 'out/glassbox-film.mp4');
  const ffmpeg = spawn('ffmpeg', ['-y', '-loglevel', 'error', '-f', 'image2pipe', '-framerate', String(FPS), '-i', '-',
    '-c:v', 'libx264', '-preset', 'medium', '-crf', '15', '-pix_fmt', 'yuv420p', '-movflags', '+faststart', out], { stdio: ['pipe', 'inherit', 'inherit'] });
  const frames = Math.round((to - from) * FPS);
  for (let i = 0; i < frames; i++) {
    const png = await shoot(from + i / FPS);
    if (!ffmpeg.stdin.write(png)) await new Promise((r) => ffmpeg.stdin.once('drain', r));
    if (i % 300 === 0) console.log(`frame ${i}/${frames}`);
  }
  ffmpeg.stdin.end();
  await new Promise((r) => ffmpeg.on('close', r));
  console.log(out);
}
await browser.close();
