// Lay a music track under the silent film. The picture is copied, not re-encoded.
//
//   node mix.mjs <track> [start-seconds] [fade-out-seconds]   -> out/glassbox-film-<track name>.mp4
//
// The track plays from `start` at its own level less 1 dB (headroom for the AAC
// encode) and fades out over the last seconds of the film. Nothing here judges
// how it sounds; it prints the measured loudness and leaves that to a listener.
import { spawnSync } from 'node:child_process';
import { basename, dirname, extname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { scenes } from './timeline.mjs';

const here = dirname(fileURLToPath(import.meta.url));
const [track, start = '0', fade = '5'] = process.argv.slice(2);
if (!track) { console.error('node mix.mjs <track> [start-seconds] [fade-out-seconds]'); process.exit(1); }
const length = scenes.end.to, name = basename(track, extname(track)).replace(/[^A-Za-z0-9]+/g, '-').toLowerCase();
const out = join(here, `out/glassbox-film-${name}.mp4`);
const run = (args) => spawnSync('ffmpeg', ['-hide_banner', '-nostats', ...args], { encoding: 'utf8' });
const r = run(['-y', '-loglevel', 'error', '-i', join(here, 'out/glassbox-film.mp4'), '-ss', start, '-i', track,
  '-filter_complex', `[1:a]atrim=0:${length},asetpts=PTS-STARTPTS,volume=-1dB,afade=t=out:st=${length - Number(fade)}:d=${fade}[a]`,
  '-map', '0:v', '-map', '[a]', '-c:v', 'copy', '-c:a', 'aac', '-b:a', '256k', '-t', String(length), '-movflags', '+faststart', out]);
if (r.status !== 0) { console.error(r.stderr); process.exit(1); }
const m = run(['-i', out, '-map', '0:a', '-af', 'ebur128=peak=true', '-f', 'null', '-']).stderr;
const pick = (re) => (m.slice(m.lastIndexOf('Summary:')).match(re) || [])[1];
console.log(out);
console.log(`integrated ${pick(/I:\s+(-?[\d.]+) LUFS/)} LUFS, range ${pick(/LRA:\s+([\d.]+) LU/)} LU, peak ${pick(/Peak:\s+(-?[\d.]+) dBFS/)} dBFS`);
