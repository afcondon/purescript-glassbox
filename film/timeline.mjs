// The whole cut. Times are seconds from the start of the film.
// Change a number here, then `node build.mjs && node render.mjs`.

export const FPS = 30;

export const scenes = {
  title:    { from: 0,   to: 8 },
  soup:     { from: 8,   to: 32 },
  file:     { from: 32,  to: 42 },
  machine:  { from: 42,  to: 96 },
  runtimes: { from: 96,  to: 112 },
  end:      { from: 112, to: 121 },
};

// One headline at a time, top left, on every scene but the title and the end.
export const captions = [
  { from: 8.4,  to: 14.5, text: 'A car radio, written the usual way.' },
  { from: 14.5, to: 19.5, text: 'Four booleans make sixteen states.' },
  { from: 19.5, to: 23.5, text: 'Five of them are a car radio.' },
  { from: 23.5, to: 31.8, text: 'Press power while the CD plays. Now you are in one of the other eleven.' },

  { from: 32.3, to: 36.5, text: 'So write the machine down.' },
  { from: 36.5, to: 41.8, text: 'One text file: states, events, guards, refusals, a timeout.' },

  { from: 42.3, to: 53,   text: 'The text, the table and the drawing are one value.' },
  { from: 53,   to: 62,   text: 'A timeout is part of the machine.' },
  { from: 62,   to: 69,   text: 'A guard asks the host about the world.' },
  { from: 69,   to: 76,   text: 'When the machine says no, it gives the reason.' },
  { from: 76,   to: 87,   text: 'The world changes. The file does not.' },
  { from: 87,   to: 95.8, text: 'Take out the CD slot, and Bluetooth goes with it.' },

  { from: 96.3, to: 111.8, text: 'One file, three runtimes, the same answers.' },
];

// The script, as subtitles along the bottom. Chattier than the headlines, and
// timed to what is on screen: the machine's steps below are at 42 + `at`.
export const subtitles = [
  { from: 1.0,   to: 4.4,   text: 'Most programs have a state machine hiding in them.' },
  { from: 4.6,   to: 7.8,   text: 'Glassbox is a library for taking it out and looking at it.' },

  { from: 8.4,   to: 11.5,  text: 'Here’s a car radio, the way most of us would write it.' },
  { from: 11.5,  to: 14.5,  text: 'Four flags and a function that flips them. It looks fine.' },
  { from: 14.5,  to: 19.5,  text: 'But four booleans can be set sixteen ways, and the compiler will accept any of them.' },
  { from: 19.5,  to: 23.5,  text: 'Only five of the sixteen mean anything. The rest shouldn’t happen.' },
  { from: 23.5,  to: 27.3,  text: 'They do, though. Press power while a CD is playing…' },
  { from: 27.3,  to: 31.8,  text: '…and the radio is off with the CD flag still set. No type error, no warning.' },

  { from: 32.3,  to: 36.5,  text: 'So don’t leave the machine implied by the code. Write it down.' },
  { from: 36.5,  to: 41.8,  text: 'This is the whole car radio. It’s a text file, and nothing was compiled to add it.' },

  { from: 42.3,  to: 45.2,  text: 'Glassbox reads the file and gives you three views of it.' },
  { from: 45.2,  to: 50,    text: 'Press power. The same rule lights up in the text, the table and the drawing.' },
  { from: 50,    to: 54,    text: 'Each move tells the host program what to do next. Here: start seeking.' },
  { from: 54,    to: 58.4,  text: 'Nobody presses anything now. The file says seeking gives up after eight seconds.' },
  { from: 58.4,  to: 62.5,  text: 'So the runtime fires that event itself, and we’re back on the radio.' },
  { from: 62.5,  to: 65.2,  text: 'Some rules depend on facts only the host knows. Is there a disc in?' },
  { from: 65.2,  to: 69.5,  text: 'There is, so source takes us to CD.' },
  { from: 69.5,  to: 73,    text: 'Press source again. There’s no phone paired, so the machine says no.' },
  { from: 73,    to: 76.3,  text: 'It says why, and it tells the host to do nothing. A refusal can’t half-happen.' },
  { from: 76.3,  to: 82.5,  text: 'Pair a phone, and the same press goes through. The file hasn’t changed; the world has.' },
  { from: 82.5,  to: 87,    text: 'Power off. Leaving Bluetooth disconnects the phone, whichever way you leave.' },
  { from: 87,    to: 91,    text: 'One more switch: a cheaper model, with no CD slot.' },
  { from: 91,    to: 95.8,  text: 'Bluetooth turns out to be unreachable too. That’s a design bug, and now you can see it.' },

  { from: 96.3,  to: 100,   text: 'Because the machine is data, you can list every question it could be asked.' },
  { from: 100,   to: 104.5, text: 'Every state, every event, every way the facts can fall: 687 cases over seven machines.' },
  { from: 104.5, to: 108.5, text: 'PureScript on JavaScript, PureScript on the BEAM, and a Rust port all give the same answers.' },
  { from: 108.5, to: 111.8, text: 'Two of those machines are already running real programs.' },
];

// What happens to the machine, in seconds from the start of the `machine` scene.
// An event's outcome and commands are not written here: they are looked up in
// the conformance vectors, which is the runtime's own answer.
export const steps = [
  { at: 3.5,  event: 'power' },
  { at: 8.5,  event: 'seek' },
  { at: 12,   alarm: 4.5, event: 'giveup' },          // the ring fills for 4.5 s, then the runtime fires giveup
  { at: 21,   fact: ['disc-loaded', true] },
  { at: 23,   event: 'source' },
  { at: 28.5, event: 'source' },                      // no phone: refused
  { at: 34.5, fact: ['bluetooth-paired', true] },
  { at: 36.5, event: 'source' },
  { at: 41,   event: 'power' },
  { at: 45.5, config: ['has-cd-slot', false] },
];
