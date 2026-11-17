# The Glassbox film

A two-minute promotional film, drawn as one HTML page and stepped frame by
frame in headless Chrome. Silent; the music is laid on afterwards.

```sh
spago run -p glassbox-film -- core/machines/car-radio.json film/data/geometry.json   # from the repo root
cd film && npm install          # once: puppeteer-core, driving the installed Chrome
node build.mjs                  # out/film.html
node render.mjs                 # out/glassbox-film.mp4   (about 10 minutes)
node render.mjs still 12 40.5   # single frames, to look at
node render.mjs 42 60           # one passage
```

`timeline.mjs` is the whole cut: scene lengths, the headlines, and what
happens to the machine and when.

Nothing about the machine is written here. The states and arrows are
`Glassbox.Edges` laid out by `DataViz.Layout.StateMachine` (`src/Film/Main.purs`
writes them to `data/geometry.json`, once with the CD slot and once without);
what each event does is looked up in `conformance/vectors/car-radio.json`; the
text is `core/machines/rendered/car-radio.blocks.txt`. If the film and the
runtime disagreed about the car radio, `build.mjs` or the page would throw.

The one thing that is written here is the opening counter-example, the car
radio as four booleans (`film.js`, `SOUP`).
