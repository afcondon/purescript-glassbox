// The film as a function of time. `window.seek(t)` puts every element where it
// is at second t; nothing here runs on a clock, so a frame is reproducible.
(() => {
  const D = window.DATA, T = D.timeline, S = T.scenes;
  const stage = document.getElementById('stage');

  const cl = (x) => Math.max(0, Math.min(1, x));
  const sp = (t, a, b) => cl((t - a) / (b - a));
  const ez = (x) => x * x * (3 - 2 * x);
  const lerp = (a, b, u) => a + (b - a) * u;
  const esc = (s) => s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  const el = (tag, cls, html) => { const e = document.createElement(tag); if (cls) e.className = cls; if (html != null) e.innerHTML = html; return e; };
  const scene = (id) => { const e = el('div', 'scene'); e.id = id; stage.appendChild(e); return e; };
  const show = (e, o) => { e.style.opacity = o; e.style.visibility = o <= 0 ? 'hidden' : 'visible'; };
  const cls = (e, name, on) => e.classList.toggle(name, !!on);

  // ------------------------------------------------------------ the machine, replayed
  const clone = (x) => JSON.parse(JSON.stringify(x));
  const world0 = { config: Object.fromEntries(D.spec.config.map((c) => [c.id, c.default])), facts: Object.fromEntries(D.spec.facts.map((f) => [f.id, false])) };
  const answer = (state, event, world) => {
    const v = D.vectors.find((x) => x.from === state && x.event === event &&
      Object.entries(x.config).every(([k, val]) => world.config[k] === val) &&
      Object.entries(x.facts).every(([k, val]) => world.facts[k] === val));
    if (!v) throw new Error(`no vector for ${state} × ${event}`);
    return v;
  };
  const moments = (() => {
    let state = D.spec.initial; const world = clone(world0), out = [];
    for (const s of T.steps) {
      const m = { ...s, before: state };
      if (s.fact) world.facts[s.fact[0]] = s.fact[1];
      if (s.config) world.config[s.config[0]] = s.config[1];
      if (s.event) { m.v = answer(state, s.event, world); state = m.v.current; m.fire = s.at + (s.alarm || 0); }
      m.after = state; m.world = clone(world);
      out.push(m);
    }
    return out;
  })();
  const events = moments.filter((m) => m.event);
  const ARRIVE = 1.5, LIT = 3.0;

  // ------------------------------------------------------------ the printed rules
  const paint = (l) => esc(l)
    .replace(/-&gt; (refuse [a-z-]+)/, '-&gt; <b>$1</b>')
    .replace(/\b(when|elif|else|entry|exit|after|fires)\b/g, '<em>$1</em>')
    .replace(/^([a-z-]+:)/, '<strong>$1</strong>')
    .replace(/^(machine|format|clock|initial|states|events|config|facts|refusals|commands)\b/, '<strong>$1</strong>');
  const blockStart = (state) => D.rules.findIndex((l) => l.startsWith(state + ':'));
  const ruleLine = (state, event, want) => {
    let held = [];
    for (let i = blockStart(state) + 1; i < D.rules.length && !/^[a-z-]+:/.test(D.rules[i]); i++) {
      const l = D.rules[i], head = l.match(/^  ([a-z-]+(?:, [a-z-]+)*)\s+(?:when\b|->)/);
      if (head) held = head[1].split(', ');
      else if (!/^\s+(elif|else)\b/.test(l)) { held = []; continue; }
      const rhs = (l.split('->')[1] || '').trim();
      if (held.includes(event) && rhs === want) return i;
    }
    throw new Error(`no printed rule for ${state} × ${event} -> ${want}`);
  };
  const wantOf = (v) => v.outcome === 'move' ? v.current : v.outcome === 'refuse' ? `refuse ${v.refusal}` : 'stay';

  // ------------------------------------------------------------ the drawing
  const fitted = D.geometry.fitted.layout, reshaped = D.geometry.withoutCdSlot;
  const edgeKey = (e) => `${e.from}>${e.to}|${e.kind}|${e.label}`;
  const survives = new Set(reshaped.layout.transitions.map(edgeKey));
  const lost = new Set(reshaped.unreachable);
  const bez = (e, u) => ({ x: (1 - u) * (1 - u) * e.startX + 2 * (1 - u) * u * e.controlX + u * u * e.endX, y: (1 - u) * (1 - u) * e.startY + 2 * (1 - u) * u * e.controlY + u * u * e.endY });
  const edgeFor = (from, to, event) => fitted.transitions.findIndex((e) => e.from === from && e.to === to &&
    (event === 'giveup' ? e.kind === 'deadline' : e.kind === 'event' && e.label.split(', ').includes(D.spec.eventLabels[event])));
  const firstEdgeFor = (from, event) => fitted.transitions.findIndex((e) => e.from === from && e.kind === 'event' && e.label.split(', ').includes(D.spec.eventLabels[event]));

  let drawings = 0;
  function drawing() {
    const p = `d${drawings++}`, L = fitted;
    const marker = (id, c) => `<marker id="${p}${id}" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7.5" markerHeight="7.5" markerUnits="userSpaceOnUse" orient="auto"><path d="M0,0.8 L10,5 L0,9.2 z" class="${c}"/></marker>`;
    const ia = L.initialArrow;
    const svg = `<svg class="dg" viewBox="${L.originX} ${L.originY} ${L.width} ${L.height}">
<defs>${marker('ah', 'ah')}${marker('ahAuto', 'ahAuto')}${marker('ahOn', 'ahOn')}${marker('ahNo', 'ahNo')}</defs>
<path class="init" d="M${ia.x - 34 * Math.cos(ia.angle)},${ia.y - 34 * Math.sin(ia.angle)} L${ia.x},${ia.y}" marker-end="url(#${p}ah)"/>
${L.transitions.map((e, i) => {
      const w = e.label.length * 7.9 + 12;
      return `<g class="e${e.kind === 'deadline' ? ' auto' : ''}" data-e="${i}"><path d="M${e.startX},${e.startY} Q${e.controlX},${e.controlY} ${e.endX},${e.endY}"/>
<rect class="lb" x="${e.labelX - w / 2}" y="${e.labelY - 10}" width="${w}" height="20"/><text x="${e.labelX}" y="${e.labelY + 4.5}">${e.label}</text></g>`;
    }).join('')}
<ellipse class="timer" pathLength="1" stroke-dasharray="1" stroke-dashoffset="1"/><text class="timerLabel"></text>
${L.states.map((s) => `<g class="n" data-n="${s.id}"><ellipse cx="${s.cx}" cy="${s.cy}" rx="${s.rx}" ry="${s.ry}"/><text x="${s.cx}" y="${s.cy + 6.5}">${s.label}</text><text class="un" x="${s.cx}" y="${s.cy + s.ry + 19}" text-anchor="middle">unreachable</text></g>`).join('')}
<circle class="tok" r="9.5"/>
<g class="tag"><rect width="236" height="58"/><text class="b" x="14" y="23">REFUSED</text><text class="id" x="92" y="23"></text><text class="q" x="14" y="45"></text></g>
</svg>`;
    const root = el('div', null, svg).firstChild;
    const q = (s) => root.querySelector(s), qa = (s) => [...root.querySelectorAll(s)];
    const edges = qa('.e'), nodes = qa('.n'), tok = q('.tok'), timer = q('.timer'), timerLabel = q('.timerLabel'), tag = q('.tag');
    const stateAt = (id) => L.states.find((s) => s.id === id);
    const update = (st) => {
      edges.forEach((g, i) => {
        const e = L.transitions[i], on = st.lit === i, auto = e.kind === 'deadline';
        cls(g, 'on', on && !st.refused); cls(g, 'no', on && st.refused);
        g.firstChild.setAttribute('marker-end', `url(#${p}${on ? (st.refused ? 'ahNo' : 'ahOn') : auto ? 'ahAuto' : 'ah'})`);
        const gone = !survives.has(edgeKey(e)), dim = lost.has(e.from) || lost.has(e.to);
        g.style.opacity = gone ? 1 - st.reshape : dim ? lerp(1, .22, st.reshape) : 1;
      });
      nodes.forEach((g) => {
        const id = g.dataset.n, away = lost.has(id);
        cls(g, 'cur', st.current === id && !(st.refuseNode === id)); cls(g, 'ref', st.refuseNode === id);
        g.firstChild.style.opacity = g.children[1].style.opacity = away ? lerp(1, .3, st.reshape) : 1;
        g.children[2].style.opacity = away ? st.reshape : 0;
      });
      if (st.token) { const pt = bez(L.transitions[st.token.edge], st.token.u); tok.setAttribute('cx', pt.x); tok.setAttribute('cy', pt.y); cls(tok, 'no', st.refused); tok.style.opacity = 1; }
      else tok.style.opacity = 0;
      if (st.timer) {
        const s = stateAt(st.timer.state);
        for (const [k, v] of [['cx', s.cx], ['cy', s.cy], ['rx', s.rx + 10], ['ry', s.ry + 10], ['stroke-dashoffset', 1 - st.timer.p]]) timer.setAttribute(k, v);
        timerLabel.setAttribute('x', s.cx + s.rx + 14); timerLabel.setAttribute('y', s.cy + s.ry + 30); timerLabel.textContent = st.timer.label;
        timer.style.opacity = timerLabel.style.opacity = 1;
      } else timer.style.opacity = timerLabel.style.opacity = 0;
      if (st.tag) {
        const s = stateAt(st.tag.state);
        tag.setAttribute('transform', `translate(${s.cx - 118},${s.cy + s.ry + 12})`);
        tag.querySelector('.id').textContent = st.tag.id; tag.querySelector('.q').textContent = `“${st.tag.text}”`;
        tag.style.opacity = st.tag.o;
      } else tag.style.opacity = 0;
    };
    return { root, update };
  }
  const still = { current: null, lit: -1, refused: false, refuseNode: null, token: null, timer: null, tag: null, reshape: 0 };

  // ------------------------------------------------------------ headline
  const capBox = el('div', 'cap'); stage.appendChild(capBox);
  const caps = T.captions.map((c) => { const e = el('div', null, `<div class="t">${esc(c.text)}</div>${c.sub ? `<div class="s">${esc(c.sub)}</div>` : ''}`); e.style.position = 'absolute'; e.style.inset = '0 0 auto 0'; capBox.appendChild(e); return e; });
  const subs = caps.map((e) => e.querySelector('.s'));
  const subBox = el('div', 'subtitles'); stage.appendChild(subBox);
  const subtitles = T.subtitles.map((c) => { const e = el('div', null, esc(c.text)); subBox.appendChild(e); return e; });

  // ------------------------------------------------------------ title
  const title = scene('title');
  title.innerHTML = `<div class="tl"><div class="eyebrow">A state machine library</div><h1>Glass<br>box</h1><div class="lede">The machine is a text file.<br>You can read it, run it and draw it.</div></div>
<div class="box"></div><div class="lid"><span>events in</span><span>behaviour out</span></div>`;
  const titleDrawing = drawing(); title.querySelector('.box').appendChild(titleDrawing.root);
  const cover = el('div', 'cover'); title.querySelector('.box').appendChild(cover);
  titleDrawing.update({ ...still, current: D.spec.initial });
  const titleLede = title.querySelector('.lede'), titleLid = title.querySelector('.lid');

  // ------------------------------------------------------------ soup
  const SOUP = `type Radio =
  { on        :: Boolean
  , seeking   :: Boolean
  , cd        :: Boolean
  , bluetooth :: Boolean
  }

press :: Button -> Radio -> Radio
press Source r
  | not r.on    = r
  | r.seeking   = r
  | r.cd        = r { cd = false, bluetooth = true }
  | r.bluetooth = r { bluetooth = false }
  | otherwise   = r { cd = true }
press Seek r
  | r.on && not r.cd = r { seeking = true }
  | otherwise        = r
press Power r = r { on = not r.on }`.split('\n');
  const FIELDS = ['on', 'seeking', 'cd', 'bluetooth'];
  const LEGAL = { '0000': 'off', '1000': 'radio', '1100': 'seeking', '1010': 'cd', '1001': 'bluetooth' };
  const soup = scene('soup');
  soup.innerHTML = `<pre class="code">${SOUP.map((l) => `<span>${esc(l).replace(/Boolean/, '<b>Boolean</b>') || ' '}</span>`).join('')}</pre>
<div class="right"><div class="cols"><span class="bits"><i></i><i></i><i></i><i></i></span><span>each dot is one boolean, in order: ${FIELDS.join(', ')}</span></div><div class="combos">${Array.from({ length: 16 }, (_, i) => {
    const bits = [8, 4, 2, 1].map((m) => (i & m ? 1 : 0)), name = LEGAL[bits.join('')] || '';
    return `<div class="combo" data-name="${name}"><div class="hatch"></div><span class="bits">${bits.map((b) => `<i class="${b ? 't' : ''}"></i>`).join('')}</span><span class="nm">${name}</span></div>`;
  }).join('')}</div></div>
<div class="press"><span class="ev">power</span><span></span></div>`;
  const soupLines = [...soup.querySelectorAll('.code span')], combos = [...soup.querySelectorAll('.combo')];
  const press = soup.querySelector('.press'), pressNote = press.lastChild;
  const CD = 0b1010, STUCK = 0b0010;
  const illegal = combos.map((c, i) => i).filter((i) => !combos[i].dataset.name);

  // ------------------------------------------------------------ the file
  const file = scene('file');
  const third = Math.ceil(D.whole.length / 3);
  // break columns at blank lines where one is near, so no block is split mid-rule
  const cuts = [0];
  for (let k = 1; k < 3; k++) { let c = k * third; for (let d = 0; d < 6; d++) { if (!D.whole[c + d]?.trim()) { c = c + d + 1; break; } if (!D.whole[c - d]?.trim()) { c = c - d + 1; break; } } cuts.push(c); }
  cuts.push(D.whole.length);
  file.innerHTML = `<div class="cols3"><div class="hd colhd">Car radio<span>the whole machine, as its author reads it</span></div>${[0, 1, 2].map((k) =>
    `<pre class="bl">${D.whole.slice(cuts[k], cuts[k + 1]).map((l) => `<span>${paint(l) || ' '}</span>`).join('')}</pre>`).join('')}</div>`;
  const fileLines = [...file.querySelectorAll('.bl span')];

  // ------------------------------------------------------------ machine
  const machine = scene('machine');
  const kindOf = (state, event) => {
    const outs = [...new Set(D.vectors.filter((v) => v.from === state && v.event === event).map(wantOf))];
    if (outs.length > 1) return ['guard', null];
    return outs[0] === 'stay' ? ['stay', '·'] : outs[0].startsWith('refuse ') ? ['refuse', outs[0].slice(7)] : ['move', outs[0]];
  };
  let guards = 0;
  const longest = Math.max(...D.rules.map((l) => l.length));
  machine.innerHTML = `<div class="ctext"><div class="hd">Car radio<span>the file</span></div><div class="window"><pre class="bl" style="font-size:${Math.min(15.5, 610 / (longest * 0.602)).toFixed(2)}px">${D.rules.map((l) => `<span>${paint(l) || ' '}</span>`).join('')}</pre></div></div>
<div class="cgrid"><div class="hd">state × event<span>every pair</span></div>
<table class="gr"><tr><th></th>${D.spec.events.map((e) => `<th data-ev="${e}">${e}</th>`).join('')}</tr>${D.spec.states.map((s) => `<tr data-row="${s}"><th data-st="${s}">${s}</th>${D.spec.events.map((e) => {
    const [k, text] = kindOf(s, e); return `<td class="${k}" data-cell="${s}|${e}">${k === 'guard' ? `<i>${++guards}</i>` : text}</td>`;
  }).join('')}</tr>`).join('')}</table>
<div class="key"><span class="k-move">moves</span><span class="k-stay">stays</span><span class="k-ref">refuses</span><span class="k-g">depends on the world</span></div>
<div class="world"><div class="hd">the world<span>as the host reports it</span></div>
${D.spec.facts.map((f) => `<div class="fact" data-w="${f.id}"><i></i>${f.id}<span>fact</span></div>`).join('')}
${D.spec.config.filter((c) => typeof c.default === 'boolean').map((c) => `<div class="fact" data-w="${c.id}"><i></i>${c.id}<span>config</span></div>`).join('')}</div></div>
<div class="cdraw"><div class="hd">the drawing<span>derived from the file</span></div></div>
<div class="tape"><div><span class="k">events</span><span class="evs"></span></div><div><span class="k">the host is told</span><span class="cmds"></span></div></div>`;
  const main = drawing(); machine.querySelector('.cdraw').appendChild(main.root);
  const textPre = machine.querySelector('.ctext .bl'), textLines = [...textPre.children];
  const lineHeight = () => textLines[1].offsetTop - textLines[0].offsetTop;
  const grid = machine.querySelector('.gr'), cells = [...grid.querySelectorAll('td')], heads = [...grid.querySelectorAll('th[data-ev]')], rowHeads = [...grid.querySelectorAll('th[data-st]')], rows = [...grid.querySelectorAll('tr[data-row]')];
  const worldRows = [...machine.querySelectorAll('.fact')];
  const evs = machine.querySelector('.evs'), cmds = machine.querySelector('.cmds');
  const scrollFor = (state) => Math.max(0, blockStart(state) - 1);
  const focusChanges = [{ at: -1e9, line: scrollFor(D.spec.initial) }, ...events.map((m) => ({ at: m.fire - 0.9, line: scrollFor(m.before) }))];
  const reshapeStep = moments.find((m) => m.config);
  const reshapeLine = ruleLine('radio', 'source', 'refuse not-fitted');
  focusChanges.push({ at: reshapeStep.at - 0.2, line: scrollFor('radio') });

  function seekMachine(lt) {
    const fired = events.filter((m) => lt >= m.fire);
    const last = fired[fired.length - 1];
    const active = last && lt < last.fire + LIT ? last : null;
    const arrived = events.filter((m) => lt >= m.fire + ARRIVE);
    const current = arrived.length ? arrived[arrived.length - 1].after : D.spec.initial;
    const settled = moments.filter((m) => lt >= m.at);
    const world = settled.length ? settled[settled.length - 1].world : world0;
    const refused = !!active && active.v.outcome === 'refuse';
    const reshape = ez(sp(lt, reshapeStep.at + 0.5, reshapeStep.at + 2.2));

    // drawing
    const st = { ...still, current, refused, reshape };
    if (active) {
      const a = lt - active.fire, v = active.v;
      const edge = v.outcome === 'move' ? edgeFor(active.before, v.current, active.event) : v.outcome === 'refuse' ? firstEdgeFor(active.before, active.event) : -1;
      st.lit = edge;
      if (edge >= 0 && v.outcome === 'move' && a > 0.25 && a < ARRIVE + 0.05) st.token = { edge, u: ez(sp(a, 0.3, ARRIVE)) };
      if (edge >= 0 && refused && a > 0.25 && a < 1.75) st.token = { edge, u: 0.4 * Math.sin(Math.PI * sp(a, 0.3, 1.7)) };
      if (refused && a > 0.95) { st.refuseNode = active.before; st.tag = { state: active.before, id: v.refusal, text: D.spec.refusals[v.refusal], o: 1 - sp(a, LIT - 0.3, LIT) }; }
    }
    const waiting = moments.find((m) => m.alarm && lt >= m.at && lt < m.fire);
    if (waiting) {
      const alarm = D.alarms.find((x) => x.state === waiting.before).alarm, p = sp(lt, waiting.at, waiting.fire);
      st.timer = { state: waiting.before, p, label: `${alarm.fires} in ${Math.max(1, Math.ceil(alarm.after * (1 - p)))} s` };
    }
    main.update(st);

    // text
    let scroll = focusChanges[0].line;
    for (const c of focusChanges) if (lt >= c.at) scroll = lerp(scroll, c.line, ez(sp(lt, c.at, c.at + 0.8)));
    const maxScroll = Math.max(0, textLines.length - Math.floor(600 / lineHeight()));
    textPre.style.transform = `translateY(${-Math.min(scroll, maxScroll) * lineHeight()}px)`;
    const hit = active ? ruleLine(active.before, active.event, wantOf(active.v)) : reshape > 0 ? reshapeLine : -1;
    textLines.forEach((l, i) => { cls(l, 'hit', i === hit); cls(l, 'no', i === hit && (refused || !active)); });

    // grid
    cls(grid, 'no', refused);
    cells.forEach((c) => cls(c, 'hit', !!active && c.dataset.cell === `${active.before}|${active.event}`));
    heads.forEach((h) => cls(h, 'x', !!active && h.dataset.ev === active.event));
    rowHeads.forEach((h) => cls(h, 'x', !!active && h.dataset.st === active.before));
    rows.forEach((r) => { r.style.opacity = lost.has(r.dataset.row) ? lerp(1, .3, reshape) : 1; });

    // world
    worldRows.forEach((r) => {
      const id = r.dataset.w, on = id in world.facts ? world.facts[id] : world.config[id];
      cls(r.firstChild, 't', on);
      const change = settled.filter((m) => (m.fact && m.fact[0] === id) || (m.config && m.config[0] === id)).pop();
      const glow = change ? 1 - sp(lt, change.at + 1.2, change.at + 2.2) : 0;
      r.style.background = glow > 0 ? `rgba(20, 84, 140, ${0.16 * glow})` : 'transparent';
    });

    // tape
    evs.innerHTML = fired.map((m) => `<span class="ev${m === active ? ' now' : ''}${m === active && refused ? ' no' : ''}${m.alarm ? ' rt' : ''}">${m.event}</span>`).join(' ');
    const told = arrived.length ? arrived[arrived.length - 1] : null;
    cmds.innerHTML = !told ? D.start.map((c) => `<span class="cmd">${c}</span>`).join('')
      : told.v.commands.length ? told.v.commands.map((c) => `<span class="cmd">${c}</span>`).join('')
        : `<span class="cmd none">nothing</span>`;
  }

  // ------------------------------------------------------------ runtimes
  const runtimes = scene('runtimes');
  const total = D.suites.reduce((n, s) => n + s.cases, 0);
  const tick = '<svg viewBox="0 0 12 12" class="tk"><path d="M2,6.5 L5,9.5 L10,3"/></svg>';
  const worldText = (v) => [...Object.entries(v.config), ...Object.entries(v.facts)].map(([k, x]) => (x === true ? k : x === false ? `no ${k}` : `${k} ${x}`)).join(', ');
  const cols = '<colgroup><col class="c1"><col class="c2"><col class="c3"><col class="c4"><col><col class="r"><col class="r"><col class="r"></colgroup>';
  runtimes.innerHTML = `<div class="lhs"><div class="eyebrow">Cases checked</div><div class="n">0</div>
<p>Every state, every event, every way the guards can fall. Each runtime answers all of them.</p>
<div class="suites">${D.suites.map((s) => `<span>${s.machine}</span><b>${s.cases}</b>`).join('')}</div></div>
<div class="rhs"><table class="vec">${cols}<tr><th>state</th><th>event</th><th>world</th><th>answer</th><th>commands</th><th class="r">JavaScript</th><th class="r">BEAM</th><th class="r">Rust</th></tr></table>
<div class="rows"><table class="vec">${cols}${D.vectors.map((v) => `<tr><td>${v.from}</td><td>${v.event}</td><td class="w">${worldText(v)}</td><td class="${v.outcome === 'refuse' ? 'ref' : v.outcome === 'move' ? 'mv' : 'st'}">${wantOf(v)}</td><td class="c">${v.commands.join(', ')}</td><td class="r">${tick}</td><td class="r">${tick}</td><td class="r">${tick}</td></tr>`).join('')}</table></div></div>
<div class="hosts"><div><b>Bosun</b> a process supervisor, running its machine on the BEAM</div><div><b>Itajara</b> an audio looper in Rust, held to the same file</div></div>`;
  const counter = runtimes.querySelector('.n'), vecBody = runtimes.querySelector('.rows table'), vecRows = [...vecBody.querySelectorAll('tr')], hosts = runtimes.querySelector('.hosts');
  const ticks = vecRows.map((r) => [...r.querySelectorAll('.tk')]);
  function seekRuntimes(lt) {
    const rate = 6.4 / vecRows.length, head = (lt - 1.6) / rate;           // which row is being checked
    ticks.forEach((ts, i) => ts.forEach((k, j) => { k.style.opacity = sp(lt, 1.6 + i * rate + j * 0.09, 1.6 + i * rate + j * 0.09 + 0.12); }));
    const view = 10, scroll = Math.max(0, Math.min(vecRows.length - view, head - 7));
    vecBody.style.transform = `translateY(${-scroll * 58}px)`;
    counter.textContent = Math.round(total * ez(sp(lt, 1.0, 4.0)));
    show(hosts, sp(lt, 11.4, 12.2));
  }

  // ------------------------------------------------------------ end
  const end = scene('end');
  end.innerHTML = `<div class="in"><div class="eyebrow">State machines as text files</div><h1>Glass<br>box</h1>
<div class="lede">Write the machine down.<br>Read it, run it, draw it, check it.</div><div class="url">github.com/afcondon/purescript-glassbox</div></div>
<div class="rt"><span>PureScript on JavaScript and the BEAM</span><span>Rust</span><span>${total} cases, one set of answers</span></div>`;
  const endParts = [end.querySelector('.lede'), end.querySelector('.url'), end.querySelector('.rt')];

  // ------------------------------------------------------------ seek
  const scenesByName = { title, soup, file, machine, runtimes, end };
  window.seek = (t) => {
    for (const [name, e] of Object.entries(scenesByName)) {
      const s = S[name];
      show(e, Math.min(name === 'title' ? 1 : sp(t, s.from, s.from + 0.45), name === 'end' ? 1 : 1 - sp(t, s.to - 0.45, s.to)) * (t >= s.from - 0.01 && t <= s.to + 0.01 ? 1 : 0));
    }
    T.captions.forEach((c, i) => {
      show(caps[i], Math.min(sp(t, c.from, c.from + 0.4), 1 - sp(t, c.to - 0.4, c.to)));
      if (subs[i]) subs[i].style.opacity = sp(t, c.from + 2.6, c.from + 3.2);
    });

    T.subtitles.forEach((c, i) => show(subtitles[i], Math.min(sp(t, c.from, c.from + 0.25), 1 - sp(t, c.to - 0.25, c.to))));

    { // title
      const lt = t - S.title.from;
      cover.style.width = `${100 * (1 - ez(sp(lt, 1.6, 4.6)))}%`;
      titleLede.style.opacity = sp(lt, 4.4, 5.2); titleLid.style.opacity = sp(lt, 0.4, 1.2);
    }
    { // soup
      const lt = t - S.soup.from;
      soupLines.forEach((l, i) => { l.style.opacity = sp(lt, 0.5 + i * 0.24, 0.5 + i * 0.24 + 0.2); cls(l, 'bug', i === soupLines.length - 1 && lt > 16.2); });
      soup.querySelector('.cols').style.opacity = sp(lt, 6.2, 6.8);
      combos.forEach((c, i) => {
        c.style.opacity = sp(lt, 6.6 + i * 0.19, 6.6 + i * 0.19 + 0.18);
        const k = illegal.indexOf(i), out = k >= 0 ? sp(lt, 11.6 + k * 0.24, 11.6 + k * 0.24 + 0.25) : 0;
        c.firstChild.style.opacity = out; cls(c, 'no', out > 0.5);
        c.querySelector('.nm').style.opacity = k >= 0 ? 1 : sp(lt, 11.4, 12);
        cls(c, 'here', i === CD && lt > 15.6 && lt < 18.4);
        const stuck = i === STUCK && lt >= 18.4;
        cls(c, 'wrong', stuck); if (i === STUCK) { c.querySelector('.nm').textContent = stuck ? 'off + cd' : ''; if (stuck) { c.firstChild.style.opacity = 0; c.classList.remove('no'); } }
      });
      press.style.opacity = sp(lt, 17.2, 17.6);
      pressNote.textContent = lt >= 18.4 ? 'on is now false. cd is still true.' : 'pressed while the CD plays';
    }
    { // file
      const lt = t - S.file.from;
      fileLines.forEach((l, i) => { l.style.opacity = sp(lt, 0.5 + i * 0.03, 0.5 + i * 0.03 + 0.25); });
    }
    seekMachine(t - S.machine.from);
    seekRuntimes(t - S.runtimes.from);
    { // end
      const lt = t - S.end.from;
      endParts.forEach((e, i) => { e.style.opacity = sp(lt, 1 + i * 0.7, 1.6 + i * 0.7); });
    }
  };
  window.seek(0);
})();
