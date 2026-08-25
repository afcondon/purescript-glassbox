/* Plate — the reading aids.
 *
 * Four of them, and each answers a question you cannot answer by looking at a
 * printed grid:
 *
 *   crosshair   which row and column is this cell in? (a finger, on paper)
 *   reach       WHO GETS TO THIS STATE? Point at a state anywhere — a row head,
 *               a cell, a glossary entry — and every cell that names it lights
 *               up. This is the one that matters: inbound edges are the hardest
 *               thing to check in a grid, because they are scattered down a
 *               column you have to read cell by cell.
 *   trace       the same, for a refusal tag, a command, or a config/fact id:
 *               everywhere in the machine that this word is load-bearing.
 *   open        pull the guarded ladders up into the cells they belong to.
 *
 * The page is complete without any of it. Nothing here adds information; it
 * only saves you from having to hold a column in your head.
 */
(function () {
  "use strict";

  var q = function (sel, root) { return Array.prototype.slice.call((root || document).querySelectorAll(sel)); };
  var esc = function (v) { return (window.CSS && CSS.escape) ? CSS.escape(v) : v.replace(/["\\]/g, "\\$&"); };

  /* ---------------------------------------------------------- crosshair */

  q("table.grid").forEach(function (grid) {
    grid.addEventListener("mouseover", function (ev) {
      var cell = ev.target.closest("td.c");
      clearHot(grid);
      if (!cell) return;
      var c = cell.getAttribute("data-c");
      q('[data-c="' + esc(c) + '"]', grid).forEach(function (n) { n.classList.add("hot"); });
    });
    grid.addEventListener("mouseleave", function () { clearHot(grid); });
  });

  function clearHot(root) {
    q(".hot", root).forEach(function (n) { n.classList.remove("hot"); });
  }

  /* --------------------------------------------------- reach, and traces */

  var pinned = null;

  document.addEventListener("mouseover", function (ev) {
    if (pinned) return;
    var t = ev.target.closest("[data-reach],[data-refusal],[data-command],[data-reads],.chip");
    if (t) light(t); else clearLit();
  });

  document.addEventListener("click", function (ev) {
    var tool = ev.target.closest(".tool");
    if (tool) { runTool(tool); return; }

    var t = ev.target.closest("[data-reach],[data-refusal],[data-command],[data-reads]");
    if (!t) { pinned = null; clearLit(); return; }
    if (pinned === t) { pinned = null; clearLit(); return; }
    pinned = t;
    light(t);
  });

  /* Keyboard: the row heads and the move names are real buttons, so tabbing
   * through a grid announces the same highlighting a pointer would. */
  document.addEventListener("focusin", function (ev) {
    var t = ev.target.closest("[data-reach],[data-refusal],[data-command],[data-reads],.chip");
    if (t) light(t);
  });

  document.addEventListener("keydown", function (ev) {
    if (ev.key === "Escape") { pinned = null; clearLit(); }
  });

  function light(el) {
    clearLit();
    var plate = el.closest(".plate") || document;

    var state = el.getAttribute("data-reach");
    if (state) {
      // Every cell that moves here, plus the row that IS here.
      q('td[data-target="' + esc(state) + '"]', plate).forEach(mark);
      q('.rh-id[data-reach="' + esc(state) + '"]', plate).forEach(mark);
      q('dt[data-reach="' + esc(state) + '"]', plate).forEach(mark);
      q('tr[data-state="' + esc(state) + '"]', plate).forEach(function (n) {
        n.classList.add("lit-row");
      });
    }

    var refusal = el.getAttribute("data-refusal");
    if (refusal) {
      q('[data-refusal="' + esc(refusal) + '"]', plate).forEach(mark);
    }

    var command = el.getAttribute("data-command");
    if (command) {
      q('[data-command="' + esc(command) + '"]', plate).forEach(mark);
    }

    var reads = el.getAttribute("data-reads");
    if (reads) {
      reads.split(/\s+/).filter(Boolean).forEach(function (name) {
        q('[data-reads~="' + esc(name) + '"]', plate).forEach(mark);
      });
    }

    // A chip in a cell points at its own case block below the grid.
    if (el.classList.contains("chip") && el.hash) {
      var target = document.getElementById(el.hash.slice(1));
      if (target) target.classList.add("lit");
    }
  }

  function mark(n) { n.classList.add("lit"); }

  function clearLit() {
    q(".lit").forEach(function (n) { n.classList.remove("lit"); });
    q(".lit-row").forEach(function (n) { n.classList.remove("lit-row"); });
  }

  /* ------------------------------------------------------------- tools */

  function runTool(tool) {
    var plate = tool.closest(".plate");
    var grid = plate ? plate.querySelector("table.grid") : null;
    var which = tool.getAttribute("data-tool");

    if (which === "cases" && grid) {
      var on = grid.classList.toggle("with-cases");
      tool.setAttribute("aria-pressed", on ? "true" : "false");
      tool.textContent = on ? "close the guarded cells" : "open the guarded cells";
    }
    if (which === "clear") { pinned = null; clearLit(); clearHot(document); }
  }

  q(".tool[data-tool='cases']").forEach(function (t) { t.setAttribute("aria-pressed", "false"); });

  /* -------------------------------------------------------- the spine */

  var items = q(".spine-item");
  if (items.length && "IntersectionObserver" in window) {
    var seen = {};
    var obs = new IntersectionObserver(function (entries) {
      entries.forEach(function (e) {
        seen[e.target.getAttribute("data-machine")] = e.isIntersecting ? e.intersectionRatio : 0;
      });
      var best = null, bestRatio = 0;
      Object.keys(seen).forEach(function (k) {
        if (seen[k] > bestRatio) { bestRatio = seen[k]; best = k; }
      });
      items.forEach(function (it) {
        it.classList.toggle("here", it.getAttribute("data-spine") === best);
      });
    }, { threshold: [0, 0.05, 0.2, 0.5, 0.9] });
    q("section.plate").forEach(function (s) { obs.observe(s); });
  }
})();
