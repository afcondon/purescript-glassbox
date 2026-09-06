//! One machine, one string, the same on every runtime.
//!
//! `Glassbox.Canonical` exists because a machine hash taken over a JSON
//! encoding differs between runtimes — Go sorts object keys where
//! `JSON.stringify` preserves insertion order — so it writes every field in a
//! fixed order by hand, sorts every collection explicitly, and is therefore
//! identical on every runtime by construction rather than by agreement
//! between two JSON libraries. This is the same text, produced by the same
//! rules, from this side; the conformance suite carries the PureScript
//! runtime's text so the two are compared rather than trusted.
//!
//! ### What it must NOT ignore
//!
//! Three orders carry meaning and are preserved exactly: `cases`, because a
//! rule is first-match-wins; `entry` and `exit`, because commands run in the
//! order given; `notes`, because it is prose. `rules` and `deadlines` are
//! sorted by the key each is looked up under, and the sort is **stable**
//! (`sort_by` is a merge sort here as `Data.Array.sortBy` is there), so where
//! an artifact carries two entries under one key the first still wins.
//!
//! String ordering is by UTF-16 code unit, as it is in the PureScript
//! runtime, for the reason given at [`crate::run::cmp_utf16`].

use crate::run::cmp_utf16;
use crate::spec::*;

/// The version of this encoding, not of the format. Written into the first
/// line so that a stored hash says which scheme produced it.
pub const CANONICAL_VERSION: i64 = 1;

/// The canonical text of a machine.
pub fn canonical_bytes(spec: &Spec) -> String {
    let mut lines: Vec<String> = vec![
        format!("glassbox-canonical {}", CANONICAL_VERSION),
        format!("version {}", spec.version),
        format!("id {}", quoted(&spec.id)),
        format!("title {}", quoted(&spec.title)),
        format!("clock {}", spec.clock.map_or("-".to_owned(), |c| c.name().to_owned())),
        format!("initial {}", quoted(&spec.initial.0)),
    ];
    lines.extend(block("notes", spec.notes.iter().map(|n| format!("note {}", quoted(n)))));
    lines.extend(block("states", sorted_by(&spec.states, |s| &s.id.0).map(state)));
    lines.extend(block("events", sorted_by(&spec.events, |e| &e.id.0).map(event)));
    lines.extend(block("config", sorted_by(&spec.config, |c| &c.id.0).map(config)));
    lines.extend(block("facts", sorted_by(&spec.facts, |f| &f.id.0).map(fact)));
    lines.extend(block("refusals", sorted_by(&spec.refusals, |r| &r.id.0).map(refusal)));
    lines.extend(block("commands", sorted_by(&spec.commands, |c| &c.id.0).map(command)));
    lines.extend(block("rules", sorted_rules(&spec.rules).map(rule)));
    lines.extend(block("deadlines", sorted_by(&spec.deadlines, |d| &d.in_state.0).map(deadline)));
    lines.push(String::new());
    lines.join("\n")
}

/// A counted section. The count is not redundant: it makes a truncated
/// canonical form detectable rather than merely shorter.
fn block(name: &str, items: impl Iterator<Item = String>) -> Vec<String> {
    let items: Vec<String> = items.collect();
    let mut out = Vec::with_capacity(items.len() + 1);
    out.push(format!("{} {}", name, items.len()));
    out.extend(items.into_iter().map(|i| format!("  {}", i)));
    out
}

fn sorted_by<'a, T>(items: &'a [T], key: impl Fn(&T) -> &str) -> impl Iterator<Item = &'a T> {
    let mut refs: Vec<&T> = items.iter().collect();
    refs.sort_by(|a, b| cmp_utf16(key(a), key(b)));
    refs.into_iter()
}

fn sorted_rules(rules: &[Rule]) -> impl Iterator<Item = &Rule> {
    let mut refs: Vec<&Rule> = rules.iter().collect();
    refs.sort_by(|a, b| cmp_utf16(&a.from.0, &b.from.0).then_with(|| cmp_utf16(&a.on.0, &b.on.0)));
    refs.into_iter()
}

fn state(s: &StateDecl) -> String {
    [
        "state".to_owned(),
        quoted(&s.id.0),
        quoted(&s.label),
        bool(s.is_final),
        list("entry", s.entry.iter().map(|c| quoted(&c.0))),
        list("exit", s.exit.iter().map(|c| quoted(&c.0))),
    ]
    .join(" ")
}

fn event(e: &EventDecl) -> String {
    let source = match e.source {
        EventSource::FromUser => "user",
        EventSource::FromRuntime => "runtime",
    };
    ["event".to_owned(), quoted(&e.id.0), quoted(&e.label), source.to_owned()].join(" ")
}

fn config(c: &ConfigDecl) -> String {
    ["config".to_owned(), quoted(&c.id.0), quoted(&c.label), value(&c.default)].join(" ")
}

fn fact(f: &FactDecl) -> String {
    ["fact".to_owned(), quoted(&f.id.0), quoted(&f.label)].join(" ")
}

fn refusal(r: &RefusalDecl) -> String {
    ["refusal".to_owned(), quoted(&r.id.0), quoted(&r.text)].join(" ")
}

fn command(c: &CommandDecl) -> String {
    ["command".to_owned(), quoted(&c.id.0), quoted(&c.label)].join(" ")
}

/// `cases` keeps its order. See the module header — this one is behaviour.
fn rule(r: &Rule) -> String {
    ["rule".to_owned(), quoted(&r.from.0), quoted(&r.on.0), list("cases", r.cases.iter().map(case))].join(" ")
}

fn case(c: &Case) -> String {
    paren([
        "case".to_owned(),
        c.when.as_ref().map_or("-".to_owned(), guard),
        outcome(&c.outcome),
    ])
}

fn deadline(d: &Deadline) -> String {
    ["deadline".to_owned(), quoted(&d.in_state.0), quoted(&d.fires.0), expr(&d.after)].join(" ")
}

fn outcome(o: &Outcome) -> String {
    match o {
        Outcome::Move(to) => paren(["move".to_owned(), quoted(&to.0)]),
        Outcome::Stay => paren(["stay".to_owned()]),
        Outcome::Refuse(rid) => paren(["refuse".to_owned(), quoted(&rid.0)]),
    }
}

fn guard(g: &Guard) -> String {
    match g {
        Guard::Holds(e) => paren(["holds".to_owned(), expr(e)]),
        Guard::Not(inner) => paren(["not".to_owned(), guard(inner)]),
        Guard::And(gs) => {
            let mut parts = vec!["and".to_owned(), gs.len().to_string()];
            parts.extend(gs.iter().map(guard));
            paren(parts)
        }
        Guard::Or(gs) => {
            let mut parts = vec!["or".to_owned(), gs.len().to_string()];
            parts.extend(gs.iter().map(guard));
            paren(parts)
        }
        Guard::Cmp(op, a, b) => paren(["cmp".to_owned(), op.name().to_owned(), expr(a), expr(b)]),
    }
}

fn expr(e: &Expr) -> String {
    match e {
        Expr::Lit(v) => paren(["lit".to_owned(), value(v)]),
        Expr::ConfigOf(c) => paren(["config".to_owned(), quoted(&c.0)]),
        Expr::FactOf(f) => paren(["fact".to_owned(), quoted(&f.0)]),
    }
}

fn value(v: &Value) -> String {
    match v {
        Value::Number(n) => paren(["num".to_owned(), number(*n)]),
        Value::Boolean(b) => paren(["bool".to_owned(), bool(*b)]),
        Value::String(s) => paren(["str".to_owned(), quoted(s)]),
    }
}

/// The one place a runtime could disagree, handled rather than hoped about.
///
/// An integral number that fits a PureScript `Int` (32-bit) is written as
/// exact digits, as `Int.fromNumber` and `show` would write it. Anything
/// else goes through [`js_number`], which reproduces `Number.prototype
/// .toString` closely enough for every value the corpus could plausibly
/// carry — and the conformance suite is what says whether "closely enough"
/// held, one line at a time.
pub fn number(n: f64) -> String {
    if n.is_nan() {
        "nan".to_owned()
    } else if n.is_infinite() {
        if n > 0.0 { "inf".to_owned() } else { "-inf".to_owned() }
    } else if n.fract() == 0.0 && (-2147483648.0..=2147483647.0).contains(&n) {
        format!("{}", n as i64)
    } else {
        js_number(n)
    }
}

/// `Number.prototype.toString`, transcribed.
///
/// JavaScript prints an integral value below 1e21 as digits, uses an
/// exponent at or above 1e21 and below 1e-6, and otherwise prints the
/// shortest decimal that round-trips — which is also what Rust's `Display`
/// prints for the middle case, so only the two ends need help: the sign on a
/// positive exponent, and negative zero.
pub fn js_number(n: f64) -> String {
    if n == 0.0 {
        return "0".to_owned();
    }
    if n.is_nan() {
        return "NaN".to_owned();
    }
    if n.is_infinite() {
        return if n > 0.0 { "Infinity".to_owned() } else { "-Infinity".to_owned() };
    }
    let abs = n.abs();
    if !(1e-6..1e21).contains(&abs) {
        // Rust writes `1.5e-7` and `1e21`; JavaScript writes `1.5e-7` and
        // `1e+21`.
        let s = format!("{:e}", n);
        return match s.find('e') {
            Some(i) if !s[i + 1..].starts_with('-') => format!("{}e+{}", &s[..i], &s[i + 1..]),
            _ => s,
        };
    }
    format!("{}", n)
}

fn bool(b: bool) -> String {
    (if b { "true" } else { "false" }).to_owned()
}

/// A counted, parenthesised list, so nesting needs no delimiter discipline.
fn list(name: &str, items: impl Iterator<Item = String>) -> String {
    let items: Vec<String> = items.collect();
    let mut parts = vec![name.to_owned(), items.len().to_string()];
    parts.extend(items);
    paren(parts)
}

fn paren(parts: impl IntoIterator<Item = String>) -> String {
    format!("({})", parts.into_iter().collect::<Vec<_>>().join(" "))
}

/// Length-free escaping: nothing here counts characters, because what counts
/// as one character is exactly the sort of thing two runtimes disagree about.
/// The backslash must be escaped first or it would escape the escapes.
fn quoted(s: &str) -> String {
    let escaped = s
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t");
    format!("\"{}\"", escaped)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn integers_are_digits_and_fractions_are_shortest() {
        assert_eq!(number(8.0), "8");
        assert_eq!(number(-3.0), "-3");
        assert_eq!(number(0.5), "0.5");
        assert_eq!(number(0.1), "0.1");
        assert_eq!(number(3000000000.0), "3000000000");
    }

    #[test]
    fn the_ends_follow_javascript() {
        assert_eq!(js_number(1e21), "1e+21");
        assert_eq!(js_number(1.5e-7), "1.5e-7");
        assert_eq!(js_number(-0.0), "0");
        assert_eq!(number(f64::NAN), "nan");
        assert_eq!(number(f64::NEG_INFINITY), "-inf");
    }

    #[test]
    fn quoting_escapes_the_backslash_first() {
        assert_eq!(quoted("a\\\"b\n"), "\"a\\\\\\\"b\\n\"");
    }
}
