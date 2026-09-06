//! Replaying the PureScript runtime's table.
//!
//! `conformance/vectors/<machine>.json` is `Glassbox.Conformance`'s output:
//! every (state × event × guard-assignment) the artifact admits and what
//! `Glassbox.Run.step` answered. This module reads that file and asks
//! [`crate::run::step`] the same questions, and every disagreement comes back
//! as a [`Mismatch`] naming the machine, the state, the event and the
//! assignment — because that is the reproducing case, and a conformance
//! failure that says only "different" has replaced a diff with a shrug.
//!
//! The schema is documented in `conformance/README.md`. What a vector does
//! not say is part of the contract: config it does not mention is at the
//! machine's default, facts it does not mention are unreported.

use crate::canonical::canonical_bytes;
use crate::codec::DecodeError;
use crate::run::{alarm_for, start_commands, step, World};
use crate::spec::*;
use serde::Deserialize;
use serde_json::{Map, Value as Json};
use std::fmt;

/// The version of the suite file this replayer reads.
pub const SUITE_VERSION: i64 = 1;

/// One machine's table, as written by the PureScript side.
#[derive(Debug, Deserialize)]
pub struct Suite {
    #[serde(rename = "glassbox-vectors")]
    pub suite_version: i64,
    pub machine: String,
    pub glassbox: i64,
    pub canonical: String,
    pub start: Vec<String>,
    pub vectors: Vec<Vector>,
    pub alarms: Vec<AlarmVector>,
}

/// One line of the table.
#[derive(Debug, Deserialize)]
pub struct Vector {
    pub from: String,
    pub event: String,
    #[serde(default)]
    pub config: Map<String, Json>,
    #[serde(default)]
    pub facts: Map<String, Json>,
    pub outcome: String,
    #[serde(default)]
    pub refusal: Option<String>,
    pub current: String,
    pub commands: Vec<String>,
}

#[derive(Debug, Deserialize)]
pub struct AlarmVector {
    pub state: String,
    #[serde(default)]
    pub config: Map<String, Json>,
    #[serde(default)]
    pub facts: Map<String, Json>,
    pub alarm: Option<AlarmWire>,
}

#[derive(Debug, Deserialize)]
pub struct AlarmWire {
    pub fires: String,
    pub after: f64,
    pub clock: Option<String>,
}

/// Where the two runtimes disagreed, and about what.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Mismatch {
    pub machine: String,
    /// `vector`, `alarm`, `start`, `canonical` or `suite`.
    pub what: &'static str,
    pub state: String,
    pub event: String,
    pub assignment: String,
    pub expected: String,
    pub got: String,
}

impl fmt::Display for Mismatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} / {}: state `{}`, event `{}`, {}\n    expected {}\n    got      {}",
            self.machine, self.what, self.state, self.event, self.assignment, self.expected, self.got
        )
    }
}

impl Suite {
    /// Read a suite from text.
    pub fn parse(text: &str) -> Result<Suite, DecodeError> {
        serde_json::from_str(text)
            .map_err(|e| DecodeError { path: String::new(), message: format!("not a vectors file: {}", e) })
    }
}

/// Every place this runtime disagrees with the table. Empty means conformant.
pub fn replay(spec: &Spec, suite: &Suite) -> Vec<Mismatch> {
    let mut out = Vec::new();
    let machine = suite.machine.clone();
    let mismatch = |what, state: &str, event: &str, assignment: String, expected: String, got: String| Mismatch {
        machine: machine.clone(),
        what,
        state: state.to_owned(),
        event: event.to_owned(),
        assignment,
        expected,
        got,
    };

    if suite.suite_version != SUITE_VERSION {
        out.push(mismatch(
            "suite",
            "",
            "",
            String::new(),
            format!("glassbox-vectors {}", SUITE_VERSION),
            format!("glassbox-vectors {}", suite.suite_version),
        ));
        return out;
    }
    if suite.machine != spec.id {
        out.push(mismatch("suite", "", "", String::new(), suite.machine.clone(), spec.id.clone()));
    }
    if suite.glassbox != spec.version {
        out.push(mismatch("suite", "", "", "format version".to_owned(), suite.glassbox.to_string(), spec.version.to_string()));
    }

    let ours = canonical_bytes(spec);
    if ours != suite.canonical {
        let (expected, got) = first_difference(&suite.canonical, &ours);
        out.push(mismatch("canonical", "", "", "first differing line".to_owned(), expected, got));
    }

    let start: Vec<String> = start_commands(spec).iter().map(ToString::to_string).collect();
    if start != suite.start {
        out.push(mismatch("start", spec.initial.as_str(), "", String::new(), format!("{:?}", suite.start), format!("{:?}", start)));
    }

    for v in &suite.vectors {
        let assignment = describe(&v.config, &v.facts);
        let world = match world_of(spec, &v.config, &v.facts) {
            Ok(w) => w,
            Err(e) => {
                out.push(mismatch("vector", &v.from, &v.event, assignment, "a readable assignment".to_owned(), e.to_string()));
                continue;
            }
        };
        let got = step(spec, &world, &StateId::new(&v.from), &EventId::new(&v.event));
        let expected_outcome = match (v.outcome.as_str(), &v.refusal) {
            ("move", _) => Outcome::Move(StateId::new(&v.current)),
            ("stay", _) => Outcome::Stay,
            ("refuse", Some(tag)) => Outcome::Refuse(RefusalId::new(tag)),
            (other, _) => {
                out.push(mismatch("vector", &v.from, &v.event, assignment, "move, stay or refuse".to_owned(), other.to_owned()));
                continue;
            }
        };
        let got_commands: Vec<String> = got.commands.iter().map(ToString::to_string).collect();
        if got.outcome != expected_outcome || got.current.as_str() != v.current || got_commands != v.commands {
            out.push(mismatch(
                "vector",
                &v.from,
                &v.event,
                assignment,
                format!("{} -> `{}` {:?}", show_outcome(&expected_outcome), v.current, v.commands),
                format!("{} -> `{}` {:?}", show_outcome(&got.outcome), got.current, got_commands),
            ));
        }
    }

    for a in &suite.alarms {
        let assignment = describe(&a.config, &a.facts);
        let world = match world_of(spec, &a.config, &a.facts) {
            Ok(w) => w,
            Err(e) => {
                out.push(mismatch("alarm", &a.state, "", assignment, "a readable assignment".to_owned(), e.to_string()));
                continue;
            }
        };
        let got = alarm_for(spec, &world, &StateId::new(&a.state));
        let expected = a.alarm.as_ref().map(|w| (w.fires.clone(), w.after, w.clock.clone()));
        let got_wire = got.as_ref().map(|al| (al.fires.to_string(), al.after, al.clock.map(|c| c.name().to_owned())));
        if expected != got_wire {
            out.push(mismatch("alarm", &a.state, "", assignment, format!("{:?}", expected), format!("{:?}", got_wire)));
        }
    }

    out
}

/// The world a vector describes: the machine's defaults, then the config it
/// names, then the facts it names.
pub fn world_of(spec: &Spec, config: &Map<String, Json>, facts: &Map<String, Json>) -> Result<World, DecodeError> {
    let mut world = World::from_spec(spec);
    for (k, v) in config {
        world.set_config(ConfigId::new(k), crate::codec::decode_value(v, &format!("config.{}", k))?);
    }
    for (k, v) in facts {
        world.set_fact(FactId::new(k), crate::codec::decode_value(v, &format!("facts.{}", k))?);
    }
    Ok(world)
}

fn describe(config: &Map<String, Json>, facts: &Map<String, Json>) -> String {
    let part = |name: &str, m: &Map<String, Json>| {
        if m.is_empty() {
            format!("no {}", name)
        } else {
            format!("{} {{{}}}", name, m.iter().map(|(k, v)| format!("{}: {}", k, v)).collect::<Vec<_>>().join(", "))
        }
    };
    format!("{}, {}", part("config", config), part("facts", facts))
}

fn show_outcome(o: &Outcome) -> String {
    match o {
        Outcome::Move(to) => format!("move `{}`", to),
        Outcome::Stay => "stay".to_owned(),
        Outcome::Refuse(tag) => format!("refuse `{}`", tag),
    }
}

fn first_difference(expected: &str, got: &str) -> (String, String) {
    let mut e = expected.lines();
    let mut g = got.lines();
    loop {
        match (e.next(), g.next()) {
            (Some(a), Some(b)) if a == b => continue,
            (a, b) => return (a.unwrap_or("<end>").to_owned(), b.unwrap_or("<end>").to_owned()),
        }
    }
}
