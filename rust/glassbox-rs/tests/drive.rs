//! `drive` — a machine running somewhere that is not a UI.
//!
//! The same machine and the same expectations as `Test.Unit.Drive` on the
//! PureScript side: one of everything the driver has a rule about, and a log
//! that IS the design read left to right.

use glassbox::drive::{enter, fire, start, MemoryHost};
use glassbox::run::World;
use glassbox::spec::{EventId, StateId};
use glassbox::{parse_spec, Spec};

fn machine() -> Spec {
    parse_spec(
        r#"{
          "glassbox": 2, "id": "drive", "initial": "idle", "clock": "milliseconds",
          "states": [
            { "id": "idle" },
            { "id": "working", "entry": ["begin"], "exit": ["cleanup"] },
            { "id": "done" }
          ],
          "events": [ { "id": "go" }, { "id": "finished" }, { "id": "poke" } ],
          "commands": [ { "id": "begin" }, { "id": "cleanup" } ],
          "refusals": [ { "id": "not-yet", "text": "nothing to do" } ],
          "rules": [
            { "from": "idle", "on": "go", "cases": [ { "to": "working" } ] },
            { "from": "idle", "on": "finished", "cases": [ { "stay": true } ] },
            { "from": "idle", "on": "poke", "cases": [ { "refuse": "not-yet" } ] },
            { "from": "working", "on": "finished", "cases": [ { "to": "done" } ] },
            { "from": "working", "on": "go", "cases": [ { "stay": true } ] },
            { "from": "working", "on": "poke", "cases": [ { "stay": true } ] },
            { "from": "done", "on": "go", "cases": [ { "to": "working" } ] },
            { "from": "done", "on": "finished", "cases": [ { "stay": true } ] },
            { "from": "done", "on": "poke", "cases": [ { "stay": true } ] }
          ],
          "deadlines": [ { "in": "working", "fires": "finished", "after": 250 } ]
        }"#,
    )
    .unwrap()
}

/// `begin` answers with an event, which is how the format says "and then
/// this happened" without a continuation. `cleanup` answers with nothing.
fn host(spec: &Spec, from: &str) -> MemoryHost {
    MemoryHost::new(StateId::new(from), World::from_spec(spec)).answering("begin", "finished")
}

fn ran(spec: &Spec, from: &str, event: &str) -> MemoryHost {
    let mut h = host(spec, from);
    fire(spec, &mut h, &EventId::new(event));
    h
}

#[test]
fn a_move_writes_the_phase_before_it_runs_anything() {
    let spec = machine();
    let h = ran(&spec, "idle", "go");
    // The phase moved, the alarm was set, the entry command ran, its
    // answering event came back in as an ordinary event, arriving somewhere
    // with no deadline cancelled the one that was running, and leaving
    // `working` ran its exit command last. Six things, one press, and not
    // one of them written by the host.
    assert_eq!(h.log, ["-> working", "arm 250", "begin", "-> done", "disarm", "cleanup"]);
    assert_eq!(h.phase, StateId::new("done"));
    assert_eq!(h.alarm, None);
}

#[test]
fn a_refusal_is_reported_and_commands_nothing() {
    let spec = machine();
    let h = ran(&spec, "idle", "poke");
    assert_eq!(h.log, ["refused not-yet"]);
    assert_eq!(h.phase, StateId::new("idle"));
}

#[test]
fn a_stay_does_nothing_at_all() {
    // A `stay` is a statechart INTERNAL transition. Re-arming here would make
    // every ignored keypress quietly extend a running timeout.
    let spec = machine();
    assert!(ran(&spec, "working", "poke").log.is_empty());
    assert!(ran(&spec, "idle", "sneeze").log.is_empty());
}

#[test]
fn entering_runs_the_entry_commands_and_arms() {
    let spec = machine();
    let mut h = host(&spec, "idle");
    enter(&spec, &mut h, &StateId::new("working"));
    assert_eq!(h.log, ["-> working", "arm 250", "begin", "-> done", "disarm", "cleanup"]);
}

#[test]
fn start_enters_the_initial_state_and_disarms() {
    let spec = machine();
    let mut h = host(&spec, "elsewhere");
    start(&spec, &mut h);
    assert_eq!(h.phase, StateId::new("idle"));
    assert_eq!(h.log, ["-> idle", "disarm"]);
}
