//! Running a machine inside a program.
//!
//! `Glassbox.Drive` asks a host six questions and imposes `Monad m` and
//! nothing else: somewhere to keep a [`StateId`], somewhere to read the world
//! from, a way to carry out a command, a way to report a refusal, a way to
//! not block on a command, and a way to set a timer. Here the six questions
//! are the methods of [`Host`], and what is left is a trait bound and nothing
//! else — no async runtime, no channel, no thread. A host that keeps its
//! phase in an atomic, a database row or a struct field answers the same
//! questions.
//!
//! What is deliberately NOT here: anything that decides. [`fire`] chooses
//! nothing, guards nothing and has no opinion about optimistic updates. Every
//! decision is in the artifact, which is the claim the whole project rests
//! on.

use crate::run::{alarm_for, step, Alarm, World};
use crate::spec::{CommandId, EventId, Outcome, RefusalId, Spec, StateId};

/// What a host supplies to run a machine.
///
/// `phase` and `move_to` say where the current state is kept — in the
/// **host's** own storage as a plain, comparable, serialisable [`StateId`],
/// not inside anything here.
///
/// `perform` returns **an event, not a result**. The machine asks for
/// `send-follow` and stops; the host feeds `ok` or `failed` back in as an
/// ordinary event with ordinary arms. Completion is an event, not a
/// continuation — which is what keeps the failure arm in the enumeration
/// where a lint can see it, instead of in a callback where it can be
/// forgotten.
///
/// `refused` is required rather than optional, deliberately: a refusal that
/// reaches nobody is the failure the `Stay`/`Refuse` distinction exists to
/// expose.
///
/// `fork` is how a host says "do this, but let me return first". The
/// default runs the job inline, which is right for a host with nothing to
/// paint and for a test that wants a deterministic log; a host that wants
/// the phase written before the request goes out overrides it to queue the
/// job. The machine cannot tell the difference, and that is the point.
///
/// `arm` is called on **every** state change with the new state's alarm or
/// with `None`, so "leaving a state cancels its deadline" is a rule of the
/// driver rather than a thing each host has to remember. `None` means cancel
/// and set nothing; `Some` means cancel and set this.
pub trait Host {
    fn phase(&self) -> StateId;
    fn move_to(&mut self, to: &StateId);
    fn world(&self) -> World;
    fn perform(&mut self, cmd: &CommandId) -> Option<EventId>;
    fn refused(&mut self, tag: &RefusalId);
    fn arm(&mut self, alarm: Option<Alarm>);

    fn fork<'a>(&mut self, job: Box<dyn FnOnce(&mut Self) + 'a>)
    where
        Self: Sized,
    {
        job(self)
    }
}

/// Feed one event to the machine, move it, and carry out what it asks for.
///
/// The order is load-bearing. The phase is written **before** any command
/// runs, and each command goes through `fork`, so a host that paints has
/// painted by the time the request goes out.
///
/// A command that yields an event feeds it straight back in. That recursion
/// terminates because a machine that answers an event with a command has to
/// reach a state whose entry commands are empty, or it was written wrong — a
/// thing a lint can say and this function cannot.
pub fn fire<H: Host>(spec: &Spec, host: &mut H, event: &EventId) {
    let here = host.phase();
    let world = host.world();
    let record = step(spec, &world, &here, event);
    match &record.outcome {
        // Nothing moved, so nothing is entered, nothing exited, and a running
        // deadline keeps running. That is a statechart *internal* transition,
        // and re-arming here would silently make every ignored keypress
        // extend the timeout.
        Outcome::Stay => {}
        Outcome::Refuse(tag) => host.refused(tag),
        Outcome::Move(_) => {
            host.move_to(&record.current);
            host.arm(alarm_for(spec, &world, &record.current));
        }
    }
    run_all(spec, host, record.commands);
}

/// Put the machine into a state the host already knows it is in, and run
/// that state's entry commands.
///
/// This is the obligation `fire` cannot discharge. A host that seeds its
/// phase from outside the machine — from a snapshot, from a config file,
/// from a process it found already running — has performed a `Move` that no
/// event caused, and the entry commands of the state it lands in are owed
/// just the same. Writing the field directly skips them, silently, for ever.
pub fn enter<H: Host>(spec: &Spec, host: &mut H, phase: &StateId) {
    let world = host.world();
    host.move_to(phase);
    host.arm(alarm_for(spec, &world, phase));
    run_all(spec, host, spec.entry_of(phase).to_vec());
}

/// Enter the machine's declared initial state. What a host calls once, at
/// the point it has nothing better to seed from.
pub fn start<H: Host>(spec: &Spec, host: &mut H) {
    let initial = spec.initial.clone();
    enter(spec, host, &initial)
}

/// Carry out commands, off the caller's path, feeding any follow-up event
/// straight back in.
fn run_all<H: Host>(spec: &Spec, host: &mut H, cmds: Vec<CommandId>) {
    for cmd in cmds {
        host.fork(Box::new(move |h: &mut H| {
            if let Some(follow_up) = h.perform(&cmd) {
                fire(spec, h, &follow_up);
            }
        }));
    }
}

// =============================================================================
// A host in memory
// =============================================================================

/// Everything a host would keep, and a log of what it was asked to do.
///
/// The simplest possible host: the phase in a field, the world in a field, a
/// table of which commands answer with which event, and a log. Commands run
/// inline, so the log is an order rather than a race. It is what the tests
/// drive, and it is the shape a daemon's adapter would take before it grows
/// atomics.
#[derive(Clone, Debug, Default)]
pub struct MemoryHost {
    pub phase: StateId,
    pub world: World,
    /// Commands that answer with an event when carried out.
    pub answers: std::collections::BTreeMap<CommandId, EventId>,
    /// What happened, in order: `-> state`, `arm N` / `disarm`, a command
    /// id, or `refused tag`.
    pub log: Vec<String>,
    pub alarm: Option<Alarm>,
}

impl MemoryHost {
    pub fn new(phase: StateId, world: World) -> Self {
        MemoryHost { phase, world, ..Default::default() }
    }

    pub fn answering(mut self, cmd: &str, with: &str) -> Self {
        self.answers.insert(CommandId::new(cmd), EventId::new(with));
        self
    }
}

impl Host for MemoryHost {
    fn phase(&self) -> StateId {
        self.phase.clone()
    }

    fn move_to(&mut self, to: &StateId) {
        self.phase = to.clone();
        self.log.push(format!("-> {}", to));
    }

    fn world(&self) -> World {
        self.world.clone()
    }

    fn perform(&mut self, cmd: &CommandId) -> Option<EventId> {
        self.log.push(cmd.to_string());
        self.answers.get(cmd).cloned()
    }

    fn refused(&mut self, tag: &RefusalId) {
        self.log.push(format!("refused {}", tag));
    }

    fn arm(&mut self, alarm: Option<Alarm>) {
        self.log.push(match &alarm {
            None => "disarm".to_owned(),
            Some(a) => format!("arm {}", crate::canonical::js_number(a.after)),
        });
        self.alarm = alarm;
    }
}
