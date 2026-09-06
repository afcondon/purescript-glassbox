//! The decoded machine — all data, no functions.
//!
//! This is `Glassbox.Spec` in Rust, and it is deliberately a transcription
//! rather than a redesign: the point of a second runtime is that it reads the
//! same artifact and answers the same questions, so every type here has a
//! counterpart in `core/src/Glassbox/Spec.purs` with the same name and the
//! same fields, and the doc comments say what the PureScript ones say.
//!
//! ### The five vocabularies
//!
//! **States and events** are opaque identifiers owned by the machine's author.
//! No runtime interprets them; it moves between them and reports where it is.
//! That is why they are `String` newtypes and not enums — an enum would mean
//! the set was fixed by whoever compiled the host, which is exactly the
//! property being given up. **Config and facts** are the two halves of the
//! world a guard may read: config is declared by the artifact and reshapes the
//! machine, facts are supplied by the host at each step. **Refusals** are a
//! closed list the artifact declares, so a refusal carries a tag rather than
//! prose. **Commands** are a closed list too, hanging off state entry and exit
//! rather than off edges. The fifth is the **clock**, and it is closed the
//! other way round — named by the format, not the author — because a host
//! cannot arm a timer in a unit it has never heard of.
//!
//! ### What a runtime must and must not do with a `Spec`
//!
//! Nothing here is a function, and nothing here is interpreted beyond what
//! [`crate::run`] does with it. A `Spec` can be serialised, hashed
//! ([`crate::canonical`]), diffed and replayed. A host that finds itself
//! matching on the *text* of a state id has left the design.

use std::fmt;

/// The newest wire format version this crate understands.
///
/// Present because a second implementation — this one — reads these
/// artifacts, and a format with two implementations and no version is a
/// format that cannot be changed. It earned its keep at 2, which added
/// commands.
pub const FORMAT_VERSION: i64 = 2;

macro_rules! id_newtype {
    ($(#[$doc:meta])* $name:ident) => {
        $(#[$doc])*
        #[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
        pub struct $name(pub String);

        impl $name {
            pub fn new(raw: impl Into<String>) -> Self {
                $name(raw.into())
            }
            pub fn as_str(&self) -> &str {
                &self.0
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str(&self.0)
            }
        }

        impl From<&str> for $name {
            fn from(raw: &str) -> Self {
                $name(raw.to_owned())
            }
        }

        impl From<String> for $name {
            fn from(raw: String) -> Self {
                $name(raw)
            }
        }
    };
}

id_newtype!(
    /// A state, named by the machine's author and interpreted by nobody.
    StateId
);
id_newtype!(
    /// An event, named by the machine's author and interpreted by nobody.
    EventId
);
id_newtype!(
    /// A refusal tag, from the closed list the artifact declares.
    RefusalId
);
id_newtype!(
    /// A config setting, declared by the artifact with a default.
    ConfigId
);
id_newtype!(
    /// A fact the host reports about the world.
    FactId
);
id_newtype!(
    /// Something the artifact asks the host to do.
    ///
    /// Opaque here, exactly as a state is: the format knows that
    /// `start-recorder` must be carried out on entering Recording, and knows
    /// nothing whatever about recorders. Every meaning lives on the host side
    /// of the boundary, which is what lets a browser, a daemon and a
    /// microcontroller run the same machine and each do something different
    /// with it.
    CommandId
);

// =============================================================================
// Values, expressions and guards
// =============================================================================

/// What a config setting or a fact can hold.
///
/// `PartialEq` and not `Eq`, because a number is a float and the PureScript
/// side compares numbers as floats too: equality is structural and IEEE.
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(String),
}

impl Value {
    /// Whether a value counts as true where a guard expects a condition.
    ///
    /// Only a boolean is true or false. A number or a string used where a
    /// condition belongs is a mistake in the artifact rather than a value to
    /// coerce, and it reads as false so that the vocabulary lint has something
    /// to catch rather than the machine silently taking a branch.
    pub fn truthy(&self) -> bool {
        match self {
            Value::Boolean(b) => *b,
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", crate::canonical::js_number(*n)),
            Value::Boolean(b) => f.write_str(if *b { "true" } else { "false" }),
            Value::String(s) => f.write_str(s),
        }
    }
}

/// Something a guard can evaluate to a value.
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Lit(Value),
    ConfigOf(ConfigId),
    FactOf(FactId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum CmpOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl CmpOp {
    pub fn name(self) -> &'static str {
        match self {
            CmpOp::Eq => "eq",
            CmpOp::Ne => "ne",
            CmpOp::Lt => "lt",
            CmpOp::Le => "le",
            CmpOp::Gt => "gt",
            CmpOp::Ge => "ge",
        }
    }

    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "eq" => Some(CmpOp::Eq),
            "ne" => Some(CmpOp::Ne),
            "lt" => Some(CmpOp::Lt),
            "le" => Some(CmpOp::Le),
            "gt" => Some(CmpOp::Gt),
            "ge" => Some(CmpOp::Ge),
            _ => None,
        }
    }
}

/// A condition on the world, as data.
#[derive(Clone, Debug, PartialEq)]
pub enum Guard {
    Holds(Expr),
    Not(Box<Guard>),
    And(Vec<Guard>),
    Or(Vec<Guard>),
    Cmp(CmpOp, Expr, Expr),
}

// =============================================================================
// Outcomes and rules
// =============================================================================

/// What happened when an event met a state.
///
/// `Stay` is deliberately distinct from `Refuse`: doing nothing because the
/// event is meaningless here is not the same as doing nothing because
/// something else in the system forbade it, and a press that leaves no trace
/// is the failure this distinction exists to prevent.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Outcome {
    Move(StateId),
    Stay,
    Refuse(RefusalId),
}

impl Outcome {
    /// Where the machine ends up, given where it was and what happened.
    pub fn target<'a>(&'a self, current: &'a StateId) -> &'a StateId {
        match self {
            Outcome::Move(next) => next,
            Outcome::Stay | Outcome::Refuse(_) => current,
        }
    }

    pub fn is_refusal(&self) -> bool {
        matches!(self, Outcome::Refuse(_))
    }
}

/// One branch of a rule.
///
/// A `when` of `None` is the catch-all. Cases are tried in order and the
/// first match wins, which makes the machine deterministic by construction
/// rather than by a check — at the cost that an artifact can hide an
/// unreachable case behind an earlier one. That is what the overlap lint is
/// for; it is not a soundness problem.
#[derive(Clone, Debug, PartialEq)]
pub struct Case {
    pub when: Option<Guard>,
    pub outcome: Outcome,
}

/// What one state does with one event.
#[derive(Clone, Debug, PartialEq)]
pub struct Rule {
    pub from: StateId,
    pub on: EventId,
    pub cases: Vec<Case>,
}

/// A transition that fires with no event from anybody.
///
/// Names the **event that will be delivered**, not the state that will be
/// reached, so that `rules` stays the single authority on where anything
/// goes. `after` is a bare quantity in the machine's own clock.
#[derive(Clone, Debug, PartialEq)]
pub struct Deadline {
    pub in_state: StateId,
    pub fires: EventId,
    pub after: Expr,
}

/// The unit a machine's deadlines are counted in.
///
/// A **closed** vocabulary rather than an opaque string like a command,
/// because a host cannot arm a timer in a unit it has never heard of.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Clock {
    Milliseconds,
    Seconds,
    Minutes,
    Beats,
    Frames,
}

/// Every clock, so a host can be exhaustive over the list.
pub const EVERY_CLOCK: [Clock; 5] = [
    Clock::Milliseconds,
    Clock::Seconds,
    Clock::Minutes,
    Clock::Beats,
    Clock::Frames,
];

impl Clock {
    pub fn name(self) -> &'static str {
        match self {
            Clock::Milliseconds => "milliseconds",
            Clock::Seconds => "seconds",
            Clock::Minutes => "minutes",
            Clock::Beats => "beats",
            Clock::Frames => "frames",
        }
    }

    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "milliseconds" => Some(Clock::Milliseconds),
            "seconds" => Some(Clock::Seconds),
            "minutes" => Some(Clock::Minutes),
            "beats" => Some(Clock::Beats),
            "frames" => Some(Clock::Frames),
            _ => None,
        }
    }

    /// How many milliseconds one unit of this clock is — when that is a
    /// fixed quantity at all.
    ///
    /// `None` is the interesting answer and it is not a gap: a beat is not a
    /// duration until somebody says what the tempo is, and a frame is not one
    /// until somebody says what the rate is. Those rates are not the machine's
    /// to know, so a host that owns only wall-clock time can accept the first
    /// three by construction and refuse the rest at load, by name.
    pub fn milliseconds_per(self) -> Option<f64> {
        match self {
            Clock::Milliseconds => Some(1.0),
            Clock::Seconds => Some(1000.0),
            Clock::Minutes => Some(60_000.0),
            Clock::Beats | Clock::Frames => None,
        }
    }
}

// =============================================================================
// Declarations
// =============================================================================

/// A state, with the commands the host must carry out on the way in and on
/// the way out.
///
/// They live on the **state**, not on the edges, and that is the load-bearing
/// choice: it is what makes "entering Recording starts the recorder" a fact
/// about Recording rather than a duty each of its inbound edges has to
/// remember. Order within each list is the order they are carried out in.
#[derive(Clone, Debug, PartialEq)]
pub struct StateDecl {
    pub id: StateId,
    pub label: String,
    pub is_final: bool,
    pub entry: Vec<CommandId>,
    pub exit: Vec<CommandId>,
}

/// Who delivers an event.
///
/// A surface that draws a button for every event would offer the reader a
/// button for the count-in elapsing, which misrepresents the machine. The
/// distinction is in the artifact rather than inferred, because only the
/// author knows it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EventSource {
    FromUser,
    FromRuntime,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EventDecl {
    pub id: EventId,
    pub label: String,
    pub source: EventSource,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ConfigDecl {
    pub id: ConfigId,
    pub label: String,
    pub default: Value,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FactDecl {
    pub id: FactId,
    pub label: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RefusalDecl {
    pub id: RefusalId,
    pub text: String,
}

/// A command the host must know how to carry out.
///
/// Declared, and not merely mentioned on a state, because the list is the
/// **contract with the host**: it can be read at load time so a host says up
/// front which commands it cannot honour.
#[derive(Clone, Debug, PartialEq)]
pub struct CommandDecl {
    pub id: CommandId,
    pub label: String,
}

/// A whole machine, as decoded from an artifact.
///
/// `notes` is prose the author wrote about the machine and is **part of the
/// value**: an artifact that cannot carry the reasoning behind it loses that
/// reasoning the first time anything saves the file.
#[derive(Clone, Debug, PartialEq)]
pub struct Spec {
    pub version: i64,
    pub id: String,
    pub title: String,
    pub notes: Vec<String>,
    pub initial: StateId,
    pub states: Vec<StateDecl>,
    pub events: Vec<EventDecl>,
    pub config: Vec<ConfigDecl>,
    pub facts: Vec<FactDecl>,
    pub refusals: Vec<RefusalDecl>,
    pub commands: Vec<CommandDecl>,
    pub rules: Vec<Rule>,
    pub deadlines: Vec<Deadline>,
    /// Absent is legal only for a machine with no deadlines.
    pub clock: Option<Clock>,
}

impl Spec {
    /// The **oldest** version that can express this machine.
    ///
    /// Version is a fact about the artifact rather than a preference of
    /// whoever wrote it, so it is computed and not asked for. A machine with
    /// no commands is still a version 1 machine and is written as one.
    pub fn required_version(&self) -> i64 {
        let has_commands = self
            .states
            .iter()
            .any(|s| !s.entry.is_empty() || !s.exit.is_empty());
        if self.commands.is_empty() && !has_commands {
            1
        } else {
            2
        }
    }

    /// The version to stamp on an artifact: never below what the file
    /// claimed, never below what it uses.
    pub fn version_for(&self) -> i64 {
        self.version.max(self.required_version())
    }

    pub fn state_ids(&self) -> impl Iterator<Item = &StateId> {
        self.states.iter().map(|s| &s.id)
    }

    pub fn event_ids(&self) -> impl Iterator<Item = &EventId> {
        self.events.iter().map(|e| &e.id)
    }

    pub fn command_ids(&self) -> impl Iterator<Item = &CommandId> {
        self.commands.iter().map(|c| &c.id)
    }

    /// Events a person can cause, which is what a surface should offer
    /// buttons for.
    pub fn user_events(&self) -> impl Iterator<Item = &EventId> {
        self.events
            .iter()
            .filter(|e| e.source == EventSource::FromUser)
            .map(|e| &e.id)
    }

    pub fn state(&self, sid: &StateId) -> Option<&StateDecl> {
        self.states.iter().find(|s| &s.id == sid)
    }

    pub fn label_of_state<'a>(&'a self, sid: &'a StateId) -> &'a str {
        self.state(sid).map(|s| s.label.as_str()).unwrap_or(sid.as_str())
    }

    pub fn label_of_event<'a>(&'a self, eid: &'a EventId) -> &'a str {
        self.events
            .iter()
            .find(|e| &e.id == eid)
            .map(|e| e.label.as_str())
            .unwrap_or(eid.as_str())
    }

    pub fn label_of_command<'a>(&'a self, cid: &'a CommandId) -> &'a str {
        self.commands
            .iter()
            .find(|c| &c.id == cid)
            .map(|c| c.label.as_str())
            .unwrap_or(cid.as_str())
    }

    pub fn text_of_refusal<'a>(&'a self, rid: &'a RefusalId) -> &'a str {
        self.refusals
            .iter()
            .find(|r| &r.id == rid)
            .map(|r| r.text.as_str())
            .unwrap_or(rid.as_str())
    }

    /// What the host must do on the way into a state.
    ///
    /// A state nobody declared has no commands, rather than being an error:
    /// the runtime is total everywhere else and there is no reason for this
    /// to be the one place it is not. The closure lint is what objects to the
    /// rule that named it.
    pub fn entry_of(&self, sid: &StateId) -> &[CommandId] {
        self.state(sid).map(|s| s.entry.as_slice()).unwrap_or(&[])
    }

    pub fn exit_of(&self, sid: &StateId) -> &[CommandId] {
        self.state(sid).map(|s| s.exit.as_slice()).unwrap_or(&[])
    }

    /// The rule for a pair: the **first** one written, as `ruleFor` is `find`.
    pub fn rule_for(&self, from: &StateId, on: &EventId) -> Option<&Rule> {
        self.rules.iter().find(|r| &r.from == from && &r.on == on)
    }

    /// The deadline for a state: the **first** one written. The timing lint
    /// rejects a state with two, because the runtime would run this one and
    /// the printer would print both.
    pub fn deadline_for(&self, sid: &StateId) -> Option<&Deadline> {
        self.deadlines.iter().find(|d| &d.in_state == sid)
    }
}
