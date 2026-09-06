//! Running a decoded [`Spec`].
//!
//! The interpreter is a **pure function of (world, state, event)** and
//! nothing else. It holds no state of its own, so a host stores the current
//! [`StateId`] — a plain, comparable, serialisable value — rather than an
//! opaque machine object. That is what makes session restore, time travel
//! and pasting a reproducing trace into a bug report possible at all.
//!
//! It also answers **what the host must do** — [`commands_for`], and the
//! `commands` field of a [`Step`]. That is still a pure function of the same
//! three things; carrying out the commands is the host's business, and
//! nothing here performs an effect, allocates beyond the small `Vec` of
//! command ids it hands back, or blocks. A daemon may call it from any
//! thread that is not the audio callback.

use crate::spec::*;
use std::cmp::Ordering;
use std::collections::BTreeMap;

/// The world a guard may read: what the artifact declares, and what the host
/// reports.
///
/// Both arrive per step rather than being captured once, because both
/// genuinely change between events — a setting is turned, another loop takes
/// the converter — and because a captured world is a closure by another name.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct World {
    pub config: BTreeMap<ConfigId, Value>,
    pub facts: BTreeMap<FactId, Value>,
}

impl World {
    /// The world implied by a spec alone: every config at its declared
    /// default and no facts. What a host starts from before it knows anything.
    pub fn from_spec(spec: &Spec) -> World {
        World {
            config: spec.config.iter().map(|c| (c.id.clone(), c.default.clone())).collect(),
            facts: BTreeMap::new(),
        }
    }

    pub fn set_config(&mut self, k: ConfigId, v: Value) -> &mut Self {
        self.config.insert(k, v);
        self
    }

    pub fn set_fact(&mut self, k: FactId, v: Value) -> &mut Self {
        self.facts.insert(k, v);
        self
    }

    pub fn lookup_config(&self, k: &ConfigId) -> Option<&Value> {
        self.config.get(k)
    }

    pub fn lookup_fact(&self, k: &FactId) -> Option<&Value> {
        self.facts.get(k)
    }
}

// =============================================================================
// Evaluation
// =============================================================================

/// An expression that names something the world does not have evaluates to
/// `None` rather than to a default. A missing fact is not a false fact, and
/// quietly reading it as one is how a machine takes a branch nobody authored.
pub fn eval_expr<'a>(world: &'a World, expr: &'a Expr) -> Option<&'a Value> {
    match expr {
        Expr::Lit(v) => Some(v),
        Expr::ConfigOf(k) => world.lookup_config(k),
        Expr::FactOf(k) => world.lookup_fact(k),
    }
}

pub fn eval_guard(world: &World, guard: &Guard) -> bool {
    match guard {
        Guard::Holds(e) => eval_expr(world, e).map_or(false, Value::truthy),
        Guard::Not(g) => !eval_guard(world, g),
        Guard::And(gs) => gs.iter().all(|g| eval_guard(world, g)),
        Guard::Or(gs) => gs.iter().any(|g| eval_guard(world, g)),
        Guard::Cmp(op, l, r) => match (eval_expr(world, l), eval_expr(world, r)) {
            (Some(a), Some(b)) => compare_values(*op, a, b),
            _ => false,
        },
    }
}

/// Equality is structural; ordering requires both sides to be the same kind
/// of value. Ordering a number against a string is a mistake in the
/// artifact, and it reads as false rather than falling back on constructor
/// order, which would be a well-defined answer to a question nobody asked.
pub fn compare_values(op: CmpOp, a: &Value, b: &Value) -> bool {
    match op {
        CmpOp::Eq => a == b,
        CmpOp::Ne => a != b,
        _ => match ordering(a, b) {
            None => false,
            Some(ord) => match op {
                CmpOp::Lt => ord == Ordering::Less,
                CmpOp::Le => ord != Ordering::Greater,
                CmpOp::Gt => ord == Ordering::Greater,
                CmpOp::Ge => ord != Ordering::Less,
                CmpOp::Eq | CmpOp::Ne => false,
            },
        },
    }
}

/// The PureScript `Ord` instances, transcribed rather than assumed.
///
/// Numbers order the way `compare` on `Number` does — `<`, then `>`, else
/// `EQ`, so a NaN compares equal to everything rather than panicking or
/// poisoning. Strings order by **UTF-16 code unit**, because that is what
/// JavaScript's `<` does and therefore what the PureScript runtime does; a
/// byte-wise comparison of UTF-8 would disagree on any pair straddling the
/// surrogate range, which is exactly the kind of quiet divergence a second
/// runtime exists to expose. No machine in the corpus compares strings; this
/// is here so that the first one that does gets the same answer.
fn ordering(a: &Value, b: &Value) -> Option<Ordering> {
    match (a, b) {
        (Value::Number(x), Value::Number(y)) => Some(if x < y {
            Ordering::Less
        } else if x > y {
            Ordering::Greater
        } else {
            Ordering::Equal
        }),
        (Value::String(x), Value::String(y)) => Some(cmp_utf16(x, y)),
        (Value::Boolean(x), Value::Boolean(y)) => Some(x.cmp(y)),
        _ => None,
    }
}

/// Ordering by UTF-16 code units, as JavaScript compares strings.
pub fn cmp_utf16(a: &str, b: &str) -> Ordering {
    a.encode_utf16().cmp(b.encode_utf16())
}

// =============================================================================
// Stepping
// =============================================================================

/// What a state does with an event, under a given world.
///
/// Cases are tried in order and the first whose guard holds wins; a case with
/// no guard is the catch-all. A state and event with no rule at all yields
/// `Stay`, which is the same answer a flat machine gives for "not mine" — the
/// totality lint is what ensures a real artifact never relies on it.
pub fn resolve(spec: &Spec, world: &World, from: &StateId, on: &EventId) -> Outcome {
    match spec.rule_for(from, on) {
        None => Outcome::Stay,
        Some(rule) => rule
            .cases
            .iter()
            .find(|c| c.when.as_ref().map_or(true, |g| eval_guard(world, g)))
            .map_or(Outcome::Stay, |c| c.outcome.clone()),
    }
}

/// What the host is asked to do, given where the machine was and what the
/// outcome was.
///
/// Three clauses, and each of them is a decision rather than a consequence.
///
/// * **A refusal commands nothing.** A refused press that nonetheless does
///   something is precisely the bug the `Stay`/`Refuse` distinction was
///   introduced to make visible.
/// * **`Stay` commands nothing.** This is a statechart *internal* transition:
///   the machine did not leave, so nothing was exited and nothing entered.
/// * **`Move` exits then enters**, and it does so **even when the target is
///   the state it is already in**. That is the *external* self-transition,
///   and re-entering Recording genuinely restarts the recorder.
pub fn commands_for(spec: &Spec, from: &StateId, outcome: &Outcome) -> Vec<CommandId> {
    match outcome {
        Outcome::Refuse(_) | Outcome::Stay => Vec::new(),
        Outcome::Move(to) => {
            let exit = spec.exit_of(from);
            let entry = spec.entry_of(to);
            let mut out = Vec::with_capacity(exit.len() + entry.len());
            out.extend_from_slice(exit);
            out.extend_from_slice(entry);
            out
        }
    }
}

/// What the host must do before the machine has been fed anything at all.
///
/// The initial state is entered, so its entry commands are owed. A host that
/// skips this starts a machine whose Recording state is current and whose
/// recorder was never started.
pub fn start_commands(spec: &Spec) -> Vec<CommandId> {
    spec.entry_of(&spec.initial).to_vec()
}

/// Where it was, what happened, where it now is, and what the host owes.
///
/// `outcome` is carried rather than collapsed so that a refusal reaches the
/// display instead of vanishing into an unchanged state.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Step {
    pub from: StateId,
    pub event: EventId,
    pub outcome: Outcome,
    pub current: StateId,
    pub commands: Vec<CommandId>,
}

impl Step {
    /// The step as a `Result`: the state reached and the commands owed, or
    /// the refusal tag. `Stay` is an `Ok` that reaches where it started and
    /// owes nothing.
    pub fn into_result(self) -> Result<(StateId, Vec<CommandId>), RefusalId> {
        match self.outcome {
            Outcome::Refuse(tag) => Err(tag),
            _ => Ok((self.current, self.commands)),
        }
    }
}

/// One step. Pure, total, and the only thing a host needs to run a machine.
pub fn step(spec: &Spec, world: &World, from: &StateId, event: &EventId) -> Step {
    let outcome = resolve(spec, world, from, event);
    let current = outcome.target(from).clone();
    let commands = commands_for(spec, from, &outcome);
    Step { from: from.clone(), event: event.clone(), outcome, current, commands }
}

/// [`step`], shaped for a caller that wants the refusal as an error.
pub fn transition(
    spec: &Spec,
    world: &World,
    from: &StateId,
    event: &EventId,
) -> Result<(StateId, Vec<CommandId>), RefusalId> {
    step(spec, world, from, event).into_result()
}

/// The step a machine is notionally in before anything has been fed to it.
///
/// NOT `start_commands`: nothing has yet happened, and entering the initial
/// state is a separate event in the host's life.
pub fn initial_step(spec: &Spec, event: &EventId) -> Step {
    Step {
        from: spec.initial.clone(),
        event: event.clone(),
        outcome: Outcome::Stay,
        current: spec.initial.clone(),
        commands: Vec::new(),
    }
}

/// What a host must set a timer for on entering a state.
///
/// **Relative**, and carrying its own unit: a host is told *how long* and
/// *in what*, so the arithmetic that converts to its own clock happens once,
/// where the unit is still in scope.
///
/// `clock` is `None` only for an artifact that declares deadlines and no
/// clock, which the timing lint rejects.
#[derive(Clone, Debug, PartialEq)]
pub struct Alarm {
    pub fires: EventId,
    pub after: f64,
    pub clock: Option<Clock>,
}

/// The alarm a state asks for, or `None` if it resolves only on events.
pub fn alarm_for(spec: &Spec, world: &World, sid: &StateId) -> Option<Alarm> {
    let d = spec.deadline_for(sid)?;
    match eval_expr(world, &d.after) {
        Some(Value::Number(n)) => Some(Alarm { fires: d.fires.clone(), after: *n, clock: spec.clock }),
        _ => None,
    }
}

/// When the deadline for a state falls due, on a clock the caller is keeping.
///
/// Both numbers must already be in the machine's own clock; this function
/// cannot check that and does not try. Prefer [`alarm_for`].
pub fn due_at(spec: &Spec, world: &World, sid: &StateId, entered_at: f64) -> Option<(EventId, f64)> {
    let d = spec.deadline_for(sid)?;
    match eval_expr(world, &d.after) {
        Some(Value::Number(n)) => Some((d.fires.clone(), entered_at + n)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eq_across_kinds_is_false_not_an_error() {
        assert!(!compare_values(CmpOp::Eq, &Value::Number(1.0), &Value::String("1".into())));
        assert!(compare_values(CmpOp::Ne, &Value::Number(1.0), &Value::String("1".into())));
    }

    #[test]
    fn ordering_across_kinds_is_false() {
        assert!(!compare_values(CmpOp::Lt, &Value::Number(1.0), &Value::String("2".into())));
        assert!(!compare_values(CmpOp::Ge, &Value::Boolean(true), &Value::Number(0.0)));
    }

    #[test]
    fn strings_order_by_utf16_code_unit() {
        // U+FF5E (one BMP unit, 0xFF5E) versus U+1F600 (a surrogate pair
        // starting 0xD83D): JavaScript says the emoji sorts first.
        assert_eq!(cmp_utf16("\u{1F600}", "\u{FF5E}"), Ordering::Less);
        // Byte-wise UTF-8 would have said the opposite.
        assert_eq!("\u{1F600}".cmp("\u{FF5E}"), Ordering::Greater);
    }

    #[test]
    fn a_missing_fact_is_not_false() {
        let world = World::default();
        let g = Guard::Not(Box::new(Guard::Holds(Expr::FactOf(FactId::new("x")))));
        // `not (missing)` is true, because `missing` reads as false at the
        // Holds; that is the PureScript answer too, and the lint exists so a
        // real artifact never asks.
        assert!(eval_guard(&world, &g));
    }
}
