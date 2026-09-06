//! The wire format: one bidirectional codec, versioned.
//!
//! This is `Glassbox.Codec.JSON` again, and it is the boundary the whole
//! design turns on: a machine is data loaded at runtime, so this module is
//! the single place where untyped bytes become typed values — narrow,
//! explicit, versioned, checked rather than trusted, and loud on failure.
//!
//! ### Shape
//!
//! ```json
//! { "glassbox": 2
//! , "id": "car-radio"
//! , "title": "Car radio"
//! , "initial": "off"
//! , "states":  [ { "id": "off", "label": "Off" }
//!              , { "id": "cd", "label": "CD", "entry": ["spin-up"], "exit": ["park"] } ]
//! , "events":  [ { "id": "power", "label": "power", "source": "user" } ]
//! , "config":  [ { "id": "band-count", "label": "Bands", "default": 2 } ]
//! , "facts":   [ { "id": "disc-present", "label": "A disc is loaded" } ]
//! , "refusals":[ { "id": "no-disc", "text": "there is no disc to play" } ]
//! , "commands":[ { "id": "spin-up", "label": "Spin the disc up" } ]
//! , "rules":   [ { "from": "off", "on": "power", "cases": [ { "to": "radio" } ] } ]
//! , "deadlines": [ { "in": "seeking", "fires": "locked", "after": 3 } ]
//! }
//! ```
//!
//! A case is `{ "when": <guard>, "to" | "stay" | "refuse" }`, with `when`
//! omitted meaning the catch-all. A guard is one of `{"fact":…}`,
//! `{"config":…}`, a literal, `{"not":…}`, `{"all":[…]}`, `{"any":[…]}`, or
//! a comparison `{"op":"gt","left":…,"right":…}`.
//!
//! ### Why the decoder is written by hand
//!
//! The PureScript codec makes a series of small decisions that `serde`'s
//! derive would make differently: a `label` that is present but not a string
//! falls back to the id rather than failing; `final` that is not a boolean
//! reads as false; a case's `to` is only a `to` if it is a string, otherwise
//! `stay` and `refuse` are consulted in turn; the keys of a guard are tried
//! in a fixed order. Each is small, and each is a place where two runtimes
//! could accept different artifacts. So the decoder here walks a
//! `serde_json::Value` and makes the same decisions in the same order, and
//! every error names the path it happened at. The encoder uses `serde` in the
//! ordinary way, because writing is not where the two can disagree.
//!
//! ### The version field is computed, not copied
//!
//! `glassbox` is written as [`Spec::version_for`] and read back the same
//! way, so an artifact always declares the oldest version that can express
//! it. A file understating itself is corrected on read; a file from the
//! future is refused outright.

use crate::spec::*;
use serde::ser::SerializeStruct;
use serde::{Deserialize, Serialize, Serializer};
use serde_json::{Map, Value as Json};
use std::fmt;

// =============================================================================
// Errors
// =============================================================================

/// What went wrong, and where in the artifact.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DecodeError {
    pub path: String,
    pub message: String,
}

impl fmt::Display for DecodeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.path.is_empty() {
            f.write_str(&self.message)
        } else {
            write!(f, "at {}: {}", self.path, self.message)
        }
    }
}

impl std::error::Error for DecodeError {}

fn err<T>(path: &str, message: impl Into<String>) -> Result<T, DecodeError> {
    Err(DecodeError { path: path.to_owned(), message: message.into() })
}

// =============================================================================
// Entry points
// =============================================================================

/// Parse an artifact from text. The only function most callers need.
pub fn parse_spec(text: &str) -> Result<Spec, DecodeError> {
    let json: Json = serde_json::from_str(text)
        .map_err(|e| DecodeError { path: String::new(), message: format!("not JSON: {}", e) })?;
    decode_spec(&json)
}

/// The artifact as text, compact.
pub fn print_spec(spec: &Spec) -> String {
    serde_json::to_string(spec).expect("a Spec always serialises")
}

/// The artifact as the decoder understood it, re-encoded and indented.
///
/// Deliberately NOT the bytes that arrived. Showing the round trip is the
/// point: if this differs from the file on disk, the codec lost something,
/// and it can be seen rather than inferred from a passing test.
pub fn print_spec_pretty(spec: &Spec) -> String {
    serde_json::to_string_pretty(spec).expect("a Spec always serialises")
}

// =============================================================================
// Decoding
// =============================================================================

fn object<'a>(json: &'a Json, path: &str, what: &str) -> Result<&'a Map<String, Json>, DecodeError> {
    json.as_object().map_or_else(|| err(path, format!("expected {}", what)), Ok)
}

fn string_at<'a>(o: &'a Map<String, Json>, key: &str) -> Option<&'a str> {
    o.get(key).and_then(Json::as_str)
}

/// `boolAt`: absent or not a boolean reads as false, as the PureScript
/// decoder has it (and as its own test suite documents).
fn bool_at(o: &Map<String, Json>, key: &str) -> bool {
    o.get(key).and_then(Json::as_bool).unwrap_or(false)
}

fn required<'a>(o: &'a Map<String, Json>, key: &str, path: &str) -> Result<&'a Json, DecodeError> {
    o.get(key).map_or_else(|| err(&join(path, key), "missing"), Ok)
}

fn required_string(o: &Map<String, Json>, key: &str, path: &str) -> Result<String, DecodeError> {
    string_at(o, key).map(str::to_owned).map_or_else(|| err(&join(path, key), "expected a string"), Ok)
}

/// An absent list decodes as empty. A machine with no config, no facts or no
/// deadlines is entirely ordinary, and requiring an empty array for each
/// would make the smallest artifact noisy for no gain.
fn list_at<T>(
    o: &Map<String, Json>,
    key: &str,
    path: &str,
    each: impl Fn(&Json, &str) -> Result<T, DecodeError>,
) -> Result<Vec<T>, DecodeError> {
    let here = join(path, key);
    match o.get(key) {
        None => Ok(Vec::new()),
        Some(json) => {
            let items = json.as_array().map_or_else(|| err(&here, "expected an array"), Ok)?;
            items
                .iter()
                .enumerate()
                .map(|(i, item)| each(item, &format!("{}[{}]", here, i)))
                .collect()
        }
    }
}

fn join(path: &str, key: &str) -> String {
    if path.is_empty() {
        key.to_owned()
    } else {
        format!("{}.{}", path, key)
    }
}

/// A value is its bare JSON self — `4`, `true`, `"fm"`.
pub(crate) fn decode_value(json: &Json, path: &str) -> Result<Value, DecodeError> {
    match json {
        Json::Number(n) => n
            .as_f64()
            .map(Value::Number)
            .map_or_else(|| err(path, "a number this runtime cannot hold"), Ok),
        Json::Bool(b) => Ok(Value::Boolean(*b)),
        Json::String(s) => Ok(Value::String(s.clone())),
        _ => err(path, "expected a number, boolean or string"),
    }
}

/// `{"config": "count-in"}`, `{"fact": "converter-held"}`, or a bare literal.
fn decode_expr(json: &Json, path: &str) -> Result<Expr, DecodeError> {
    match json.as_object() {
        None => decode_value(json, path).map(Expr::Lit),
        Some(o) => {
            if let Some(k) = string_at(o, "config") {
                Ok(Expr::ConfigOf(ConfigId::new(k)))
            } else if let Some(k) = string_at(o, "fact") {
                Ok(Expr::FactOf(FactId::new(k)))
            } else {
                err(path, "expected an expression: a literal, {config}, or {fact}")
            }
        }
    }
}

fn decode_guard(json: &Json, path: &str) -> Result<Guard, DecodeError> {
    let Some(o) = json.as_object() else {
        return decode_expr(json, path).map(Guard::Holds);
    };
    if let Some(inner) = o.get("not") {
        return decode_guard(inner, &join(path, "not")).map(|g| Guard::Not(Box::new(g)));
    }
    if let Some(arr) = o.get("all") {
        return decode_guard_list(arr, &join(path, "all")).map(Guard::And);
    }
    if let Some(arr) = o.get("any") {
        return decode_guard_list(arr, &join(path, "any")).map(Guard::Or);
    }
    if let Some(op_json) = o.get("op") {
        let op_path = join(path, "op");
        let op = op_json
            .as_str()
            .and_then(CmpOp::from_name)
            .map_or_else(|| err(&op_path, "expected one of eq, ne, lt, le, gt, ge"), Ok)?;
        let l = decode_expr(required(o, "left", path)?, &join(path, "left"))?;
        let r = decode_expr(required(o, "right", path)?, &join(path, "right"))?;
        return Ok(Guard::Cmp(op, l, r));
    }
    decode_expr(json, path).map(Guard::Holds)
}

fn decode_guard_list(json: &Json, path: &str) -> Result<Vec<Guard>, DecodeError> {
    let items = json.as_array().map_or_else(|| err(path, "expected an array of guards"), Ok)?;
    items
        .iter()
        .enumerate()
        .map(|(i, g)| decode_guard(g, &format!("{}[{}]", path, i)))
        .collect()
}

/// `{"to": "armed"}`, `{"stay": true}` or `{"refuse": "converter-busy"}`,
/// optionally with a `when`. The keys are consulted in that order.
fn decode_case(json: &Json, path: &str) -> Result<Case, DecodeError> {
    let o = object(json, path, "a case object")?;
    let when = match o.get("when") {
        None => None,
        Some(g) => Some(decode_guard(g, &join(path, "when"))?),
    };
    let outcome = if let Some(sid) = string_at(o, "to") {
        Outcome::Move(StateId::new(sid))
    } else if o.contains_key("stay") {
        Outcome::Stay
    } else if let Some(rid) = string_at(o, "refuse") {
        Outcome::Refuse(RefusalId::new(rid))
    } else {
        return err(path, "a case needs exactly one of to, stay or refuse");
    };
    Ok(Case { when, outcome })
}

fn decode_rule(json: &Json, path: &str) -> Result<Rule, DecodeError> {
    let o = object(json, path, "a rule object")?;
    let from = StateId::new(required_string(o, "from", path)?);
    let on = EventId::new(required_string(o, "on", path)?);
    required(o, "cases", path)?;
    let cases = list_at(o, "cases", path, decode_case)?;
    Ok(Rule { from, on, cases })
}

fn decode_deadline(json: &Json, path: &str) -> Result<Deadline, DecodeError> {
    let o = object(json, path, "a deadline object")?;
    let in_state = StateId::new(required_string(o, "in", path)?);
    let fires = EventId::new(required_string(o, "fires", path)?);
    let after = decode_expr(required(o, "after", path)?, &join(path, "after"))?;
    Ok(Deadline { in_state, fires, after })
}

fn decode_command_id(json: &Json, path: &str) -> Result<CommandId, DecodeError> {
    json.as_str()
        .map(CommandId::new)
        .map_or_else(|| err(path, "expected a command id"), Ok)
}

fn decode_state(json: &Json, path: &str) -> Result<StateDecl, DecodeError> {
    let o = object(json, path, "a state object")?;
    let raw = required_string(o, "id", path)?;
    let label = string_at(o, "label").unwrap_or(&raw).to_owned();
    let entry = list_at(o, "entry", path, decode_command_id)?;
    let exit = list_at(o, "exit", path, decode_command_id)?;
    Ok(StateDecl { id: StateId::new(raw), label, is_final: bool_at(o, "final"), entry, exit })
}

fn decode_event(json: &Json, path: &str) -> Result<EventDecl, DecodeError> {
    let o = object(json, path, "an event object")?;
    let raw = required_string(o, "id", path)?;
    let label = string_at(o, "label").unwrap_or(&raw).to_owned();
    // Absent means user-driven: the common case, and the one an author
    // writing an artifact by hand should not have to say.
    let source = match o.get("source") {
        None => EventSource::FromUser,
        Some(s) => match s.as_str() {
            Some("user") => EventSource::FromUser,
            Some("runtime") => EventSource::FromRuntime,
            _ => return err(&join(path, "source"), "expected \"user\" or \"runtime\""),
        },
    };
    Ok(EventDecl { id: EventId::new(raw), label, source })
}

fn decode_config(json: &Json, path: &str) -> Result<ConfigDecl, DecodeError> {
    let o = object(json, path, "a config object")?;
    let raw = required_string(o, "id", path)?;
    let label = string_at(o, "label").unwrap_or(&raw).to_owned();
    let default = decode_value(required(o, "default", path)?, &join(path, "default"))?;
    Ok(ConfigDecl { id: ConfigId::new(raw), label, default })
}

fn decode_fact(json: &Json, path: &str) -> Result<FactDecl, DecodeError> {
    let o = object(json, path, "a fact object")?;
    let raw = required_string(o, "id", path)?;
    let label = string_at(o, "label").unwrap_or(&raw).to_owned();
    Ok(FactDecl { id: FactId::new(raw), label })
}

fn decode_refusal(json: &Json, path: &str) -> Result<RefusalDecl, DecodeError> {
    let o = object(json, path, "a refusal object")?;
    let raw = required_string(o, "id", path)?;
    let text = string_at(o, "text").unwrap_or(&raw).to_owned();
    Ok(RefusalDecl { id: RefusalId::new(raw), text })
}

fn decode_command(json: &Json, path: &str) -> Result<CommandDecl, DecodeError> {
    let o = object(json, path, "a command object")?;
    let raw = required_string(o, "id", path)?;
    let label = string_at(o, "label").unwrap_or(&raw).to_owned();
    Ok(CommandDecl { id: CommandId::new(raw), label })
}

/// A whole artifact, from JSON.
pub fn decode_spec(json: &Json) -> Result<Spec, DecodeError> {
    let o = object(json, "", "a Glassbox artifact object")?;
    let version_json = required(o, "glassbox", "")?;
    let version = integer(version_json).map_or_else(|| err("glassbox", "expected an integer"), Ok)?;
    // Refuse a future format outright rather than decode the parts we
    // recognise. Silently ignoring fields we do not understand is how one
    // implementation comes to run a different machine from another, which is
    // the failure this format exists to prevent.
    if version > FORMAT_VERSION {
        return err("glassbox", format!("format version {}; this build reads {}", version, FORMAT_VERSION));
    }
    let id = required_string(o, "id", "")?;
    let notes = list_at(o, "notes", "", |j, p| {
        j.as_str().map(str::to_owned).map_or_else(|| err(p, "expected a string"), Ok)
    })?;
    let initial = StateId::new(required_string(o, "initial", "")?);
    let states = list_at(o, "states", "", decode_state)?;
    let events = list_at(o, "events", "", decode_event)?;
    let config = list_at(o, "config", "", decode_config)?;
    let facts = list_at(o, "facts", "", decode_fact)?;
    let refusals = list_at(o, "refusals", "", decode_refusal)?;
    let commands = list_at(o, "commands", "", decode_command)?;
    let rules = list_at(o, "rules", "", decode_rule)?;
    let deadlines = list_at(o, "deadlines", "", decode_deadline)?;
    // A clock the format does not know is a hard failure rather than a
    // default, for the same reason a future version is: guessing produces a
    // machine that runs, and runs differently.
    let clock = match o.get("clock") {
        None => None,
        Some(c) => Some(
            c.as_str()
                .and_then(Clock::from_name)
                .map_or_else(|| err("clock", "expected milliseconds, seconds, minutes, beats or frames"), Ok)?,
        ),
    };
    let mut spec = Spec {
        version,
        id: id.clone(),
        title: string_at(o, "title").unwrap_or(&id).to_owned(),
        notes,
        initial,
        states,
        events,
        config,
        facts,
        refusals,
        commands,
        rules,
        deadlines,
        clock,
    };
    // Normalised upward, never downward. A file that says 1 and carries a
    // command was understating itself; a file that says 0 is read as the 1 it
    // actually is.
    spec.version = spec.version_for();
    Ok(spec)
}

/// An integer, whether the file wrote `2` or `2.0` — codec-argonaut's `int`
/// accepts both, so this does too.
fn integer(json: &Json) -> Option<i64> {
    if let Some(i) = json.as_i64() {
        return Some(i);
    }
    let f = json.as_f64()?;
    if f.fract() == 0.0 && f.abs() < 9007199254740992.0 {
        Some(f as i64)
    } else {
        None
    }
}

impl<'de> Deserialize<'de> for Spec {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let json = Json::deserialize(deserializer)?;
        decode_spec(&json).map_err(serde::de::Error::custom)
    }
}

// =============================================================================
// Encoding
// =============================================================================

macro_rules! serialize_as_str {
    ($($t:ty),*) => {
        $(impl Serialize for $t {
            fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
                s.serialize_str(&self.0)
            }
        })*
    };
}

serialize_as_str!(StateId, EventId, RefusalId, ConfigId, FactId, CommandId);

impl Serialize for Value {
    /// A number that is a whole number is written as one — `4`, not `4.0` —
    /// which is what `JSON.stringify` does and what keeps a re-encoded
    /// artifact looking like the one that was written.
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        match self {
            Value::Number(n) => {
                if n.fract() == 0.0 && n.abs() < 9007199254740992.0 {
                    s.serialize_i64(*n as i64)
                } else {
                    s.serialize_f64(*n)
                }
            }
            Value::Boolean(b) => s.serialize_bool(*b),
            Value::String(t) => s.serialize_str(t),
        }
    }
}

impl Serialize for Clock {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        s.serialize_str(self.name())
    }
}

impl Serialize for CmpOp {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        s.serialize_str(self.name())
    }
}

impl Serialize for EventSource {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        s.serialize_str(match self {
            EventSource::FromUser => "user",
            EventSource::FromRuntime => "runtime",
        })
    }
}

impl Serialize for Expr {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        match self {
            Expr::Lit(v) => v.serialize(s),
            Expr::ConfigOf(k) => {
                let mut o = s.serialize_struct("Expr", 1)?;
                o.serialize_field("config", k)?;
                o.end()
            }
            Expr::FactOf(k) => {
                let mut o = s.serialize_struct("Expr", 1)?;
                o.serialize_field("fact", k)?;
                o.end()
            }
        }
    }
}

impl Serialize for Guard {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        match self {
            Guard::Holds(e) => e.serialize(s),
            Guard::Not(g) => {
                let mut o = s.serialize_struct("Guard", 1)?;
                o.serialize_field("not", g.as_ref())?;
                o.end()
            }
            Guard::And(gs) => {
                let mut o = s.serialize_struct("Guard", 1)?;
                o.serialize_field("all", gs)?;
                o.end()
            }
            Guard::Or(gs) => {
                let mut o = s.serialize_struct("Guard", 1)?;
                o.serialize_field("any", gs)?;
                o.end()
            }
            Guard::Cmp(op, l, r) => {
                let mut o = s.serialize_struct("Guard", 3)?;
                o.serialize_field("op", op)?;
                o.serialize_field("left", l)?;
                o.serialize_field("right", r)?;
                o.end()
            }
        }
    }
}

impl Serialize for Case {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        let n = usize::from(self.when.is_some()) + 1;
        let mut o = s.serialize_struct("Case", n)?;
        if let Some(g) = &self.when {
            o.serialize_field("when", g)?;
        }
        match &self.outcome {
            Outcome::Move(sid) => o.serialize_field("to", sid)?,
            Outcome::Stay => o.serialize_field("stay", &true)?,
            Outcome::Refuse(rid) => o.serialize_field("refuse", rid)?,
        }
        o.end()
    }
}

impl Serialize for Rule {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        let mut o = s.serialize_struct("Rule", 3)?;
        o.serialize_field("from", &self.from)?;
        o.serialize_field("on", &self.on)?;
        o.serialize_field("cases", &self.cases)?;
        o.end()
    }
}

impl Serialize for Deadline {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        let mut o = s.serialize_struct("Deadline", 3)?;
        o.serialize_field("in", &self.in_state)?;
        o.serialize_field("fires", &self.fires)?;
        o.serialize_field("after", &self.after)?;
        o.end()
    }
}

impl Serialize for StateDecl {
    /// `final`, `entry` and `exit` are omitted when they say nothing, so a
    /// state that does nothing on the way in or out reads exactly as it did
    /// before commands existed.
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        let mut o = s.serialize_struct("StateDecl", 5)?;
        o.serialize_field("id", &self.id)?;
        o.serialize_field("label", &self.label)?;
        if self.is_final {
            o.serialize_field("final", &true)?;
        }
        if !self.entry.is_empty() {
            o.serialize_field("entry", &self.entry)?;
        }
        if !self.exit.is_empty() {
            o.serialize_field("exit", &self.exit)?;
        }
        o.end()
    }
}

impl Serialize for EventDecl {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        let mut o = s.serialize_struct("EventDecl", 3)?;
        o.serialize_field("id", &self.id)?;
        o.serialize_field("label", &self.label)?;
        o.serialize_field("source", &self.source)?;
        o.end()
    }
}

impl Serialize for ConfigDecl {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        let mut o = s.serialize_struct("ConfigDecl", 3)?;
        o.serialize_field("id", &self.id)?;
        o.serialize_field("label", &self.label)?;
        o.serialize_field("default", &self.default)?;
        o.end()
    }
}

impl Serialize for FactDecl {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        let mut o = s.serialize_struct("FactDecl", 2)?;
        o.serialize_field("id", &self.id)?;
        o.serialize_field("label", &self.label)?;
        o.end()
    }
}

impl Serialize for RefusalDecl {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        let mut o = s.serialize_struct("RefusalDecl", 2)?;
        o.serialize_field("id", &self.id)?;
        o.serialize_field("text", &self.text)?;
        o.end()
    }
}

impl Serialize for CommandDecl {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        let mut o = s.serialize_struct("CommandDecl", 2)?;
        o.serialize_field("id", &self.id)?;
        o.serialize_field("label", &self.label)?;
        o.end()
    }
}

impl Serialize for Spec {
    /// Field order is the PureScript encoder's, so the two runtimes write a
    /// machine the same way round. `notes` and `clock` are omitted when there
    /// is nothing to say.
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        let mut o = s.serialize_struct("Spec", 14)?;
        o.serialize_field("glassbox", &self.version_for())?;
        o.serialize_field("id", &self.id)?;
        o.serialize_field("title", &self.title)?;
        if !self.notes.is_empty() {
            o.serialize_field("notes", &self.notes)?;
        }
        o.serialize_field("initial", &self.initial)?;
        o.serialize_field("states", &self.states)?;
        o.serialize_field("events", &self.events)?;
        o.serialize_field("config", &self.config)?;
        o.serialize_field("facts", &self.facts)?;
        o.serialize_field("refusals", &self.refusals)?;
        o.serialize_field("commands", &self.commands)?;
        o.serialize_field("rules", &self.rules)?;
        o.serialize_field("deadlines", &self.deadlines)?;
        if let Some(c) = self.clock {
            o.serialize_field("clock", &c)?;
        }
        o.end()
    }
}
