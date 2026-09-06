//! Glassbox, in Rust.
//!
//! A state machine as a **data artifact** — authored as a file, run at
//! runtime, drawn from the same value it is running. This crate is the second
//! implementation of that format, and it makes no claim of its own: it is
//! held to the PureScript runtime in `core/` by the generated tables in
//! `conformance/vectors`, and `cargo test` is where that claim is checked.
//!
//! ```text
//! spec         the decoded machine — all data, no functions
//! codec        the wire format, versioned, loud on failure
//! run          the runtime: a pure function of (world, state, event)
//! drive        the driver: what a host does with that function
//! canonical    one machine, one string, the same on every runtime
//! conformance  replaying the PureScript runtime's table
//! ```
//!
//! What is deliberately not here: the lints, the printers, the drawing. A
//! host on this side loads an artifact the PureScript lints have already
//! accepted — the committed machines are linted in the PureScript test suite
//! and the vectors are generated from them — and a Rust host that wants to
//! accept arbitrary files should port `Glassbox.Lint` before it does.
//!
//! ### Running one
//!
//! ```no_run
//! use glassbox::{load, run::{World, step}, spec::{EventId, StateId}};
//!
//! let spec = load("core/machines/car-radio.json").unwrap();
//! let world = World::from_spec(&spec);
//! let s = step(&spec, &world, &StateId::new("off"), &EventId::new("power"));
//! assert_eq!(s.current, StateId::new("radio"));
//! assert_eq!(s.commands.len(), 1);   // tune-radio
//! ```
//!
//! For a host that keeps a phase, carries out commands and sets timers, see
//! [`drive::Host`] and [`drive::fire`].

pub mod canonical;
pub mod codec;
pub mod conformance;
pub mod drive;
pub mod run;
pub mod spec;

pub use codec::{parse_spec, print_spec, print_spec_pretty, DecodeError};
pub use run::{step, transition, World};
pub use spec::Spec;

use std::fmt;
use std::path::{Path, PathBuf};

/// What can go wrong reading an artifact from disk.
#[derive(Debug)]
pub enum LoadError {
    Io(PathBuf, std::io::Error),
    Decode(PathBuf, DecodeError),
}

impl fmt::Display for LoadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoadError::Io(p, e) => write!(f, "{}: {}", p.display(), e),
            LoadError::Decode(p, e) => write!(f, "{}: {}", p.display(), e),
        }
    }
}

impl std::error::Error for LoadError {}

/// An artifact from a file, through the real codec.
pub fn load(path: impl AsRef<Path>) -> Result<Spec, LoadError> {
    let path = path.as_ref();
    let text = std::fs::read_to_string(path).map_err(|e| LoadError::Io(path.to_owned(), e))?;
    parse_spec(&text).map_err(|e| LoadError::Decode(path.to_owned(), e))
}

/// Every `*.json` in a directory, decoded, sorted by file name — what
/// `glassbox-cli` calls `readMachines`. The name returned is the file's
/// basename without `.json`, which is what the vectors directory is keyed by.
pub fn load_dir(dir: impl AsRef<Path>) -> Result<Vec<(String, Spec)>, LoadError> {
    let dir = dir.as_ref();
    let entries = std::fs::read_dir(dir).map_err(|e| LoadError::Io(dir.to_owned(), e))?;
    let mut names: Vec<String> = entries
        .filter_map(|e| e.ok())
        .filter_map(|e| {
            let name = e.file_name().to_string_lossy().into_owned();
            name.strip_suffix(".json").map(str::to_owned)
        })
        .collect();
    names.sort();
    names
        .into_iter()
        .map(|name| load(dir.join(format!("{}.json", name))).map(|spec| (name, spec)))
        .collect()
}

/// A conformance suite from a file.
pub fn load_suite(path: impl AsRef<Path>) -> Result<conformance::Suite, LoadError> {
    let path = path.as_ref();
    let text = std::fs::read_to_string(path).map_err(|e| LoadError::Io(path.to_owned(), e))?;
    conformance::Suite::parse(&text).map_err(|e| LoadError::Decode(path.to_owned(), e))
}
