//! The claim, checked.
//!
//! Every machine in `core/machines` is loaded through the codec and replayed
//! against the table the PureScript runtime wrote for it in
//! `conformance/vectors`. A machine without a table is a failure, not a skip:
//! a machine nobody measured is a machine nobody can call conformant.

use glassbox::conformance::replay;
use glassbox::{load_dir, load_suite, parse_spec, print_spec, print_spec_pretty};
use std::path::PathBuf;

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..").join("..")
}

#[test]
fn every_machine_replays_identically() {
    let machines = load_dir(repo_root().join("core/machines")).expect("the corpus loads");
    assert!(!machines.is_empty(), "no machines found");

    let mut failures = Vec::new();
    let mut vectors = 0usize;
    let mut alarms = 0usize;
    let mut summary = Vec::new();

    for (name, spec) in &machines {
        let path = repo_root().join("conformance/vectors").join(format!("{}.json", name));
        let suite = match load_suite(&path) {
            Ok(s) => s,
            Err(e) => {
                failures.push(format!("{}: no conformance suite — run `make vectors` ({})", name, e));
                continue;
            }
        };
        vectors += suite.vectors.len();
        alarms += suite.alarms.len();
        let mismatches = replay(spec, &suite);
        summary.push(format!(
            "  {:<18} {:>4} vectors {:>2} alarms  {}",
            name,
            suite.vectors.len(),
            suite.alarms.len(),
            if mismatches.is_empty() { "ok" } else { "MISMATCH" }
        ));
        for m in mismatches {
            failures.push(m.to_string());
        }
    }

    println!("\nconformance: {} machines, {} vectors, {} alarms", machines.len(), vectors, alarms);
    for line in &summary {
        println!("{}", line);
    }

    assert!(
        failures.is_empty(),
        "\n{} disagreement(s) with the PureScript runtime:\n\n{}\n",
        failures.len(),
        failures.join("\n\n")
    );
}

#[test]
fn every_suite_names_a_machine_that_exists() {
    let machines = load_dir(repo_root().join("core/machines")).expect("the corpus loads");
    let dir = repo_root().join("conformance/vectors");
    for entry in std::fs::read_dir(&dir).expect("the vectors directory exists") {
        let name = entry.unwrap().file_name().to_string_lossy().into_owned();
        let Some(stem) = name.strip_suffix(".json") else { continue };
        assert!(
            machines.iter().any(|(n, _)| n == stem),
            "conformance/vectors/{} has no machine in core/machines — stale; run `make vectors`",
            name
        );
    }
}

/// Encoding and decoding must be inverses, and the encoded text must decode
/// to the same value — through both the compact and the indented printer.
#[test]
fn every_machine_round_trips_through_the_codec() {
    let machines = load_dir(repo_root().join("core/machines")).expect("the corpus loads");
    for (name, spec) in &machines {
        let back = parse_spec(&print_spec(spec)).unwrap_or_else(|e| panic!("{}: {}", name, e));
        assert_eq!(&back, spec, "{}: compact round trip changed the machine", name);
        let back = parse_spec(&print_spec_pretty(spec)).unwrap_or_else(|e| panic!("{}: {}", name, e));
        assert_eq!(&back, spec, "{}: pretty round trip changed the machine", name);
    }
}

#[test]
fn the_boundary_is_checked_not_trusted() {
    assert!(parse_spec(r#"{"glassbox":99,"id":"x","initial":"a","states":[{"id":"a"}]}"#).is_err());
    assert!(parse_spec(r#"{"glassbox":1,"id":"x","initial":"a","states":[{"id":"a"}],"rules":[{"from":"a","on":"e","cases":[{}]}]}"#).is_err());
    assert!(parse_spec("not json at all").is_err());
    // A file that understates its version is corrected on read, not refused.
    let spec = parse_spec(r#"{"glassbox":1,"id":"x","initial":"a","states":[{"id":"a","entry":["go"]}],"commands":[{"id":"go"}]}"#).unwrap();
    assert_eq!(spec.version, 2);
    // And the error names where it happened.
    let err = parse_spec(r#"{"glassbox":2,"id":"x","initial":"a","states":[{"id":"a"},{"label":"b"}]}"#).unwrap_err();
    assert_eq!(err.path, "states[1].id");
}
