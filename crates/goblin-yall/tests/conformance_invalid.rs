use std::fs;
use std::path::{Path, PathBuf};

use goblin_yall::yall_parse;

fn invalid_suite_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../../yall-spec/tests/invalid")
}

#[test]
fn passes_yall_invalid_conformance_suite() {
    let suite_dir = invalid_suite_dir();

    assert!(
        suite_dir.is_dir(),
        "YALL invalid suite not found at {}",
        suite_dir.display()
    );

    let mut cases = fs::read_dir(&suite_dir)
        .unwrap_or_else(|err| {
            panic!(
                "failed to read invalid suite at {}: {err}",
                suite_dir.display()
            )
        })
        .collect::<Result<Vec<_>, _>>()
        .expect("failed to read invalid test entries");

    cases.sort_by_key(|entry| entry.file_name());

    assert!(!cases.is_empty(), "invalid conformance suite is empty");

    let mut failures = Vec::new();

    for case in cases {
        let case_path = case.path();

        if !case_path.is_dir() {
            continue;
        }

        let case_name = case.file_name().to_string_lossy().into_owned();
        let input_path = case_path.join("input.yall");
        let error_path = case_path.join("error.txt");

        let input = fs::read_to_string(&input_path)
            .unwrap_or_else(|err| panic!("{case_name}: failed to read input.yall: {err}"));

        let expected_error = fs::read_to_string(&error_path)
            .unwrap_or_else(|err| panic!("{case_name}: failed to read error.txt: {err}"))
            .trim()
            .to_lowercase();

        match yall_parse(&input, input_path.to_string_lossy().as_ref()) {
            Ok(value) => {
                failures.push(format!(
                    "{case_name}: expected parsing to fail, but got {value:?}"
                ));
            }

            Err(err) => {
                let actual_error = err.to_string().to_lowercase();

                if !error_class_matches(&actual_error, &expected_error) {
                    failures.push(format!(
                        "{case_name}: wrong error\n  expected class: {expected_error}\n  actual: {actual_error}"
                    ));
                }
            }
        }
    }

    assert!(
        failures.is_empty(),
        "invalid conformance failures:\n{}",
        failures.join("\n\n")
    );
}

fn error_class_matches(actual: &str, expected: &str) -> bool {
    let expected = expected.trim_end_matches('.');

    actual.contains(expected)
}