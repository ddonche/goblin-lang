use std::fs;
use std::path::{Path, PathBuf};

use goblin_yall::{yall_parse, yall_write};

fn roundtrip_suite_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../../yall-spec/tests/roundtrip")
}

#[test]
fn passes_yall_roundtrip_conformance_suite() {
    let suite_dir = roundtrip_suite_dir();

    assert!(
        suite_dir.is_dir(),
        "YALL round-trip suite not found at {}",
        suite_dir.display()
    );

    let mut cases = fs::read_dir(&suite_dir)
        .unwrap_or_else(|err| {
            panic!(
                "failed to read round-trip suite at {}: {err}",
                suite_dir.display()
            )
        })
        .collect::<Result<Vec<_>, _>>()
        .expect("failed to read round-trip test entries");

    cases.sort_by_key(|entry| entry.file_name());

    assert!(!cases.is_empty(), "round-trip conformance suite is empty");

    for case in cases {
        let case_path = case.path();

        if !case_path.is_dir() {
            continue;
        }

        let case_name = case.file_name().to_string_lossy().into_owned();
        let input_path = case_path.join("input.yall");
        let expected_path = case_path.join("expected.yall");

        let input = fs::read_to_string(&input_path)
            .unwrap_or_else(|err| panic!("{case_name}: failed to read input.yall: {err}"));

        let expected = fs::read_to_string(&expected_path)
            .unwrap_or_else(|err| panic!("{case_name}: failed to read expected.yall: {err}"));

        let parsed = yall_parse(&input, input_path.to_string_lossy().as_ref())
            .unwrap_or_else(|err| panic!("{case_name}: input failed to parse: {err}"));

        let actual = yall_write(&parsed);

        assert_eq!(
            normalize_newlines(&actual),
            normalize_newlines(&expected),
            "{case_name}: canonical writer output did not match expected.yall"
        );
    }
}

fn normalize_newlines(text: &str) -> String {
    text.replace("\r\n", "\n")
}