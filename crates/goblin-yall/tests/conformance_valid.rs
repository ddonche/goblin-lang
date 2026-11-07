use std::fs;
use std::path::{Path, PathBuf};

use goblin_yall::{yall_parse, YallValue};

fn valid_suite_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../../yall-spec/tests/valid")
}

#[test]
fn passes_yall_valid_conformance_suite() {
    let suite_dir = valid_suite_dir();

    assert!(
        suite_dir.is_dir(),
        "YALL valid suite not found at {}",
        suite_dir.display()
    );

    let mut cases = fs::read_dir(&suite_dir)
        .unwrap_or_else(|err| {
            panic!(
                "failed to read valid suite at {}: {err}",
                suite_dir.display()
            )
        })
        .collect::<Result<Vec<_>, _>>()
        .expect("failed to read valid test entries");

    cases.sort_by_key(|entry| entry.file_name());

    assert!(!cases.is_empty(), "valid conformance suite is empty");

    for case in cases {
        let case_path = case.path();

        if !case_path.is_dir() {
            continue;
        }

        let case_name = case.file_name().to_string_lossy().into_owned();
        let input_path = case_path.join("input.yall");
        let expected_path = case_path.join("expected.json");

        let input = fs::read_to_string(&input_path)
            .unwrap_or_else(|err| panic!("{case_name}: failed to read input.yall: {err}"));

        let expected_json = fs::read_to_string(&expected_path)
            .unwrap_or_else(|err| panic!("{case_name}: failed to read expected.json: {err}"));

        let parsed = yall_parse(&input, input_path.to_string_lossy().as_ref())
            .unwrap_or_else(|err| panic!("{case_name}: input failed to parse: {err}"));

        let actual_json = yall_to_json(&parsed);

        let expected: serde_json::Value = serde_json::from_str(&expected_json)
            .unwrap_or_else(|err| panic!("{case_name}: invalid expected.json: {err}"));

        assert_eq!(
            actual_json,
            expected,
            "{case_name}: parsed value did not match expected.json"
        );
    }
}

fn yall_to_json(value: &YallValue) -> serde_json::Value {
    match value {
        YallValue::Null => serde_json::Value::Null,
        YallValue::Bool(value) => serde_json::Value::Bool(*value),
        YallValue::Int(value) => serde_json::Value::Number((*value).into()),
        YallValue::Float(value) => serde_json::Number::from_f64(*value)
            .map(serde_json::Value::Number)
            .expect("YALL float was not representable as JSON"),
        YallValue::Str(value) => serde_json::Value::String(value.clone()),
        YallValue::Array(values) => {
            serde_json::Value::Array(values.iter().map(yall_to_json).collect())
        }
        YallValue::Map(values) => {
            let object = values
                .iter()
                .map(|(key, value)| (key.clone(), yall_to_json(value)))
                .collect();

            serde_json::Value::Object(object)
        }
    }
}