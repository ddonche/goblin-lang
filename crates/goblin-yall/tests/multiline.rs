// crates/goblin-yall/tests/multiline.rs

use goblin_yall::{yall_parse, YallValue};

fn parse(src: &str) -> YallValue {
    yall_parse(src, "multiline.yall").unwrap()
}

fn parse_err(src: &str) -> String {
    yall_parse(src, "multiline.yall")
        .unwrap_err()
        .to_string()
}

fn get<'a>(v: &'a YallValue, key: &str) -> &'a YallValue {
    v.as_map().unwrap().get(key).unwrap()
}

#[test]
fn parses_basic_multiline_string() {
    let v = parse(r#"
description: """
Line one
Line two
Line three
"""
"#);

    assert_eq!(
        get(&v, "description"),
        &YallValue::Str("Line one\nLine two\nLine three".into())
    );
}

#[test]
fn preserves_blank_lines() {
    let v = parse(r#"
text: """
First paragraph.

Second paragraph.
"""
"#);

    assert_eq!(
        get(&v, "text"),
        &YallValue::Str("First paragraph.\n\nSecond paragraph.".into())
    );
}

#[test]
fn comment_markers_are_literal_inside_multiline_strings() {
    let v = parse(r#"
text: """
/// not a comment
//// not a block comment
<---- not an inline comment
#site
#need::output_dir
"""
"#);

    assert_eq!(
        get(&v, "text"),
        &YallValue::Str(
            "/// not a comment\n//// not a block comment\n<---- not an inline comment\n#site\n#need::output_dir".into()
        )
    );
}

#[test]
fn multiline_string_may_contain_quotes() {
    let v = parse(r#"
text: """
She said "Hello."
He replied "Welcome."
"""
"#);

    assert_eq!(
        get(&v, "text"),
        &YallValue::Str(
            "She said \"Hello.\"\nHe replied \"Welcome.\"".into()
        )
    );
}

#[test]
fn multiline_string_followed_by_normal_values() {
    let v = parse(r#"
description: """
One
Two
"""

version: 1
enabled: true
"#);

    assert_eq!(
        get(&v, "description"),
        &YallValue::Str("One\nTwo".into())
    );

    assert_eq!(get(&v, "version"), &YallValue::Int(1));
    assert_eq!(get(&v, "enabled"), &YallValue::Bool(true));
}

#[test]
fn unterminated_multiline_string_is_error() {
    let err = parse_err(r#"
text: """
This never ends...
"#);

    assert!(
        err.contains("unterminated multiline string")
            || err.contains("unclosed multiline string"),
        "unexpected error: {err}"
    );
}