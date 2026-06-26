use goblin_yall::yall_parse;

fn parse_err(src: &str) -> String {
    yall_parse(src, "errors.yall")
        .unwrap_err()
        .to_string()
}

#[test]
fn errors_on_bad_indentation() {
    let err = parse_err(r#"
name: Goblin
    version: 1
"#);

    assert!(
        err.contains("unexpected indentation"),
        "unexpected error: {err}"
    );
}

#[test]
fn errors_on_unterminated_quoted_string() {
    let err = parse_err(r#"
title: "Goblin
"#);

    assert!(
        err.contains("unterminated string")
            || err.contains("unterminated quoted string"),
        "unexpected error: {err}"
    );
}

#[test]
fn errors_on_unterminated_inline_map() {
    let err = parse_err(r#"
author: { name: Dan, role: Creator
"#);

    assert!(
        err.contains("unterminated inline map"),
        "unexpected error: {err}"
    );
}

#[test]
fn errors_on_unterminated_inline_array() {
    let err = parse_err(r#"
ports: [80, 443, 8080
"#);

    assert!(
        err.contains("unterminated inline array"),
        "unexpected error: {err}"
    );
}

#[test]
fn errors_on_unterminated_block_comment() {
    let err = parse_err(r#"
name: Goblin

////
this never closes
"#);

    assert!(
        err.contains("unterminated block comment")
            || err.contains("unclosed block comment"),
        "unexpected error: {err}"
    );
}

#[test]
fn errors_on_unterminated_multiline_string() {
    let err = parse_err(r#"
description: """
this never closes
"#);

    assert!(
        err.contains("unterminated multiline string")
            || err.contains("unclosed multiline string"),
        "unexpected error: {err}"
    );
}

#[test]
fn error_message_includes_label_and_line() {
    let err = parse_err(r#"
name: Goblin
    version: 1
"#);

    assert!(err.contains("errors.yall"), "missing label: {err}");
    assert!(err.contains("line"), "missing line number: {err}");
}