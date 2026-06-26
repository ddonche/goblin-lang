// crates/goblin-yall/tests/comments.rs

use goblin_yall::{yall_parse, YallValue};

fn parse(src: &str) -> YallValue {
    yall_parse(src, "comments.yall").unwrap()
}

fn parse_err(src: &str) -> String {
    yall_parse(src, "comments.yall")
        .unwrap_err()
        .to_string()
}

fn get<'a>(v: &'a YallValue, key: &str) -> &'a YallValue {
    v.as_map().unwrap().get(key).unwrap()
}

#[test]
fn parses_single_line_comments() {
    let v = parse(r#"
/// this is ignored
name: Goblin
/// this is also ignored
version: 1
"#);

    assert_eq!(get(&v, "name"), &YallValue::Str("Goblin".into()));
    assert_eq!(get(&v, "version"), &YallValue::Int(1));
}

#[test]
fn parses_inline_comments_after_values() {
    let v = parse(r#"
title: Goblin <---- homepage title
count: 42 <---- number of things
enabled: true <---- feature flag
"#);

    assert_eq!(get(&v, "title"), &YallValue::Str("Goblin".into()));
    assert_eq!(get(&v, "count"), &YallValue::Int(42));
    assert_eq!(get(&v, "enabled"), &YallValue::Bool(true));
}

#[test]
fn inline_comment_requires_whitespace_before_marker() {
    let v = parse(r#"
value: hello<----not a comment
"#);

    assert_eq!(
        get(&v, "value"),
        &YallValue::Str("hello<----not a comment".into())
    );
}

#[test]
fn parses_block_comments() {
    let v = parse(r#"
name: Goblin

////
ignored: true
also_ignored: 123
nested:
  thing: nope
////

version: 1
"#);

    assert_eq!(get(&v, "name"), &YallValue::Str("Goblin".into()));
    assert_eq!(get(&v, "version"), &YallValue::Int(1));
}

#[test]
fn hash_is_not_a_comment_marker() {
    let v = parse(r#"
portal: #site
need: #need::output_dir
"#);

    assert_eq!(get(&v, "portal"), &YallValue::Str("#site".into()));
    assert_eq!(get(&v, "need"), &YallValue::Str("#need::output_dir".into()));
}

#[test]
fn old_hash_comment_syntax_is_not_supported() {
    let v = parse(r#"
name: Goblin # old comment syntax is now part of value
"#);

    assert_eq!(
        get(&v, "name"),
        &YallValue::Str("Goblin # old comment syntax is now part of value".into())
    );
}

#[test]
fn unclosed_block_comment_is_error() {
    let err = parse_err(r#"
name: Goblin

////
this never closes
"#);

    assert!(
        err.contains("unterminated block comment") || err.contains("unclosed block comment"),
        "unexpected error: {err}"
    );
}