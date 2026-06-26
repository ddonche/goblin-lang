use goblin_yall::{yall_parse, yall_write};

fn round_trip(src: &str) -> String {
    let value = yall_parse(src, "writer.yall").unwrap();
    yall_write(&value)
}

#[test]
fn writes_nil_keyword() {
    let out = round_trip(r#"
parent: nil
"#);

    assert!(out.contains("parent: nil"));
    assert!(!out.contains("null"));
}

#[test]
fn round_trip_simple_map() {
    let src = r#"
name: Goblin
version: 1
enabled: true
"#;

    let out = round_trip(src);

    let reparsed = yall_parse(&out, "writer.yall").unwrap();
    let original = yall_parse(src, "writer.yall").unwrap();

    assert_eq!(reparsed, original);
}

#[test]
fn round_trip_nested_map() {
    let src = r#"
site:
  title: Goblin
  base_url: /
"#;

    let out = round_trip(src);

    let reparsed = yall_parse(&out, "writer.yall").unwrap();
    let original = yall_parse(src, "writer.yall").unwrap();

    assert_eq!(reparsed, original);
}

#[test]
fn round_trip_arrays() {
    let src = r#"
ports:
  - 80
  - 443
  - 8080
"#;

    let out = round_trip(src);

    let reparsed = yall_parse(&out, "writer.yall").unwrap();
    let original = yall_parse(src, "writer.yall").unwrap();

    assert_eq!(reparsed, original);
}

#[test]
fn round_trip_inline_map() {
    let src = r#"
author: { name: Dan, role: Creator }
"#;

    let out = round_trip(src);

    let reparsed = yall_parse(&out, "writer.yall").unwrap();
    let original = yall_parse(src, "writer.yall").unwrap();

    assert_eq!(reparsed, original);
}

#[test]
fn round_trip_inline_array() {
    let src = r#"
ports: [80, 443, 8080]
"#;

    let out = round_trip(src);

    let reparsed = yall_parse(&out, "writer.yall").unwrap();
    let original = yall_parse(src, "writer.yall").unwrap();

    assert_eq!(reparsed, original);
}

#[test]
fn writes_bare_goblin_strings() {
    let out = round_trip(r#"
portal: #site
need: #need::output_dir
action: trailboss::build_routes_once
"#);

    assert!(out.contains("portal: #site"));
    assert!(out.contains("need: #need::output_dir"));
    assert!(out.contains("action: trailboss::build_routes_once"));
}

#[test]
fn writes_quoted_strings_when_needed() {
    let out = round_trip(r#"
title: "Goblin Programming Language"
"#);

    let reparsed = yall_parse(&out, "writer.yall").unwrap();
    let original = yall_parse(
        r#"
title: "Goblin Programming Language"
"#,
        "writer.yall",
    )
    .unwrap();

    assert_eq!(reparsed, original);
}

#[test]
fn writer_never_emits_comments() {
    let out = round_trip(r#"
name: Goblin
"#);

    assert!(!out.contains("///"));
    assert!(!out.contains("////"));
    assert!(!out.contains("<----"));
}

#[test]
fn output_is_valid_yall() {
    let src = r#"
name: Goblin
version: 1
enabled: true
parent: nil

ports: [80, 443, 8080]

author:
  name: Dan
  role: Creator
"#;

    let out = round_trip(src);

    assert!(yall_parse(&out, "writer.yall").is_ok());
}