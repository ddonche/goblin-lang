// crates/yall/tests/parser.rs

use goblin_yall::{yall_parse, YallValue};

fn parse(src: &str) -> YallValue {
    yall_parse(src, "parser.yall").unwrap()
}

fn get<'a>(v: &'a YallValue, key: &str) -> &'a YallValue {
    v.as_map().unwrap().get(key).unwrap()
}

#[test]
fn parses_top_level_map() {
    let v = parse(r#"
name: Goblin
version: 1
"#);

    assert_eq!(get(&v, "name"), &YallValue::Str("Goblin".into()));
    assert_eq!(get(&v, "version"), &YallValue::Int(1));
}

#[test]
fn parses_nested_map() {
    let v = parse(r#"
site:
  title: Goblin
  base_url: /
"#);

    let site = get(&v, "site").as_map().unwrap();

    assert_eq!(site.get("title").unwrap(), &YallValue::Str("Goblin".into()));
    assert_eq!(site.get("base_url").unwrap(), &YallValue::Str("/".into()));
}

#[test]
fn parses_block_array() {
    let v = parse(r#"
routes:
  - /
  - /docs
  - /blog
"#);

    assert_eq!(
        get(&v, "routes"),
        &YallValue::Array(vec![
            YallValue::Str("/".into()),
            YallValue::Str("/docs".into()),
            YallValue::Str("/blog".into()),
        ])
    );
}

#[test]
fn parses_array_of_maps() {
    let v = parse(r#"
users:
  - name: Dan
    role: Admin
  - name: Lisa
    role: User
"#);

    let users = get(&v, "users").as_array().unwrap();

    let first = users[0].as_map().unwrap();
    assert_eq!(first.get("name").unwrap(), &YallValue::Str("Dan".into()));
    assert_eq!(first.get("role").unwrap(), &YallValue::Str("Admin".into()));

    let second = users[1].as_map().unwrap();
    assert_eq!(second.get("name").unwrap(), &YallValue::Str("Lisa".into()));
    assert_eq!(second.get("role").unwrap(), &YallValue::Str("User".into()));
}

#[test]
fn parses_inline_array_without_trailing_comma() {
    let v = parse("ports: [80, 443, 8080]");

    assert_eq!(
        get(&v, "ports"),
        &YallValue::Array(vec![
            YallValue::Int(80),
            YallValue::Int(443),
            YallValue::Int(8080),
        ])
    );
}

#[test]
fn parses_inline_array_with_trailing_comma() {
    let v = parse("ports: [80, 443, 8080,]");

    assert_eq!(
        get(&v, "ports"),
        &YallValue::Array(vec![
            YallValue::Int(80),
            YallValue::Int(443),
            YallValue::Int(8080),
        ])
    );
}

#[test]
fn parses_inline_map_without_trailing_comma() {
    let v = parse(r#"
author: { name: Dan, role: Creator }
"#);

    let author = get(&v, "author").as_map().unwrap();

    assert_eq!(author.get("name").unwrap(), &YallValue::Str("Dan".into()));
    assert_eq!(author.get("role").unwrap(), &YallValue::Str("Creator".into()));
}

#[test]
fn parses_inline_map_with_trailing_comma() {
    let v = parse(r#"
author: { name: Dan, role: Creator, }
"#);

    let author = get(&v, "author").as_map().unwrap();

    assert_eq!(author.get("name").unwrap(), &YallValue::Str("Dan".into()));
    assert_eq!(author.get("role").unwrap(), &YallValue::Str("Creator".into()));
}

#[test]
fn parses_goblin_ecosystem_bare_strings() {
    let v = parse(r#"
portal: #site
need: #need::output_dir
action: trailboss::build_routes_once
path: /default/docs.html
"#);

    assert_eq!(get(&v, "portal"), &YallValue::Str("#site".into()));
    assert_eq!(get(&v, "need"), &YallValue::Str("#need::output_dir".into()));
    assert_eq!(
        get(&v, "action"),
        &YallValue::Str("trailboss::build_routes_once".into())
    );
    assert_eq!(get(&v, "path"), &YallValue::Str("/default/docs.html".into()));
}