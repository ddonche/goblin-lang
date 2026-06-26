// crates/goblin-yall/tests/nil.rs

use goblin_yall::{yall_parse, YallValue};

fn parse(src: &str) -> YallValue {
    yall_parse(src, "nil.yall").unwrap()
}

fn get<'a>(v: &'a YallValue, key: &str) -> &'a YallValue {
    v.as_map().unwrap().get(key).unwrap()
}

#[test]
fn parses_nil_keyword() {
    let v = parse(r#"
parent: nil
"#);

    assert_eq!(get(&v, "parent"), &YallValue::Null);
}

#[test]
fn parses_true_and_false_keywords() {
    let v = parse(r#"
enabled: true
deleted: false
"#);

    assert_eq!(get(&v, "enabled"), &YallValue::Bool(true));
    assert_eq!(get(&v, "deleted"), &YallValue::Bool(false));
}

#[test]
fn null_is_not_a_keyword() {
    let v = parse(r#"
value: null
"#);

    assert_eq!(
        get(&v, "value"),
        &YallValue::Str("null".into())
    );
}

#[test]
fn keywords_are_case_sensitive() {
    let v = parse(r#"
a: TRUE
b: FALSE
c: NIL
d: True
e: False
f: Nil
"#);

    assert_eq!(get(&v, "a"), &YallValue::Str("TRUE".into()));
    assert_eq!(get(&v, "b"), &YallValue::Str("FALSE".into()));
    assert_eq!(get(&v, "c"), &YallValue::Str("NIL".into()));
    assert_eq!(get(&v, "d"), &YallValue::Str("True".into()));
    assert_eq!(get(&v, "e"), &YallValue::Str("False".into()));
    assert_eq!(get(&v, "f"), &YallValue::Str("Nil".into()));
}

#[test]
fn yaml_boolean_words_are_strings() {
    let v = parse(r#"
yes_value: yes
no_value: no
on_value: on
off_value: off
"#);

    assert_eq!(get(&v, "yes_value"), &YallValue::Str("yes".into()));
    assert_eq!(get(&v, "no_value"), &YallValue::Str("no".into()));
    assert_eq!(get(&v, "on_value"), &YallValue::Str("on".into()));
    assert_eq!(get(&v, "off_value"), &YallValue::Str("off".into()));
}

#[test]
fn nil_inside_quotes_is_a_string() {
    let v = parse(r#"
value: "nil"
"#);

    assert_eq!(
        get(&v, "value"),
        &YallValue::Str("nil".into())
    );
}