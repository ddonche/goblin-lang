use crate::{Diag, Session, Span, Value};
use crate::diagnostics::rtcode;
use goblin_diagnostics::{Diagnostic, Severity};

fn wrong_arity(sp: &Span, name: &str, expected: &str, got: usize) -> Diag {
    Diagnostic::new_with_code(
        Severity::Error,
        rtcode::WRONG_ARITY,
        "wrong-arity",
        format!("{name} expected {expected} argument(s), got {got}"),
        sp.clone(),
    )
    .with_link("https://goblinlang.org/docs/errors#R0301")
}

pub fn set_status(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 1 {
        return Err(wrong_arity(sp, "set_status", "1", args.len()));
    }

    let code = match &args[0] {
        Value::Int(n) => *n,
        _ => {
            return Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    rtcode::TYPE_MISMATCH,
                    "type-mismatch",
                    "set_status expects an integer",
                    sp.clone(),
                )
                .with_link("https://goblinlang.org/docs/errors#T0205"),
            )
        }
    };

    sess.response.status = Some(code);
    Ok(Value::Nil)
}

pub fn set_header(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 2 {
        return Err(wrong_arity(sp, "set_header", "2", args.len()));
    }

    let key = match &args[0] {
        Value::Str(s) => s.clone(),
        _ => {
            return Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    rtcode::TYPE_MISMATCH,
                    "type-mismatch",
                    "set_header key must be string",
                    sp.clone(),
                )
                .with_link("https://goblinlang.org/docs/errors#T0205"),
            )
        }
    };

    let value = match &args[1] {
        Value::Str(s) => s.clone(),
        _ => {
            return Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    rtcode::TYPE_MISMATCH,
                    "type-mismatch",
                    "set_header value must be string",
                    sp.clone(),
                )
                .with_link("https://goblinlang.org/docs/errors#T0205"),
            )
        }
    };

    sess.response.headers.insert(key, value);
    Ok(Value::Nil)
}

pub fn set_cookie(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() < 2 || args.len() > 3 {
        return Err(wrong_arity(sp, "set_cookie", "2 or 3", args.len()));
    }

    let name = match &args[0] {
        Value::Str(s) => s.clone(),
        _ => {
            return Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    rtcode::TYPE_MISMATCH,
                    "type-mismatch",
                    "set_cookie name must be string",
                    sp.clone(),
                )
                .with_link("https://goblinlang.org/docs/errors#T0205"),
            )
        }
    };

    let value = match &args[1] {
        Value::Str(s) => s.clone(),
        _ => {
            return Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    rtcode::TYPE_MISMATCH,
                    "type-mismatch",
                    "set_cookie value must be string",
                    sp.clone(),
                )
                .with_link("https://goblinlang.org/docs/errors#T0205"),
            )
        }
    };

    let mut cookie = format!("{name}={value}");

    if args.len() == 3 {
        match &args[2] {
            Value::Map(map) => {
                for (k, v) in map {
                    match (k.as_str(), v) {
                        ("path", Value::Str(s)) => cookie.push_str(&format!("; Path={s}")),
                        ("domain", Value::Str(s)) => cookie.push_str(&format!("; Domain={s}")),
                        ("http_only", Value::Bool(true)) => cookie.push_str("; HttpOnly"),
                        ("secure", Value::Bool(true)) => cookie.push_str("; Secure"),
                        ("max_age", Value::Int(n)) => cookie.push_str(&format!("; Max-Age={n}")),
                        _ => {}
                    }
                }
            }
            Value::MapOrd(map) => {
                for (k, v) in map {
                    match (k.as_str(), v) {
                        ("path", Value::Str(s)) => cookie.push_str(&format!("; Path={s}")),
                        ("domain", Value::Str(s)) => cookie.push_str(&format!("; Domain={s}")),
                        ("http_only", Value::Bool(true)) => cookie.push_str("; HttpOnly"),
                        ("secure", Value::Bool(true)) => cookie.push_str("; Secure"),
                        ("max_age", Value::Int(n)) => cookie.push_str(&format!("; Max-Age={n}")),
                        _ => {}
                    }
                }
            }
            _ => {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        rtcode::TYPE_MISMATCH,
                        "type-mismatch",
                        "set_cookie options must be a map",
                        sp.clone(),
                    )
                    .with_link("https://goblinlang.org/docs/errors#T0205"),
                )
            }
        }
    }

    sess.response.cookies.push(cookie);
    Ok(Value::Nil)
}