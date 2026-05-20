use crate::{Diag, Session, Span, Value};
use crate::diagnostics::rtcode;
use goblin_diagnostics::{Diagnostic, Severity};
use serde_json::Value as JsonValue;

fn get_env(name: &str) -> String {
    std::env::var(name).unwrap_or_default()
}

fn wrong_arity(sp: &Span, name: &str) -> Diag {
    Diagnostic::new_with_code(
        Severity::Error,
        rtcode::WRONG_ARITY,
        "wrong-arity",
        format!("{name} takes no arguments"),
        sp.clone(),
    )
    .with_link("https://goblinlang.org/docs/errors#R0301")
}

pub fn req_method(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if !args.is_empty() {
        return Err(wrong_arity(sp, "req_method"));
    }
    Ok(Value::Str(get_env("GOBLIN_METHOD")))
}

pub fn req_path(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if !args.is_empty() {
        return Err(wrong_arity(sp, "req_path"));
    }
    Ok(Value::Str(get_env("GOBLIN_PATH")))
}

pub fn req_query(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if !args.is_empty() {
        return Err(wrong_arity(sp, "req_query"));
    }
    Ok(Value::Str(get_env("GOBLIN_QUERY_STRING")))
}

pub fn req_body(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if !args.is_empty() {
        return Err(wrong_arity(sp, "req_body"));
    }
    Ok(Value::Str(get_env("GOBLIN_BODY")))
}

pub fn req_header(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 1 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY,
                "wrong-arity",
                format!("req_header expects 1 argument (got {})", args.len()),
                sp.clone(),
            )
            .with_help("Usage: req_header(name)")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }

    let name = match &args[0] {
        Value::Str(s) => s.to_lowercase(),
        _ => {
            return Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    rtcode::TYPE_MISMATCH,
                    "type-mismatch",
                    "req_header argument must be a string",
                    sp.clone(),
                )
                .with_link("https://goblinlang.org/docs/errors#T0205"),
            )
        }
    };

    let headers_json = get_env("GOBLIN_HEADERS_JSON");

    let parsed: JsonValue = serde_json::from_str(&headers_json).unwrap_or(JsonValue::Null);

    if let JsonValue::Object(map) = parsed {
        for (k, v) in map {
            if k.to_lowercase() == name {
                return Ok(Value::Str(v.as_str().unwrap_or("").to_string()));
            }
        }
    }

    Ok(Value::Nil)
}

pub fn cookie(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 1 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY,
                "wrong-arity",
                format!("cookie expects 1 argument (got {})", args.len()),
                sp.clone(),
            )
            .with_help("Usage: cookie(name)")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }

    let name = match &args[0] {
        Value::Str(s) => s,
        _ => {
            return Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    rtcode::TYPE_MISMATCH,
                    "type-mismatch",
                    "cookie name must be a string",
                    sp.clone(),
                )
                .with_link("https://goblinlang.org/docs/errors#T0205"),
            )
        }
    };

    let headers_json = std::env::var("GOBLIN_HEADERS_JSON").unwrap_or_default();

    let parsed: JsonValue = serde_json::from_str(&headers_json).unwrap_or(JsonValue::Null);

    if let JsonValue::Object(map) = parsed {
        for (k, v) in map {
            if k.eq_ignore_ascii_case("cookie") {
                if let Some(cookie_str) = v.as_str() {
                    for pair in cookie_str.split(';') {
                        let mut parts = pair.trim().splitn(2, '=');
                        let key = parts.next().unwrap_or("").trim();
                        let val = parts.next().unwrap_or("").trim();

                        if key == name {
                            return Ok(Value::Str(val.to_string()));
                        }
                    }
                }
            }
        }
    }

    Ok(Value::Nil)
}