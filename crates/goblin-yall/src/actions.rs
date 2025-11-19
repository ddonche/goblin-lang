use std::collections::BTreeMap;

use goblin_diagnostics::Span;
use goblin_yall::{yall_enforce, YallError};
use serde_yaml;

use crate::runtime::RuntimeError;
use crate::session::Session;
use crate::value::Value;

/// Convert serde_yaml::Value → Goblin Value.
fn yaml_to_value(y: serde_yaml::Value) -> Value {
    use serde_yaml::Value as Y;

    match y {
        Y::Null => Value::Nil,
        Y::Bool(b) => Value::Bool(b),
        Y::Number(n) => {
            if let Some(i) = n.as_i64() {
                Value::Int(i)
            } else if let Some(f) = n.as_f64() {
                Value::Float(f)
            } else {
                // extremely large or weird numbers → just keep as string
                Value::Str(n.to_string())
            }
        }
        Y::String(s) => Value::Str(s),
        Y::Sequence(seq) => {
            let arr = seq.into_iter().map(yaml_to_value).collect();
            Value::Array(arr)
        }
        Y::Mapping(map) => {
            let mut obj = BTreeMap::new();
            for (k, v) in map {
                let key = match k {
                    Y::String(s) => s,
                    other => other.to_string(),
                };
                obj.insert(key, yaml_to_value(v));
            }
            Value::Map(obj)
        }
        other => Value::Str(other.to_string()),
    }
}

/// Helper to grab a string argument and throw a runtime error if it’s not a string.
fn arg_string(
    args: &[Value],
    index: usize,
    name: &str,
    sp: &Span,
) -> Result<String, RuntimeError> {
    match args.get(index) {
        Some(Value::Str(s)) => Ok(s.clone()),
        Some(other) => Err(RuntimeError::new(
            sp.clone(),
            format!(
                "{}: argument {} must be string, got {:?}",
                name,
                index,
                other
            ),
        )),
        None => Err(RuntimeError::new(
            sp.clone(),
            format!("{}: missing argument {}", name, index),
        )),
    }
}

fn map_yall_err(sp: &Span, err: YallError) -> RuntimeError {
    RuntimeError::new(sp.clone(), err.to_string())
}

/// yall_parse(text, label) → Goblin Value
///
/// - text: raw Y’all/YAML-ish string
/// - label: name/path for error messages
pub fn yall_parse(
    _sess: &mut Session,
    args: &[Value],
    sp: &Span,
) -> Result<Value, RuntimeError> {
    let text = arg_string(args, 0, "yall_parse", sp)?;
    let label = arg_string(args, 1, "yall_parse", sp)?;

    // 1) normalize + strip comments + enforce Y’all rules
    let cleaned = yall_enforce(&text, &label, true).map_err(|e| map_yall_err(sp, e))?;

    // 2) hand off to serde_yaml
    let yaml_val: serde_yaml::Value =
        serde_yaml::from_str(&cleaned).map_err(|e| {
            RuntimeError::new(
                sp.clone(),
                format!("Y'all backend YAML parse failed in {}: {}", label, e),
            )
        })?;

    // 3) convert to Goblin Value
    Ok(yaml_to_value(yaml_val))
}

/// yall_parse_file(path) → Goblin Value
///
/// - path: file to read & parse as Y’all
pub fn yall_parse_file(
    _sess: &mut Session,
    args: &[Value],
    sp: &Span,
) -> Result<Value, RuntimeError> {
    let path = arg_string(args, 0, "yall_parse_file", sp)?;

    let text = std::fs::read_to_string(&path).map_err(|e| {
        RuntimeError::new(
            sp.clone(),
            format!("Y'all: cannot read file {}: {}", path, e),
        )
    })?;

    // reuse same pipeline
    let cleaned = yall_enforce(&text, &path, true).map_err(|e| map_yall_err(sp, e))?;

    let yaml_val: serde_yaml::Value =
        serde_yaml::from_str(&cleaned).map_err(|e| {
            RuntimeError::new(
                sp.clone(),
                format!("Y'all backend YAML parse failed in {}: {}", path, e),
            )
        })?;

    Ok(yaml_to_value(yaml_val))
}
