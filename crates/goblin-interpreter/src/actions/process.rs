use crate::{Diag, Session, Span, Value};
use crate::actions::utils::want_str;
use crate::diagnostics::rtcode;
use goblin_diagnostics::{Diagnostic, Severity};
use serde_json as sj;
use std::process::Command;

pub fn run_cmd(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() < 1 || args.len() > 3 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY,
                "wrong-arity",
                &format!("Wrong number of arguments (expected 1–3, got {})", args.len()),
                sp.clone(),
            )
            .with_help("Usage: run_cmd(command), run_cmd(command, cwd), or run_cmd(command, cwd, env)")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }

    let command = want_str(&args[0], "run_cmd command", sp)?;

    let cwd = if args.len() >= 2 {
        Some(want_str(&args[1], "run_cmd cwd", sp)?.to_string())
    } else {
        None
    };

    // Extract optional env map (third argument)
    let env_vars: Vec<(String, String)> = if args.len() == 3 {
        match &args[2] {
            Value::Map(map) => {
                map.iter().map(|(k, v)| (k.clone(), match v {
                    Value::Str(s) => s.clone(),
                    other => other.to_string(),
                })).collect()
            }
            Value::MapOrd(map) => {
                map.iter().map(|(k, v)| (k.clone(), match v {
                    Value::Str(s) => s.clone(),
                    other => other.to_string(),
                })).collect()
            }
            _ => {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        rtcode::TYPE_MISMATCH,
                        "type-mismatch",
                        "run_cmd env argument must be a map",
                        sp.clone(),
                    )
                    .with_help("Pass a map like {\"KEY\": \"value\"} as the third argument.")
                    .with_link("https://goblinlang.org/docs/errors#T0205"),
                );
            }
        }
    } else {
        vec![]
    };

    let mut cmd = if cfg!(target_os = "windows") {
        let mut c = Command::new("cmd");
        c.arg("/C").arg(command);
        c
    } else {
        let mut c = Command::new("sh");
        c.arg("-lc").arg(command);
        c
    };

    if let Some(dir) = cwd {
        if !dir.trim().is_empty() {
            cmd.current_dir(dir);
        }
    }

    for (k, v) in env_vars {
        cmd.env(k, v);
    }

    let output = cmd.output().map_err(|e| {
        Diagnostic::new_with_code(
            Severity::Error,
            rtcode::FILESYSTEM_IO,
            "process-spawn-failed",
            &format!("failed to run command: {e}"),
            sp.clone(),
        )
        .with_help("Check that the command exists and the working directory is valid.")
        .with_link("https://goblinlang.org/docs/errors#FS0001")
    })?;

    let code = output.status.code();
    let ok = output.status.success();
    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();

    let payload = sj::json!({
        "ok": ok,
        "code": code,
        "stdout": stdout,
        "stderr": stderr
    });

    Ok(Value::Str(payload.to_string()))
}