use crate::{Diag, Session, Span, Value};
use crate::actions::utils::want_str;
use crate::diagnostics::rtcode;
use goblin_diagnostics::{Diagnostic, Severity};

use serde_json as sj;
use std::process::Command;

pub fn run_cmd(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() < 1 || args.len() > 2 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY,
                "wrong-arity",
                &format!("Wrong number of arguments (expected 1–2, got {})", args.len()),
                sp.clone(),
            )
            .with_help("Usage: run_cmd(command) or run_cmd(command, cwd)")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }

    let command = want_str(&args[0], "run_cmd command", sp)?;
    let cwd = if args.len() == 2 {
        Some(want_str(&args[1], "run_cmd cwd", sp)?.to_string())
    } else {
        None
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