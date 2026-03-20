//! actions/files.rs — file/path/uuid/html helpers

use crate::{Session, Value, Diag, Span};
use crate::diagnostics::rtcode;
use goblin_diagnostics::{Diagnostic, Severity};

use crate::actions::utils::want_str;

use std::path::{Path, PathBuf};
use walkdir::WalkDir;
use uuid::Uuid;
use dunce;

// ==========================================================
// file_exists(path)
// ==========================================================

pub fn file_exists(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 1 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY,
                "wrong-arity",
                &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                sp.clone(),
            )
            .with_help("‘file_exists’ takes exactly 1 argument.")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }
    let path = want_str(&args[0], "file_exists", sp)?;
    Ok(Value::Bool(Path::new(path).exists()))
}

// ==========================================================
// create_dir / write_text (non-bang forms error)
// ==========================================================

pub fn create_dir(_sess: &mut Session, _args: &[Value], sp: &Span) -> Result<Value, Diag> {
    Err(
        Diagnostic::new_with_code(
            Severity::Error,
            rtcode::MUTATION_OPERATOR_REQUIRED,
            "mutation-operator-required",
            "‘create_dir’ requires the bang form: use create_dir!(…)",
            sp.clone(),
        )
        .with_help("Append ‘!’ to create directories, e.g., create_dir!(path).")
        .with_link("https://goblinlang.org/docs/errors#M0001"),
    )
}

pub fn write_text(_sess: &mut Session, _args: &[Value], sp: &Span) -> Result<Value, Diag> {
    Err(
        Diagnostic::new_with_code(
            Severity::Error,
            rtcode::MUTATION_OPERATOR_REQUIRED,
            "mutation-operator-required",
            "‘write_text’ requires the bang form: use write_text!(…)",
            sp.clone(),
        )
        .with_help("Append ‘!’ to write files, e.g., write_text!(path, text).")
        .with_link("https://goblinlang.org/docs/errors#M0001"),
    )
}

pub fn append_file(_sess: &mut Session, _args: &[Value], sp: &Span) -> Result<Value, Diag> {
    Err(
        Diagnostic::new_with_code(
            Severity::Error,
            rtcode::MUTATION_OPERATOR_REQUIRED,
            "mutation-operator-required",
            "'append_file' requires the bang form: use append_file!(…)",
            sp.clone(),
        )
        .with_help("Append '!' to append to files, e.g., append_file!(path, text).")
        .with_link("https://goblinlang.org/docs/errors#M0001"),
    )
}

// ==========================================================
// delete_path(path)
// ==========================================================

pub fn delete_path(_sess: &mut Session, _args: &[Value], sp: &Span) -> Result<Value, Diag> {
    Err(
        Diagnostic::new_with_code(
            Severity::Error,
            rtcode::MUTATION_OPERATOR_REQUIRED,
            "mutation-operator-required",
            "‘delete_path’ requires the bang form: use delete_path!(…)",
            sp.clone(),
        )
        .with_help("Append ‘!’ to delete files or directories, e.g., delete_path!(path).")
        .with_link("https://goblinlang.org/docs/errors#M0001"),
    )
}

// ==========================================================
// read_text(path)
// ==========================================================

pub fn read_text(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 1 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY,
                "wrong-arity",
                &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                sp.clone(),
            )
            .with_help("Usage: read_text(path)")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }

    let path = want_str(&args[0], "read_text path", sp)?;
    let s = std::fs::read_to_string(path).map_err(|e| {
        Diagnostic::new_with_code(
            Severity::Error,
            rtcode::FILESYSTEM_IO,
            "filesystem-io",
            &format!("failed to read file: {e}"),
            sp.clone(),
        )
        .with_help("Check file exists and permissions.")
        .with_link("https://goblinlang.org/docs/errors#FS0001")
    })?;

    Ok(Value::Str(s))
}

// ==========================================================
// copy_file (non-bang errors)
// ==========================================================

pub fn copy_file(_sess: &mut Session, _args: &[Value], sp: &Span) -> Result<Value, Diag> {
    Err(
        Diagnostic::new_with_code(
            Severity::Error,
            rtcode::MUTATION_OPERATOR_REQUIRED,
            "mutation-operator-required",
            "‘copy_file’ requires the bang form: use copy_file!(…)",
            sp.clone(),
        )
        .with_help("Append ‘!’ to copy files, e.g., copy_file!(src, dst).")
        .with_link("https://goblinlang.org/docs/errors#M0001"),
    )
}

// ==========================================================
// stem(path)
// ==========================================================

pub fn stem(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 1 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY,
                "wrong-arity",
                &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                sp.clone(),
            )
            .with_help("'stem(path)' takes exactly 1 argument.")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }
    let path = want_str(&args[0], "stem", sp)?;
    let p = Path::new(path);
    let stem = p.file_stem().and_then(|s| s.to_str()).unwrap_or("");
    Ok(Value::Str(stem.to_string()))
}

// ==========================================================
// ext(path)
// ==========================================================

pub fn ext(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 1 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY,
                "wrong-arity",
                &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                sp.clone(),
            )
            .with_help("'ext(path)' takes exactly 1 argument.")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }
    let path = want_str(&args[0], "ext", sp)?;
    let p = Path::new(path);
    let extension = p
        .extension()
        .and_then(|s| s.to_str())
        .map(|s| format!(".{}", s))
        .unwrap_or_else(|| "".to_string());
    Ok(Value::Str(extension))
}

// ==========================================================
// dirname(path)
// ==========================================================

pub fn dirname(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 1 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY,
                "wrong-arity",
                &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                sp.clone(),
            )
            .with_help("'dirname(path)' takes exactly 1 argument.")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }
    let path = want_str(&args[0], "dirname", sp)?;
    let p = Path::new(path);
    let dir = p.parent().and_then(|s| s.to_str()).unwrap_or("");
    Ok(Value::Str(dir.replace('\\', "/")))
}

// ==========================================================
// path_join(a, b)
// ==========================================================

pub fn path_join(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 2 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY,
                "wrong-arity",
                &format!("Wrong number of arguments (expected 2, got {})", args.len()),
                sp.clone(),
            )
            .with_help("‘path_join(a,b)’ joins two paths safely.")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }
    let a = want_str(&args[0], "path_join", sp)?;
    let b = want_str(&args[1], "path_join", sp)?;

    let mut joined = PathBuf::from(a);
    joined.push(b);
    let normalized = joined.to_string_lossy().replace('\\', "/");
    Ok(Value::Str(normalized))
}

// ==========================================================
// basename(path)
// ==========================================================

pub fn basename(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 1 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY,
                "wrong-arity",
                &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                sp.clone(),
            )
            .with_help("‘basename(path)’ takes exactly 1 argument.")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }

    let path = want_str(&args[0], "basename", sp)?;
    let p = Path::new(path);
    let base = p.file_name().and_then(|s| s.to_str()).unwrap_or("");
    Ok(Value::Str(base.to_string()))
}

// ==========================================================
// path_normalize(path)
// ==========================================================

pub fn path_normalize(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 1 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY,
                "wrong-arity",
                &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                sp.clone(),
            )
            .with_help("‘path_normalize(path)’ takes exactly 1 argument.")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }

    let path = want_str(&args[0], "path_normalize", sp)?;
    let simplified = dunce::simplified(Path::new(path));
    let normalized = simplified.to_string_lossy().replace('\\', "/");
    Ok(Value::Str(normalized))
}

// ==========================================================
// is_file / is_dir
// ==========================================================

pub fn is_file(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 1 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY,
                "wrong-arity",
                &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                sp.clone(),
            )
            .with_help("‘is_file(path)’ takes exactly 1 argument.")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }

    let path = want_str(&args[0], "is_file", sp)?;
    Ok(Value::Bool(Path::new(path).is_file()))
}

pub fn is_dir(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 1 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY,
                "wrong-arity",
                &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                sp.clone(),
            )
            .with_help("‘is_dir(path)’ takes exactly 1 argument.")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }

    let path = want_str(&args[0], "is_dir", sp)?;
    Ok(Value::Bool(Path::new(path).is_dir()))
}

// ==========================================================
// path_split(path)
// ==========================================================

pub fn path_split(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 1 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY,
                "wrong-arity",
                &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                sp.clone(),
            )
            .with_help("‘path_split(path)’ takes exactly 1 argument.")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }

    let path = want_str(&args[0], "path_split", sp)?;
    let comps: Vec<Value> = Path::new(path)
        .components()
        .map(|c| Value::Str(c.as_os_str().to_string_lossy().into_owned()))
        .collect();
    Ok(Value::Array(comps))
}

// ==========================================================
// path_relative_to(path, base)
// ==========================================================

pub fn path_relative_to(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 2 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY,
                "wrong-arity",
                &format!("Wrong number of arguments (expected 2, got {})", args.len()),
                sp.clone(),
            )
            .with_help("‘path_relative_to(path, base)’ takes exactly 2 arguments.")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }

    let path = want_str(&args[0], "path_relative_to", sp)?;
    let base = want_str(&args[1], "path_relative_to", sp)?;

    let rel = Path::new(path)
        .strip_prefix(Path::new(base))
        .unwrap_or(Path::new(path));
    Ok(Value::Str(rel.to_string_lossy().replace('\\', "/")))
}

// ==========================================================
// walk(dir, pattern)
// ==========================================================

pub fn walk(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() < 1 || args.len() > 2 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY,
                "wrong-arity",
                &format!("Wrong number of arguments (expected 1–2, got {})", args.len()),
                sp.clone(),
            )
            .with_help("‘walk(dir, pattern?)’ takes 1 or 2 arguments.")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }

    let root = want_str(&args[0], "walk", sp)?;
    let pattern: String = if args.len() > 1 {
        want_str(&args[1], "walk", sp)?.to_string()
    } else {
        "**/*.md".to_string()
    };

    let mut results = Vec::new();
    for entry in WalkDir::new(root).into_iter().filter_map(|e| e.ok()) {
        let path = entry.path();
        if path.is_file() {
            let ok = match path.extension().and_then(|e| e.to_str()) {
                Some(ext) => match pattern.as_str() {
                    "**/*.md" => ext.eq_ignore_ascii_case("md"),
                    "**/*.gbln" => ext.eq_ignore_ascii_case("gbln"),
                    _ => true,
                },
                None => match pattern.as_str() {
                    "**/*.md" | "**/*.gbln" => false,
                    _ => true,
                },
            };
            if ok {
                let rel = Path::new(".").join(path).strip_prefix(".").unwrap_or(path).to_path_buf();
                results.push(Value::Str(rel.to_string_lossy().replace('\\', "/")));
            }
        }
    }

    Ok(Value::Array(results))
}

// ==========================================================
// list_dirs(path)  -> Array(Str)
// ==========================================================

pub fn list_dirs(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 1 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY,
                "wrong-arity",
                &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                sp.clone(),
            )
            .with_help("Usage: list_dirs(path)")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }

    let root = want_str(&args[0], "list_dirs", sp)?;
    let root_path = Path::new(root);

    // Match your style: empty array if not a dir (no error)
    if !root_path.exists() || !root_path.is_dir() {
        return Ok(Value::Array(vec![]));
    }

    let mut out: Vec<Value> = Vec::new();

    let rd = std::fs::read_dir(root_path).map_err(|e| {
        Diagnostic::new_with_code(
            Severity::Error,
            rtcode::FILESYSTEM_IO,
            "filesystem-io",
            &format!("failed to read dir: {e}"),
            sp.clone(),
        )
        .with_help("Check directory exists and permissions.")
        .with_link("https://goblinlang.org/docs/errors#FS0001")
    })?;

    for entry in rd {
        if let Ok(entry) = entry {
            let p = entry.path();
            if p.is_dir() {
                if let Some(name) = p.file_name().and_then(|s| s.to_str()) {
                    if !name.is_empty() {
                        out.push(Value::Str(name.to_string()));
                    }
                }
            }
        }
    }

    Ok(Value::Array(out))
}

// ==========================================================
// escape_html(text)
// ==========================================================

pub fn escape_html(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 1 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY,
                "wrong-arity",
                &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                sp.clone(),
            )
            .with_help("‘escape_html(text)’ takes exactly 1 argument.")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }

    let input = want_str(&args[0], "escape_html", sp)?;
    let mut out = String::with_capacity(input.len());
    for ch in input.chars() {
        match ch {
            '&'  => out.push_str("&amp;"),
            '<'  => out.push_str("&lt;"),
            '>'  => out.push_str("&gt;"),
            '"'  => out.push_str("&quot;"),
            '\'' => out.push_str("&#39;"),
            _    => out.push(ch),
        }
    }
    Ok(Value::Str(out))
}

// ==========================================================
// uuid_v4 / uuid_v7
// ==========================================================

pub fn uuid_v4(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 0 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY,
                "wrong-arity",
                &format!("Wrong number of arguments (expected 0, got {})", args.len()),
                sp.clone(),
            )
            .with_help("‘uuid_v4’ takes no arguments.")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }
    Ok(Value::Str(Uuid::new_v4().to_string()))
}

pub fn uuid_v7(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 0 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY,
                "wrong-arity",
                &format!("Wrong number of arguments (expected 0, got {})", args.len()),
                sp.clone(),
            )
            .with_help("‘uuid_v7’ takes no arguments.")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }
    Ok(Value::Str(Uuid::now_v7().to_string()))
}

// ==========================================================
// pathfind(from, to, mode?)  — NEW BUILTIN
// ==========================================================

/// pathfind(from, to, mode?) -> Str
/// Compute a relative or href path between two paths (pure lexical, no FS IO)
pub fn pathfind(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    // ---- ARITY CHECK ----
    if args.len() < 2 || args.len() > 3 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY, // R0301
                "wrong-arity",
                &format!("Wrong number of arguments (expected 2–3, got {})", args.len()),
                sp.clone(),
            )
            .with_help("Usage: pathfind(from, to) or pathfind(from, to, \"href\").")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }

    // ---- ARGUMENT TYPES ----
    let from = want_str(&args[0], "pathfind from", sp)?;
    let to   = want_str(&args[1], "pathfind to", sp)?;
    let mode = if args.len() == 3 {
        want_str(&args[2], "pathfind mode", sp)?
    } else {
        "relative"
    };

    // ---- NORMALIZE ----
    fn norm(path: &str) -> String {
        let p = path.replace('\\', "/");
        let mut out = Vec::new();
        for seg in p.split('/') {
            match seg {
                "" | "." => continue,
                ".." => {
                    if let Some(last) = out.last() {
                        if last != ".." {
                            out.pop();
                            continue;
                        }
                    }
                    out.push("..".to_string());
                }
                _ => out.push(seg.to_string()),
            }
        }
        out.join("/")
    }

    let from_norm = norm(from);
    let to_norm   = norm(to);

    // dirname logic
    fn dirname(orig: &str, n: &str) -> String {
        if orig.ends_with('/') {
            return n.trim_end_matches('/').to_string();
        }
        if let Some(idx) = n.rfind('/') {
            n[..idx].to_string()
        } else {
            "".to_string()
        }
    }

    fn segs(path: &str) -> Vec<&str> {
        if path.is_empty() { Vec::new() }
        else { path.split('/').filter(|s| !s.is_empty()).collect() }
    }

    fn common(a: &[&str], b: &[&str]) -> usize {
        let mut n = 0;
        while n < a.len() && n < b.len() && a[n] == b[n] { n += 1; }
        n
    }

    fn relpath(from_dir: &[&str], to: &[&str]) -> String {
        let c = common(from_dir, to);
        let mut out = Vec::new();
        for _ in c..from_dir.len() { out.push("..".to_string()); }
        for seg in &to[c..] { out.push(seg.to_string()); }
        if out.is_empty() {
            // same directory: return filename only
            to.last().unwrap_or(&"").to_string()
        } else {
            out.join("/")
        }
    }

    // ---- MODE ----
    let result = match mode {
        "relative" => {
            let base = dirname(from, &from_norm);
            let fsegs = segs(&base);
            let tsegs = segs(&to_norm);
            relpath(&fsegs, &tsegs)
        }

        "href" => {
            let mut url = format!("/{}", to_norm);
            while url.contains("//") {
                url = url.replace("//", "/");
            }
            if !url.starts_with('/') {
                url.insert(0, '/');
            }
            url
        }

        other => {
            return Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    rtcode::INVALID_PATH_MODE, // R0330
                    "invalid-path-mode",
                    &format!("invalid pathfind mode: {}", other),
                    sp.clone(),
                )
                .with_help("Mode must be \"relative\" or \"href\".")
                .with_link("https://goblinlang.org/docs/errors#R0330"),
            );
        }
    };

    Ok(Value::Str(result))
}