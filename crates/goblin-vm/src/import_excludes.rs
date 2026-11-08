//! Import exclusions: files `import` and `use` skip.
//!
//! The list comes from `imports.exclude.yall` in the project root, or from
//! the file given with `--import-excludes FILE` (or `GOBLIN_IMPORT_EXCLUDES`),
//! which replaces it. It is read once per process, before any import, and
//! every import in that process uses it.
//!
//! ```text
//! exclude:
//!   - "modules/my_cool_module/**"
//!   - "glams/some_glam"
//! ```
//!
//! An entry is a path relative to the project root (or absolute), and may
//! use `*` and `**` as `import` patterns do. It excludes the file or
//! directory it names and everything under it. Paths are compared by real
//! path, so another spelling or a symlink of an excluded file is excluded too.

use std::path::{Component, Path, PathBuf};
use std::sync::{Arc, OnceLock};

use crate::error::GoblinError;

/// The default file, looked for in the project root.
pub const DEFAULT_FILE: &str = "imports.exclude.yall";

pub struct Excludes {
    /// Each entry as written, and as path segments from the filesystem root.
    entries: Vec<(String, Vec<String>)>,
}

static CONFIG: OnceLock<Result<Option<Arc<Excludes>>, String>> = OnceLock::new();

/// Read the exclusions for this process: `explicit` if given (it must exist),
/// else `imports.exclude.yall` in `root` if it is there. Call it before the
/// program runs; a later call keeps the first result.
pub fn configure(explicit: Option<&Path>, root: &Path) -> Result<(), String> {
    CONFIG.get_or_init(|| read_config(explicit, root)).clone().map(|_| ())
}

fn read_config(explicit: Option<&Path>, root: &Path) -> Result<Option<Arc<Excludes>>, String> {
    let file = match explicit {
        Some(f) => {
            if !f.is_file() {
                return Err(format!("import excludes: `{}` does not exist", f.display()));
            }
            f.to_path_buf()
        }
        None => {
            let f = root.join(DEFAULT_FILE);
            if !f.is_file() { return Ok(None); }
            f
        }
    };
    parse(&file, root).map(|e| Some(Arc::new(e)))
}

fn parse(file: &Path, root: &Path) -> Result<Excludes, String> {
    use goblin_yall::YallValue as Y;
    let shown = file.display();
    let text = std::fs::read_to_string(file).map_err(|e| format!("import excludes: cannot read `{shown}`: {e}"))?;
    let value = goblin_yall::yall_parse(&text, &shown.to_string())
        .map_err(|e| format!("import excludes: `{shown}` is not valid Y'all: {e}"))?;
    let map = match value {
        Y::Map(m) => m,
        _ => return Err(format!("import excludes: `{shown}` must be a map with an `exclude:` list")),
    };
    if map.is_empty() && has_content(&text) {
        return Err(format!("import excludes: `{shown}` must be a map with an `exclude:` list"));
    }
    if let Some(k) = map.keys().find(|k| k.as_str() != "exclude") {
        return Err(format!("import excludes: `{shown}`: unknown key `{k}` (the only key is `exclude`)"));
    }
    let items = match map.get("exclude") {
        // `exclude:` with nothing under it reads as an empty map.
        None | Some(Y::Null) => Vec::new(),
        Some(Y::Map(m)) if m.is_empty() => Vec::new(),
        Some(Y::Array(a)) => a.clone(),
        Some(_) => return Err(format!("import excludes: `{shown}`: `exclude` must be a list of paths")),
    };
    let root = root.canonicalize().unwrap_or_else(|_| root.to_path_buf());
    let mut entries = Vec::with_capacity(items.len());
    for (i, item) in items.iter().enumerate() {
        let text = match item {
            Y::Str(s) if !s.trim().is_empty() => s.trim().to_string(),
            _ => return Err(format!("import excludes: `{shown}`: entry {} must be a non-empty path", i + 1)),
        };
        let segs = compile(&text, &root).map_err(|why| format!("import excludes: `{shown}`: `{text}`: {why}"))?;
        entries.push((text, segs));
    }
    Ok(Excludes { entries })
}

/// Whether `text` holds anything but blank lines and `///` / `////` comments
/// (the Y'all parser reads some malformed files as an empty map).
fn has_content(text: &str) -> bool {
    let mut in_block = false;
    for line in text.lines() {
        let t = line.trim();
        if t == "////" { in_block = !in_block; continue; }
        if in_block || t.is_empty() || t.starts_with("///") { continue; }
        return true;
    }
    false
}

/// An entry as segments of an absolute path. The part before the first
/// wildcard is resolved to its real path when it exists.
fn compile(pattern: &str, root: &Path) -> Result<Vec<String>, String> {
    let p = Path::new(pattern);
    let abs = if p.is_absolute() { p.to_path_buf() } else { root.join(p) };
    let mut literal = PathBuf::new();
    let mut rest: Vec<String> = Vec::new();
    for c in abs.components() {
        let text = c.as_os_str().to_string_lossy().to_string();
        if rest.is_empty() && !text.contains('*') {
            match c {
                Component::ParentDir => { literal.pop(); }
                Component::CurDir => {}
                _ => literal.push(c.as_os_str()),
            }
        } else {
            if matches!(c, Component::ParentDir) {
                return Err("`..` can't follow a wildcard".into());
            }
            if !matches!(c, Component::CurDir) { rest.push(text); }
        }
    }
    let mut segs = segments(&real_path(&literal));
    segs.extend(rest);
    Ok(segs)
}

/// The real path of `p`: its longest existing part resolved (symlinks, `..`),
/// with the rest added as written.
fn real_path(p: &Path) -> PathBuf {
    let mut tail = Vec::new();
    let mut head = p.to_path_buf();
    loop {
        if let Ok(real) = head.canonicalize() {
            let mut out = real;
            for t in tail.iter().rev() { out.push(t); }
            return out;
        }
        match (head.file_name().map(|f| f.to_os_string()), head.parent()) {
            (Some(name), Some(parent)) => { tail.push(name); head = parent.to_path_buf(); }
            _ => return p.to_path_buf(),
        }
    }
}

fn segments(p: &Path) -> Vec<String> {
    p.components().filter_map(|c| match c {
        Component::RootDir | Component::CurDir => None,
        other => Some(other.as_os_str().to_string_lossy().to_string()),
    }).collect()
}

/// Whether an import of `path` is skipped. With no configuration yet (a
/// host that never called `configure`), the project root's default file is
/// read now.
pub fn excluded(path: &Path, root: &Path) -> Result<bool, GoblinError> {
    let cfg = CONFIG.get_or_init(|| read_config(None, root));
    let ex = match cfg {
        Ok(Some(ex)) => ex,
        Ok(None) => return Ok(false),
        Err(e) => return Err(GoblinError::Runtime(e.clone())),
    };
    let abs = if path.is_absolute() { path.to_path_buf() } else { root.join(path) };
    let segs = segments(&real_path(&abs));
    Ok(ex.entries.iter().any(|(_, pat)| covers(pat, &segs)))
}

/// `pat` names `path` or a directory above it.
fn covers(pat: &[String], path: &[String]) -> bool {
    match pat.split_first() {
        None => true,
        Some((s, rest)) if s == "**" => (0..=path.len()).any(|k| covers(rest, &path[k..])),
        Some((s, rest)) => match path.split_first() {
            Some((p, prest)) if crate::vm::segment_matches(s, p) => covers(rest, prest),
            _ => false,
        },
    }
}
