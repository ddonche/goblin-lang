//! `:load`, `:eval` and `:inject`.
//!
//! `:load(sources)` reads files into a bundle: a list of source maps
//! `{path, kind, text}` (Goblin code) or `{path, kind, data}` (parsed data).
//! `:eval(x)` runs a bundle's code sources in order and returns one value;
//! `:inject(...)` inside that code records a change to the bundle's data,
//! and all of them are applied together once the code has run.
//! The VM half (running code, finding actions) lives in vm.rs.

use indexmap::IndexMap;

use crate::error::GoblinError;
use crate::value::Value;

/// One `:inject(action, op, target, section?)` call, recorded during an eval.
#[derive(Clone, Debug)]
pub struct Injection {
    pub action: String,
    pub op: InjectOp,
    pub target: String,
    pub section: Option<String>,
    /// The source the call was made from.
    pub source: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InjectOp { Before, After, Override }

fn err(msg: String) -> GoblinError { GoblinError::Runtime(msg) }

/// A list value's items, or None when the value is not a list.
pub fn as_list(v: &Value) -> Option<Vec<Value>> {
    match v {
        Value::Array(a) => Some(a.clone()),
        Value::Collection(c) if !crate::collections::is_map_collection(c) => Some(crate::collections::to_vec(c)),
        _ => None,
    }
}

/// A map value's entries in order, or None when the value is not a map.
pub fn as_map(v: &Value) -> Option<IndexMap<String, Value>> {
    match v {
        Value::MapOrd(m) => Some((**m).clone()),
        Value::Map(m) => Some(m.iter().map(|(k, v)| (k.clone(), v.clone())).collect()),
        Value::Collection(c) if crate::collections::is_map_collection(c) => Some(
            crate::collections::to_pairs(c).into_iter()
                .map(|(k, v)| (match k { Value::Str(s) => s.to_string(), other => crate::builtins::fmt_value_raw(&other) }, v))
                .collect()),
        _ => None,
    }
}

fn str_of(v: &Value) -> Option<String> {
    match v { Value::Str(s) => Some(s.to_string()), _ => None }
}

/// What `:load` makes of a file, decided by its extension.
enum Kind { Code, Data }

fn kind_of(path: &str) -> Option<Kind> {
    let ext = std::path::Path::new(path).extension()?.to_string_lossy().to_lowercase();
    match ext.as_str() {
        "gbln" | "gob" | "imports" => Some(Kind::Code),
        "yall" | "yaml" | "yml" | "json" => Some(Kind::Data),
        _ => None,
    }
}

/// `:load(sources)`: one path or a list of paths, read in the order given.
pub fn load(arg: &Value) -> Result<Value, GoblinError> {
    let paths: Vec<Value> = match arg {
        Value::Str(_) => vec![arg.clone()],
        other => as_list(other).ok_or_else(|| err(format!(
            "load: expects a path or a list of paths, got {}", other.type_name())))?,
    };
    let mut out = Vec::with_capacity(paths.len());
    for p in &paths {
        let path = str_of(p).ok_or_else(|| err(format!(
            "load: every source must be a path string, got {}", p.type_name())))?;
        out.push(load_one(&path)?);
    }
    Ok(Value::Array(out))
}

fn load_one(path: &str) -> Result<Value, GoblinError> {
    let kind = kind_of(path).ok_or_else(|| err(format!(
        "load: `{path}`: unknown kind of file. Code is .gbln, .gob or .imports; data is .yall, .yaml, .yml or .json")))?;
    let text = std::fs::read_to_string(path).map_err(|e| err(format!("load: cannot read `{path}`: {e}")))?;
    let mut m = IndexMap::new();
    m.insert("path".to_string(), Value::Str(path.into()));
    match kind {
        Kind::Code => {
            m.insert("kind".to_string(), Value::Str("code".into()));
            m.insert("text".to_string(), Value::Str(text.into()));
        }
        Kind::Data => {
            let data = if path.to_lowercase().ends_with(".json") {
                crate::builtins::json_parse_ordered(&text)
                    .map_err(|e| err(format!("load: cannot parse `{path}`: {e}")))?
            } else {
                let parsed = goblin_yall::yall_parse(&text, path)
                    .map_err(|e| err(format!("load: cannot parse `{path}`: {e}")))?;
                crate::builtins::yall_to_value(&parsed)
            };
            m.insert("kind".to_string(), Value::Str("data".into()));
            m.insert("data".to_string(), data);
        }
    }
    Ok(Value::MapOrd(Box::new(m)))
}

/// One source of a bundle, ready to evaluate.
pub enum Source {
    Code { path: String, text: String },
    Data { path: String, data: Value },
}

/// The sources `:eval` was handed: a path, a source map, or a list of
/// either (a bundle from `:load`). Paths are loaded first.
pub fn sources_of(arg: &Value) -> Result<Vec<Source>, GoblinError> {
    let items: Vec<Value> = match arg {
        Value::Str(s) => vec![load_one(s)?],
        other => match as_list(other) {
            Some(list) => list,
            None if as_map(other).is_some() => vec![other.clone()],
            None => return Err(err(format!(
                "eval: expects a path, a source from :load, or a list of them, got {}", other.type_name()))),
        },
    };
    let mut out = Vec::with_capacity(items.len());
    for item in items {
        let item = match &item { Value::Str(s) => load_one(s)?, _ => item };
        let m = as_map(&item).ok_or_else(|| err(format!(
            "eval: a bundle holds sources from :load (maps), got {}", item.type_name())))?;
        let path = m.get("path").and_then(str_of).unwrap_or_else(|| "<source>".to_string());
        match m.get("kind").and_then(str_of).as_deref() {
            Some("code") => {
                let text = m.get("text").and_then(str_of).ok_or_else(|| err(format!(
                    "eval: code source `{path}` has no `text`")))?;
                out.push(Source::Code { path, text });
            }
            Some("data") => {
                let data = m.get("data").cloned().ok_or_else(|| err(format!(
                    "eval: data source `{path}` has no `data`")))?;
                out.push(Source::Data { path, data });
            }
            _ => return Err(err(format!(
                "eval: source `{path}` needs `kind` \"code\" or \"data\""))),
        }
    }
    Ok(out)
}

/// The data sources merged into one value. One source is used as it is;
/// several must all be maps, and no key may come from two of them.
pub fn merge_data(data: Vec<(String, Value)>) -> Result<Option<Value>, GoblinError> {
    if data.len() <= 1 {
        return Ok(data.into_iter().next().map(|(_, v)| v));
    }
    let mut merged: IndexMap<String, Value> = IndexMap::new();
    let mut from: IndexMap<String, String> = IndexMap::new();
    for (path, v) in data {
        let m = as_map(&v).ok_or_else(|| err(format!(
            "eval: `{path}` is {}, but data from several sources is merged by key, so each must be a map", v.type_name())))?;
        for (k, v) in m {
            if let Some(first) = from.get(&k) {
                return Err(err(format!("eval: key `{k}` is in both `{first}` and `{path}`")));
            }
            from.insert(k.clone(), path.clone());
            merged.insert(k, v);
        }
    }
    Ok(Some(Value::MapOrd(Box::new(merged))))
}

/// The changes aimed at one target in one section.
#[derive(Default)]
struct Anchor {
    before: Vec<String>,
    after: Vec<String>,
    replace: Option<(String, String)>,
}

/// Apply every recorded injection to `data` at once. Targets are names in
/// the data as loaded, so injections never target each other's actions.
pub fn apply(data: &Value, injections: &[Injection]) -> Result<Value, GoblinError> {
    let mut map = as_map(data).ok_or_else(|| err(format!(
        "inject: the eval's data is {}, but injections change named lists in a map", data.type_name())))?;
    // Sections: keys whose value is a list of names.
    let mut sections: IndexMap<String, Vec<String>> = IndexMap::new();
    for (k, v) in &map {
        if let Some(items) = as_list(v) {
            let names: Option<Vec<String>> = items.iter().map(str_of).collect();
            if let Some(names) = names { sections.insert(k.clone(), names); }
        }
    }
    let mut anchors: IndexMap<(String, String), Anchor> = IndexMap::new();
    for inj in injections {
        let section = match &inj.section {
            Some(sec) => {
                let names = sections.get(sec).ok_or_else(|| err(format!(
                    "inject: `{}` (from `{}`) names section `{sec}`, but the data has no list of names called `{sec}`",
                    inj.action, inj.source)))?;
                if !names.contains(&inj.target) {
                    return Err(err(format!(
                        "inject: `{}` (from `{}`) targets `{}` in `{sec}`, but `{sec}` does not list it",
                        inj.action, inj.source, inj.target)));
                }
                sec.clone()
            }
            None => {
                let found: Vec<&String> = sections.iter()
                    .filter(|(_, names)| names.contains(&inj.target)).map(|(k, _)| k).collect();
                match found.len() {
                    0 => return Err(err(format!(
                        "inject: `{}` (from `{}`) targets `{}`, which no section lists",
                        inj.action, inj.source, inj.target))),
                    1 => found[0].clone(),
                    _ => return Err(err(format!(
                        "inject: `{}` (from `{}`) targets `{}`, which is in {}; name the section as a 4th argument",
                        inj.action, inj.source, inj.target,
                        found.iter().map(|s| format!("`{s}`")).collect::<Vec<_>>().join(" and ")))),
                }
            }
        };
        let anchor = anchors.entry((section.clone(), inj.target.clone())).or_default();
        match inj.op {
            InjectOp::Before => anchor.before.push(inj.action.clone()),
            InjectOp::After => anchor.after.push(inj.action.clone()),
            InjectOp::Override => {
                if let Some((other, other_src)) = &anchor.replace {
                    return Err(err(format!(
                        "inject: `{}` in `{section}` is overridden twice: by `{other}` (from `{other_src}`) and by `{}` (from `{}`)",
                        inj.target, inj.action, inj.source)));
                }
                anchor.replace = Some((inj.action.clone(), inj.source.clone()));
            }
        }
    }
    for (sec, names) in &sections {
        if !anchors.keys().any(|(s, _)| s == sec) { continue; }
        let mut out = Vec::with_capacity(names.len());
        for (i, name) in names.iter().enumerate() {
            let anchor = anchors.get(&(sec.clone(), name.clone()));
            let first = !names[..i].contains(name);
            let last = !names[i + 1..].contains(name);
            if let (Some(a), true) = (anchor, first) {
                out.extend(a.before.iter().map(|s| Value::Str(s.as_str().into())));
            }
            match anchor.and_then(|a| a.replace.as_ref()) {
                Some((with, _)) => out.push(Value::Str(with.as_str().into())),
                None => out.push(Value::Str(name.as_str().into())),
            }
            if let (Some(a), true) = (anchor, last) {
                out.extend(a.after.iter().map(|s| Value::Str(s.as_str().into())));
            }
        }
        map.insert(sec.clone(), Value::Array(out));
    }
    Ok(Value::MapOrd(Box::new(map)))
}

/// Read `:inject`'s arguments into a record.
pub fn injection(args: &[Value], source: String) -> Result<Injection, GoblinError> {
    if args.len() != 3 && args.len() != 4 {
        return Err(GoblinError::ArityMismatch { expected: 3, got: args.len(), name: "inject".into() });
    }
    let s = |i: usize, what: &str| str_of(&args[i]).ok_or_else(|| err(format!(
        "inject: the {what} must be a string, got {}", args[i].type_name())));
    let action = s(0, "action")?;
    let op = match s(1, "operation")?.as_str() {
        "before" => InjectOp::Before,
        "after" => InjectOp::After,
        "override" => InjectOp::Override,
        other => return Err(err(format!(
            "inject: operation must be \"before\", \"after\" or \"override\", got \"{other}\""))),
    };
    let target = s(2, "target")?;
    let section = if args.len() == 4 { Some(s(3, "section")?) } else { None };
    Ok(Injection { action, op, target, section, source })
}
