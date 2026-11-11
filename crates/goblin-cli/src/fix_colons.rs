//! `goblin fix colons <dir|file>...`: rewrite Goblin code for the colon rule
//! (owner, 2026-11-11): `:name(...)` calls what Goblin provides, `name(...)`
//! calls the program's own action.
//!
//! It works on tokens, so formatting and comments stay as written:
//! - a bare call to a builtin (`lower(x)`, `say x`, `tick()`) gets a colon,
//!   unless the file declares an action of that name;
//! - a colon call to an action the file declares (`:shout(x)`) loses it.
//! Code inside `{...}` in strings is fixed the same way. Anything it cannot
//! decide is listed and left alone.

use goblin_lexer::{lex, Token, TokenKind};
use goblin_parser::is_no_parens_builtin;
use goblin_vm::compiler::{is_goblin_builtin, KEYWORD_FORMS};
use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};

/// Words that look like no-parentheses builtins but are syntax.
const SENTENCE_FORMS: &[&str] = &["raw", "sweep", "sweep_all", "judge", "judge_all", "pick", "reap", "roll", "roll_detail"];

#[derive(Default)]
struct Report {
    added: usize,
    removed: usize,
    notes: Vec<String>,
}

pub fn run(args: &[String]) -> i32 {
    let check = args.iter().any(|a| a == "--check");
    let md = args.iter().any(|a| a == "--md");
    let paths: Vec<&String> = args.iter().filter(|a| !a.starts_with("--")).collect();
    if paths.is_empty() {
        eprintln!("usage: goblin fix colons [--check] [--md] <dir|file>...");
        return 2;
    }
    let mut files = Vec::new();
    for p in paths { collect_files(Path::new(p), md, &mut files); }
    files.sort();
    let (mut added, mut removed, mut changed, mut failed) = (0, 0, 0, 0);
    for f in &files {
        let Ok(text) = fs::read_to_string(f) else { continue };
        let bom = if text.starts_with('\u{feff}') { "\u{feff}" } else { "" };
        let src = text[bom.len()..].to_string();
        let mut rep = Report::default();
        let is_rust = f.extension().is_some_and(|e| e == "rs");
        let is_md = f.extension().is_some_and(|e| e == "md");
        let fixed = if is_rust { Some(fix_rust(&src, &mut rep)) }
            else if is_md { Some(fix_markdown(&src, &mut rep)) }
            else { fix_source(&src, &f.to_string_lossy(), &mut rep) };
        let out = match fixed {
            Some(o) => o,
            None => { failed += 1; eprintln!("{}: could not read the code (lexer error); left as is", f.display()); continue; }
        };
        for n in &rep.notes { eprintln!("{}: {}", f.display(), n); }
        if out != src {
            changed += 1;
            added += rep.added;
            removed += rep.removed;
            println!("{}: {} colon(s) added, {} removed", f.display(), rep.added, rep.removed);
            if !check {
                if let Err(e) = fs::write(f, format!("{bom}{out}")) { eprintln!("{}: {e}", f.display()); failed += 1; }
            }
        }
    }
    println!("{} file(s) {}, {} colon(s) added, {} removed, {} file(s) left as is",
        changed, if check { "would change" } else { "changed" }, added, removed, failed);
    if check && changed > 0 { 1 } else if failed > 0 { 1 } else { 0 }
}

fn collect_files(p: &Path, md: bool, out: &mut Vec<PathBuf>) {
    if p.is_file() {
        out.push(p.to_path_buf());
        return;
    }
    let Ok(rd) = fs::read_dir(p) else { return };
    for e in rd.flatten() {
        let path = e.path();
        let name = e.file_name();
        let name = name.to_string_lossy();
        if path.is_dir() {
            if name.starts_with('.') || name == "target" || name == "node_modules" { continue; }
            collect_files(&path, md, out);
        } else if name.ends_with(".gbln") || name.ends_with(".gob") || (md && name.ends_with(".md")) {
            out.push(path);
        }
    }
}

/// The actions a file declares (`act name` / `action name`), at any depth.
fn declared_actions(toks: &[Token]) -> HashSet<String> {
    let mut out = HashSet::new();
    for (i, w) in toks.windows(2).enumerate() {
        if matches!(w[0].kind, TokenKind::Act | TokenKind::Action) && w[1].kind == TokenKind::Ident {
            if let Some(v) = &w[1].value {
                let bang = is_op(toks.get(i + 2), "!");
                out.insert(if bang { format!("{v}!") } else { v.clone() });
            }
        }
    }
    out
}

/// Names the file binds as variables (`name | ...`, `name |= ...`, parameters
/// are left out: a variable named like a builtin used as `count + 1`).
fn bound_names(toks: &[Token]) -> HashSet<String> {
    let mut out = HashSet::new();
    for w in toks.windows(2) {
        if w[0].kind == TokenKind::Ident && matches!(&w[1].kind, TokenKind::Op(o) if o == "|" || o == "|=") {
            if let Some(v) = &w[0].value { out.insert(v.clone()); }
        }
    }
    // parameters: `act name(a, b)`
    let mut i = 0;
    while i + 2 < toks.len() {
        if matches!(toks[i].kind, TokenKind::Act | TokenKind::Action) {
            let mut j = i + 1;
            while j < toks.len() && !is_op(toks.get(j), "(") && toks[j].kind != TokenKind::Newline { j += 1; }
            while j < toks.len() && !is_op(toks.get(j), ")") {
                if toks[j].kind == TokenKind::Ident { if let Some(v) = &toks[j].value { out.insert(v.clone()); } }
                j += 1;
            }
            i = j;
        }
        i += 1;
    }
    out
}

fn fix_source(src: &str, file: &str, rep: &mut Report) -> Option<String> {
    let toks = lex(src, file).ok()?;
    let actions = declared_actions(&toks);
    let bound = bound_names(&toks);
    Some(fix_tokens(src, &toks, &actions, &bound, rep, 0))
}

fn is_op(t: Option<&Token>, op: &str) -> bool {
    matches!(t.map(|t| &t.kind), Some(TokenKind::Op(o)) if o == op)
}

/// Whether the token after a no-parentheses builtin starts its argument
/// (as the parser decides), on the same line.
fn starts_arg(name: &Token, t: Option<&Token>, next: Option<&Token>) -> bool {
    let Some(t) = t else { return false };
    match &t.kind {
        TokenKind::String | TokenKind::Int | TokenKind::Float | TokenKind::Money | TokenKind::Duration
        | TokenKind::Date | TokenKind::Time | TokenKind::DateTime | TokenKind::Blob => true,
        TokenKind::Ident => t.value.as_deref() != Some("as"),
        TokenKind::Op(s) if s == ":" => t.span.start > name.span.end
            && next.is_some_and(|n| n.kind == TokenKind::Ident && n.span.start == t.span.end),
        TokenKind::Op(s) => matches!(s.as_str(), "(" | "[" | "{" | "-" | "+" | "!" | "@"),
        _ => false,
    }
}

fn fix_tokens(src: &str, toks: &[Token], actions: &HashSet<String>, bound: &HashSet<String>, rep: &mut Report, base_line: u32) -> String {
    // Edits as (byte offset, bytes to remove, text to insert), in order.
    let mut edits: Vec<(usize, usize, String)> = Vec::new();
    let line_of = |t: &Token| t.span.line_start + base_line;
    for i in 0..toks.len() {
        let t = &toks[i];
        if t.kind == TokenKind::String {
            if let Some(fixed) = fix_string(src, t, actions, bound, rep) {
                edits.push((t.span.start, t.span.end - t.span.start, fixed));
            }
            continue;
        }
        if t.kind != TokenKind::Ident { continue; }
        let Some(name) = t.value.as_deref() else { continue };
        let prev = if i > 0 { toks.get(i - 1) } else { None };
        let next = toks.get(i + 1);
        let after = toks.get(i + 2);
        // `update!(x, …)`: the lexer gives the `!` apart.
        let bang = is_op(next, "!") && next.is_some_and(|n| n.span.start == t.span.end) && is_op(after, "(");
        let call_parens = is_op(next, "(") || bang;
        let bang_name = format!("{name}!");
        let name: &str = if bang { &bang_name } else { name };
        let after = if bang { toks.get(i + 3) } else { after };
        // `:name`: the colon touches the name and does not follow a value
        // (which would make it `key: value` or a type).
        let colon_call = is_op(prev, ":") && prev.is_some_and(|p| p.span.end == t.span.start)
            && (i < 2 || {
                let pp = &toks[i - 2];
                !(pp.span.end == prev.unwrap().span.start && matches!(pp.kind, TokenKind::Ident | TokenKind::ClassIdent | TokenKind::String | TokenKind::Int | TokenKind::Float))
                    && !is_op(Some(pp), ")") && !is_op(Some(pp), "]")
            });
        if colon_call {
            if actions.contains(name) && (call_parens || is_no_parens_builtin(name)) {
                if is_goblin_builtin(name) {
                    rep.notes.push(format!("line {}: `:{name}` called your action `{name}`, which shares a builtin's name; it is now `{name}(...)`", line_of(t)));
                }
                let p = prev.unwrap();
                edits.push((p.span.start, p.span.end - p.span.start, String::new()));
                rep.removed += 1;
            }
            continue;
        }
        // Declarations, methods, members and namespaced names are not builtin calls.
        if matches!(prev.map(|p| &p.kind), Some(TokenKind::Act | TokenKind::Action))
            || is_op(prev, ".") || is_op(prev, "::") || is_op(prev, ">>") || is_op(prev, "?.")
            || is_op(next, "::") {
            continue;
        }
        if KEYWORD_FORMS.contains(&name) || actions.contains(name) || !is_goblin_builtin(name) { continue; }
        let no_parens = !call_parens
            && is_no_parens_builtin(name)
            && !SENTENCE_FORMS.contains(&name)
            && next.is_some_and(|n| n.span.line_start == t.span.line_start)
            && starts_arg(t, next, after);
        if call_parens || no_parens {
            if no_parens && name != "say" && (bound.contains(name) || is_op(next, "[") || is_op(next, "+") || is_op(next, "-")) {
                rep.notes.push(format!("line {}: `{name}` is also a variable here; left as is", line_of(t)));
                continue;
            }
            edits.push((t.span.start, 0, ":".into()));
            rep.added += 1;
        }
    }
    let mut out = String::with_capacity(src.len() + edits.len());
    let mut at = 0;
    for (pos, del, ins) in edits {
        if pos < at { continue; }
        out.push_str(&src[at..pos]);
        out.push_str(&ins);
        at = pos + del;
    }
    out.push_str(&src[at..]);
    out
}

/// Fix the code inside `{...}` in a string literal; None when nothing changes.
fn fix_string(src: &str, t: &Token, actions: &HashSet<String>, bound: &HashSet<String>, rep: &mut Report) -> Option<String> {
    let raw = src.get(t.span.start..t.span.end)?;
    if !raw.contains('{') { return None; }
    let bytes = raw.as_bytes();
    let mut out = String::new();
    let mut i = 0;
    let mut last = 0;
    let mut changed = false;
    while i < bytes.len() {
        if bytes[i] == b'\\' { i += 2; continue; }
        if bytes[i] == b'{' {
            // find the matching close brace
            let mut depth = 0;
            let mut j = i;
            let mut in_q = false;
            while j < bytes.len() {
                match bytes[j] {
                    b'\\' => { j += 1; }
                    b'"' => in_q = !in_q,
                    b'{' if !in_q => depth += 1,
                    b'}' if !in_q => { depth -= 1; if depth == 0 { break; } }
                    _ => {}
                }
                j += 1;
            }
            if j >= bytes.len() { break; }
            let inner = &raw[i + 1..j];
            if let Ok(toks) = lex(inner, "<string>") {
                let fixed = fix_tokens(inner, &toks, actions, bound, rep, t.span.line_start.saturating_sub(1));
                if fixed != inner {
                    out.push_str(&raw[last..i + 1]);
                    out.push_str(&fixed);
                    last = j;
                    changed = true;
                }
            }
            i = j + 1;
            continue;
        }
        i += 1;
    }
    if !changed { return None; }
    out.push_str(&raw[last..]);
    Some(out)
}

/// Goblin code inside a Rust file's string literals (tests that write a
/// `.gbln` file): `r#"..."#` raw strings and plain `"..."` strings using
/// only `\n`, `\t`, `\"` and `\\`. Only a literal with a line break counts as
/// code; one that does not lex or has nothing to fix stays as written.
fn fix_rust(src: &str, rep: &mut Report) -> String {
    let b = src.as_bytes();
    let mut out = String::with_capacity(src.len());
    let mut i = 0;
    let mut last = 0;
    while i < b.len() {
        // skip comments
        if b[i] == b'/' && b.get(i + 1) == Some(&b'/') {
            while i < b.len() && b[i] != b'\n' { i += 1; }
            continue;
        }
        // char literals like '"'
        if b[i] == b'\'' && b.get(i + 2) == Some(&b'\'') { i += 3; continue; }
        if b[i] == b'\'' && b.get(i + 1) == Some(&b'\\') && b.get(i + 3) == Some(&b'\'') { i += 4; continue; }
        if b[i] == b'r' && (b.get(i + 1) == Some(&b'#') || b.get(i + 1) == Some(&b'"'))
            && (i == 0 || !(b[i - 1].is_ascii_alphanumeric() || b[i - 1] == b'_')) {
            let mut j = i + 1;
            let mut hashes = 0;
            while b.get(j) == Some(&b'#') { hashes += 1; j += 1; }
            if b.get(j) != Some(&b'"') { i += 1; continue; }
            let start = j + 1;
            let close: String = std::iter::once('"').chain(std::iter::repeat('#').take(hashes)).collect();
            let Some(len) = src[start..].find(&close) else { break };
            let body = &src[start..start + len];
            if let Some(fixed) = fix_snippet(body, rep) {
                out.push_str(&src[last..start]);
                out.push_str(&fixed);
                last = start + len;
            }
            i = start + len + close.len();
            continue;
        }
        if b[i] == b'"' {
            let start = i + 1;
            let mut j = start;
            let mut decoded = String::new();
            let mut simple = true;
            while j < b.len() && b[j] != b'"' {
                if b[j] == b'\\' {
                    match b.get(j + 1) {
                        Some(b'n') => decoded.push('\n'),
                        Some(b't') => decoded.push('\t'),
                        Some(b'"') => decoded.push('"'),
                        Some(b'\\') => decoded.push('\\'),
                        _ => simple = false,
                    }
                    j += 2;
                    continue;
                }
                let ch = src[j..].chars().next().unwrap();
                decoded.push(ch);
                j += ch.len_utf8();
            }
            if simple && j < b.len() {
                if let Some(fixed) = fix_snippet(&decoded, rep) {
                    let enc = fixed.replace('\\', "\\\\").replace('"', "\\\"").replace('\n', "\\n").replace('\t', "\\t");
                    out.push_str(&src[last..start]);
                    out.push_str(&enc);
                    last = j;
                }
            }
            i = j + 1;
            continue;
        }
        i += 1;
    }
    out.push_str(&src[last..]);
    out
}

fn fix_snippet(code: &str, rep: &mut Report) -> Option<String> {
    // A program has lines; a one-line message (`"actor(s) busy"`) is text.
    if !code.contains('\n') { return None; }
    let mut r = Report::default();
    let fixed = fix_source(code, "<rust string>", &mut r)?;
    if fixed == code { return None; }
    rep.added += r.added;
    rep.removed += r.removed;
    rep.notes.extend(r.notes);
    Some(fixed)
}

/// Code samples in a Markdown file: fenced blocks marked `goblin` or `gbln`.
fn fix_markdown(src: &str, rep: &mut Report) -> String {
    let mut out = String::with_capacity(src.len());
    let mut lines = src.split_inclusive('\n').peekable();
    while let Some(line) = lines.next() {
        out.push_str(line);
        let t = line.trim_start();
        let fence = if t.starts_with("```") { "```" } else if t.starts_with("~~~") { "~~~" } else { continue };
        let lang = t[3..].trim().to_ascii_lowercase();
        if !(lang.starts_with("goblin") || lang.starts_with("gbln")) { continue; }
        let mut body = String::new();
        let mut close = None;
        while let Some(l) = lines.next() {
            if l.trim_start().starts_with(fence) { close = Some(l); break; }
            body.push_str(l);
        }
        let mut r = Report::default();
        match fix_source(&body, "<markdown>", &mut r) {
            Some(fixed) if fixed != body => {
                rep.added += r.added; rep.removed += r.removed; rep.notes.extend(r.notes);
                out.push_str(&fixed);
            }
            _ => out.push_str(&body),
        }
        if let Some(c) = close { out.push_str(c); }
    }
    out
}
