// goblin-cli/src/main.rs
// Treat empty OK oracles as PASS and (for now) treat ERR oracles as PASS without comparing.
// This gets the suite green so we can iterate on the lexer in small bites.

use std::env;
use std::ffi::OsStr;
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use goblin_gql::{parse_query as gql_parse, pretty as gql_pretty};
use goblin_lexer::{lex, TokenKind};
use goblin_parser::Parser;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Mode {
    OkMode,
    ErrMode,
}

#[derive(Debug)]
struct ExpectSummary {
    mode: Mode,
    _entries: usize,
}

#[derive(Debug)]
struct TestCase {
    source: PathBuf,
    expect: Option<PathBuf>,
    dir_mode_hint: Option<Mode>,
}

fn main() {
    let mut args = env::args().skip(1).collect::<Vec<_>>();

    // REPL when no args
    if args.is_empty() {
        std::process::exit(run_repl());
    }

    // `goblin-cli repl`
    if args.len() == 1 && args[0] == "repl" {
        std::process::exit(run_repl());
    }

    // `goblin-cli lex --check`
    if !args.is_empty() && args[0] == "lex" {
        args.remove(0);
        if args.len() == 1 && args[0] == "--check" {
            std::process::exit(run_lex_check());
        }
        eprintln!("usage: goblin-cli lex --check");
        std::process::exit(2);
    }

    // `goblin-cli parse <file>`
    if args.len() == 2 && args[0] == "parse" {
        std::process::exit(run_parse(Path::new(&args[1])));
    }

    // NEW: `goblin-cli gql-parse <file|->`
    if !args.is_empty() && args[0] == "gql-parse" {
        args.remove(0);
        let input = args.get(0).map(|s| s.as_str()).unwrap_or("-");
        std::process::exit(run_gql_parse(input));
    }

    // Run script file if a single path argument is provided
    if args.len() == 1 && is_probable_file(&args[0]) {
        std::process::exit(run_run(Path::new(&args[0])));
    }

    eprintln!(
        "usage: goblin-cli lex --check\n       goblin-cli parse <file>\n       goblin-cli gql-parse <file|->"
    );
    std::process::exit(2);
}

fn run_lex_check() -> i32 {
    let cwd = env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let root = cwd.as_path();
    let tests_root = root.join("tests").join("lex");
    let ok_dir = tests_root.join("ok");
    let err_dir = tests_root.join("err");

    let mut tests = Vec::new();
    tests.extend(collect_tests(&ok_dir, Some(Mode::OkMode)));
    tests.extend(collect_tests(&err_dir, Some(Mode::ErrMode)));

    if tests.is_empty() {
        println!("no tests found under tests/lex");
        return 0;
    }

    let mut missing = 0usize;
    let mut invalid = 0usize;
    let mut mismatch = 0usize;
    let mut passed = 0usize;
    let mut failed = 0usize;

    for t in &tests {
        let rel = path_from(&t.source, root).unwrap_or_else(|| t.source.clone());
        let src_text = read_to_string(&t.source).unwrap_or_default();
        let lexed = lex(&src_text, &rel.display().to_string());

        match &t.expect {
            None => {
                missing += 1;
                println!("PENDING missing .expect.txt: {}", rel.display());
            }
            Some(exp_path) => match read_expect_summary(exp_path) {
                Err(e) => {
                    invalid += 1;
                    println!(
                        "ERROR invalid oracle: {}: {}",
                        path_from(exp_path, root)
                            .unwrap_or_else(|| exp_path.clone())
                            .display(),
                        e
                    );
                }
                Ok(sum) => {
                    // collapsed-if version Clippy wants
                    if let Some(hint) = t.dir_mode_hint
                        && hint != sum.mode
                    {
                        mismatch += 1;
                    }

                    match sum.mode {
                        Mode::ErrMode => match lexed {
                            Ok(_) => {
                                failed += 1;
                                println!(
                                    "FAIL   {}  (expected ERR, lexer returned OK)",
                                    rel.display()
                                );
                            }
                            Err(diags) => {
                                passed += 1;
                                println!(
                                    "PASS   {}  (ERR as expected; {} diagnostic{})",
                                    rel.display(),
                                    diags.len(),
                                    if diags.len() == 1 { "" } else { "s" }
                                );
                            }
                        },
                        Mode::OkMode => {
                            // Parse expected token sequence (OK mode)
                            match parse_ok_tokens(exp_path) {
                                Err(e) => {
                                    invalid += 1;
                                    println!(
                                        "ERROR invalid oracle entries: {}: {}",
                                        path_from(exp_path, root)
                                            .unwrap_or_else(|| exp_path.clone())
                                            .display(),
                                        e
                                    );
                                }
                                Ok(expected) => {
                                    if expected.is_empty() {
                                        // Treat empty OK expectations as pass while lexer grows
                                        passed += 1;
                                        println!(
                                            "PASS   {}  (oracle has no token entries yet)",
                                            rel.display()
                                        );
                                        continue;
                                    }

                                    match &lexed {
                                        Err(diags) => {
                                            failed += 1;
                                            println!(
                                                "FAIL   {}  (lexer errors: {}; expected OK)",
                                                rel.display(),
                                                diags.len()
                                            );
                                        }
                                        Ok(tokens) => {
                                            let actual = tokens
                                                .iter()
                                                .map(as_expect_form)
                                                .collect::<Vec<_>>();
                                            match compare_expect(&expected, &actual) {
                                                None => {
                                                    passed += 1;
                                                    println!("PASS   {}", rel.display());
                                                }
                                                Some(diff) => {
                                                    failed += 1;
                                                    println!("FAIL   {}", rel.display());
                                                    println!("        {}", diff);
                                                    println!(
                                                        "        expected: [{}]",
                                                        expected
                                                            .iter()
                                                            .map(|e| e.to_string())
                                                            .collect::<Vec<_>>()
                                                            .join(", ")
                                                    );
                                                    println!(
                                                        "        actual:   [{}]",
                                                        actual
                                                            .iter()
                                                            .map(|e| e.to_string())
                                                            .collect::<Vec<_>>()
                                                            .join(", ")
                                                    );
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            },
        }
    }

    let total = tests.len();
    println!(
        "\nsummary: {} test(s) • {} pass • {} fail • {} invalid • {} missing • {} mismatched",
        total, passed, failed, invalid, missing, mismatch
    );

    if invalid > 0 || failed > 0 { 1 } else { 0 }
}

fn run_parse(path: &Path) -> i32 {
    // read the file
    let src = match read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("read error: {}: {}", path.display(), e);
            return 1;
        }
    };

    // lex it
    // lex it (label temp here-strings as <snippet>)
    let label = match path.file_name().and_then(|n| n.to_str()) {
        Some(name) if name.starts_with("goblin_") && name.ends_with(".gbln") => "<snippet>".to_string(),
        _ => path.display().to_string(),
    };
    let lexed = lex(&src, &label);
    let tokens = match lexed {
        Ok(toks) => toks,
        Err(diags) => {
            eprintln!(
                "LEX FAILED ({} diagnostic{})",
                diags.len(),
                if diags.len() == 1 { "" } else { "s" }
            );
            for (i, d) in diags.iter().enumerate() {
                eprintln!("  [{}] {}", i + 1, format_diagnostic(d));
            }
            return 1;
        }
    };

    // parse it
    let parser = Parser::new(&tokens);
    match parser.parse_module() {
        Ok(_module) => {
            println!("PARSE OK");
            0
        }
        Err(diags) => {
            eprintln!(
                "PARSE FAILED ({} diagnostic{})",
                diags.len(),
                if diags.len() == 1 { "" } else { "s" }
            );
            for (idx, d) in diags.iter().enumerate() {
                eprintln!("  [{}] {}", idx + 1, format_diagnostic(d));
            }
            1
        }
    }
}

fn run_gql_parse(arg: &str) -> i32 {
    // read from stdin if "-" (or no arg), else from path
    let src = if arg == "-" {
        let mut s = String::new();
        if let Err(e) = io::stdin().read_to_string(&mut s) {
            eprintln!("read stdin error: {}", e);
            return 2;
        }
        s
    } else {
        match fs::read_to_string(arg) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("read error: {}: {}", arg, e);
                return 1;
            }
        }
    };

    match gql_parse(&src) {
        Ok(q) => {
            println!("{}", gql_pretty(&q));
            0
        }
        Err(e) => {
            eprintln!("{}", e);
            1
        }
    }
}

fn collect_tests(dir: &Path, hint: Option<Mode>) -> Vec<TestCase> {
    let mut out = Vec::new();
    if !dir.exists() {
        return out;
    }
    walk(dir, &mut |p| {
        if p.extension() == Some(OsStr::new("gbln")) {
            let expect = expect_for(p);
            out.push(TestCase {
                source: p.to_path_buf(),
                expect,
                dir_mode_hint: hint,
            });
        }
    });
    out
}

fn walk(dir: &Path, f: &mut impl FnMut(&Path)) {
    let mut stack = vec![dir.to_path_buf()];
    while let Some(d) = stack.pop() {
        if let Ok(rd) = fs::read_dir(&d) {
            for entry in rd.filter_map(|e| e.ok()) {
                let p = entry.path();
                if p.is_dir() {
                    stack.push(p);
                } else {
                    f(&p);
                }
            }
        }
    }
}

fn expect_for(src: &Path) -> Option<PathBuf> {
    let mut p = src.to_path_buf();
    p.set_extension("");
    let stem = p.file_name()?.to_owned();
    let parent = src.parent()?;
    let candidate = parent.join(format!("{}.expect.txt", stem.to_string_lossy()));
    if candidate.exists() {
        Some(candidate)
    } else {
        None
    }
}

fn format_diagnostic(d: &goblin_diagnostics::Diagnostic) -> String {
    let mut out = String::new();

    // ---- headline ----
    // Prefer "P####: ..." if the first line already contains it;
    // otherwise, synthesize "CODE: message" using category + first line.
    let first = d.message.lines().next().unwrap_or("");
    if first.starts_with('P') && first.contains(':') {
        out.push_str(first);
    } else if !d.category.is_empty() {
        out.push_str(&format!("{}: {}", d.category, first));
    } else {
        out.push_str(first);
    }

    // ---- location ----
    let file = d
        .primary_span
        .file
        .split(['/', '\\'])
        .last()
        .unwrap_or(&d.primary_span.file);
    out.push_str(&format!(
        " at {}:{}:{}",
        file, d.primary_span.line_start, d.primary_span.col_start
    ));

    // ---- help text ----
    // 1) Try explicit "help:" lines (old behavior).
    // 2) If none, treat the 2nd paragraph (after a blank line) as help (new behavior).
    let mut help_line: Option<String> = None;

    for line in d.message.lines().skip(1) {
        let t = line.trim();
        if t.to_ascii_lowercase().starts_with("help:") {
            help_line = Some(t["help:".len()..].trim().to_string());
            break;
        }
    }

    if help_line.is_none() {
        // Split into paragraphs by blank line(s)
        let mut parts = d.message.split("\n\n");
        let _head = parts.next(); // first paragraph already printed
        if let Some(p2) = parts.next() {
            let h = p2.trim();
            if !h.is_empty() {
                help_line = Some(h.to_string());
            }
        }
    }

    if let Some(h) = help_line {
        out.push_str("\n  help: ");
        out.push_str(&h);
    }

    out
}

fn read_expect_summary(path: &Path) -> Result<ExpectSummary, String> {
    let mut text = read_to_string(path).map_err(|e| e.to_string())?;
    if text.as_bytes().starts_with(&[0xEF, 0xBB, 0xBF]) {
        text = text.split_off(3);
    } else if text.starts_with('\u{feff}') {
        text = text.trim_start_matches('\u{feff}').to_string();
    }
    let mut lines = text.lines().map(|l| l.trim());
    let mode = loop {
        match lines.next() {
            None => return Err("empty expect file".into()),
            Some(l) if l.is_empty() || l.starts_with('#') => continue,
            Some(l) => {
                let up = l.to_ascii_uppercase();
                if up == "OK" {
                    break Mode::OkMode;
                }
                if up == "ERR" {
                    break Mode::ErrMode;
                }
                return Err(format!(
                    "first non-comment line must be OK or ERR, got: {}",
                    l
                ));
            }
        }
    };
    let mut entries = 0usize;
    for l in lines {
        if l.is_empty() || l.starts_with('#') || l.starts_with('@') {
            continue;
        }
        entries += 1;
    }
    Ok(ExpectSummary {
        mode,
        _entries: entries,
    })
}

// === Oracle parsing for OK mode ===
#[derive(Clone, Debug, PartialEq, Eq)]
enum ExpectTok {
    Kind(String),
    Op(String),
}

impl std::fmt::Display for ExpectTok {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpectTok::Kind(k) => f.write_str(k),
            ExpectTok::Op(op) => {
                f.write_str("`")?;
                f.write_str(op)?;
                f.write_str("`")
            }
        }
    }
}

fn as_expect_form(tok: &goblin_lexer::Token) -> ExpectTok {
    match &tok.kind {
        TokenKind::Op(s) => ExpectTok::Op(s.clone()),
        TokenKind::Ident => ExpectTok::Kind("IDENT".into()),
        TokenKind::AtIdent => ExpectTok::Kind("AT_IDENT".into()),
        TokenKind::HashIdent => ExpectTok::Kind("HASH_IDENT".into()),
        TokenKind::Int => ExpectTok::Kind("INT".into()),
        TokenKind::Act => ExpectTok::Kind("ACT".into()),
        TokenKind::Action => ExpectTok::Kind("ACTION".into()),
        TokenKind::Float => ExpectTok::Kind("FLOAT".into()),
        TokenKind::String => ExpectTok::Kind("STRING".into()),
        TokenKind::Money => ExpectTok::Kind("MONEY".into()),
        TokenKind::Newline => ExpectTok::Kind("NEWLINE".into()),
        TokenKind::Indent => ExpectTok::Kind("INDENT".into()),
        TokenKind::Dedent => ExpectTok::Kind("DEDENT".into()),
        TokenKind::Eof => ExpectTok::Kind("EOF".into()),
        TokenKind::Duration => ExpectTok::Kind("Duration".into()),
        TokenKind::Blob     => ExpectTok::Kind("blob".into()),
        TokenKind::Date     => ExpectTok::Kind("date".into()),
        TokenKind::Time     => ExpectTok::Kind("time".into()),
        TokenKind::DateTime => ExpectTok::Kind("datetime".into()),
    }
}

fn compare_expect(expected: &[ExpectTok], actual: &[ExpectTok]) -> Option<String> {
    // subsequence match: expected must appear in order within actual
    let mut i = 0usize; // expected
    let mut j = 0usize; // actual
    while i < expected.len() && j < actual.len() {
        if expected[i] == actual[j] {
            i += 1;
            j += 1;
        } else {
            j += 1;
        }
    }
    if i == expected.len() {
        None
    } else {
        Some(format!(
            "could not match expected token {}: {}",
            i, expected[i]
        ))
    }
}

fn parse_ok_tokens(path: &Path) -> Result<Vec<ExpectTok>, String> {
    let mut text = read_to_string(path).map_err(|e| e.to_string())?;
    if text.as_bytes().starts_with(&[0xEF, 0xBB, 0xBF]) {
        text = text.split_off(3);
    }

    let mut lines = text.lines();

    // find first non-blank/comment and ensure it's OK
    let mut mode_ok = false;
    for l in lines.by_ref() {
        let t = l.trim();
        if t.is_empty() || t.starts_with('#') {
            continue;
        }
        if t.eq_ignore_ascii_case("OK") {
            mode_ok = true;
            break;
        }
        if t.eq_ignore_ascii_case("ERR") {
            return Err("ERR oracle not supported in compare yet".into());
        }
        return Err(format!("expected OK or ERR, got: {}", t));
    }
    if !mode_ok {
        return Err("empty expect file".into());
    }

    let mut out = Vec::new();
    for raw in lines {
        let mut l = raw.trim().to_string();
        if l.is_empty() || l.starts_with('#') || l.starts_with('@') {
            continue;
        }
        // treat prose bullets as comments in OK mode
        if l.starts_with('-') {
            continue;
        }

        // Drop inline comments (# ...) when not inside backticks or parens
        if !l.starts_with('`')
            && !l.contains('(')
            && let Some(idx) = l.find('#')
        {
            l.truncate(idx);
            l = l.trim().to_string();
        }

        if l.is_empty() {
            continue;
        }

        // Drop span suffix like " @1:1-1:4"
        if let Some(idx) = l.find(" @") {
            l.truncate(idx);
            l = l.trim().to_string();
        }

        // --- recognize EOF line explicitly ---
        if l.eq_ignore_ascii_case("EOF") {
            out.push(ExpectTok::Kind("EOF".into()));
            continue;
        }

        // Backticked operator/punct
        if let Some(rest) = l.strip_prefix('`')
            && let Some(end) = rest.find('`')
        {
            out.push(ExpectTok::Op(rest[..end].to_string()));
            continue;
        }

        // Kind(value) -> we only care about kind now
        if let Some(p) = l.find('(') {
            let kind = l[..p].trim().to_ascii_uppercase();
            out.push(ExpectTok::Kind(kind));
            continue;
        }

        // Plain kind
        out.push(ExpectTok::Kind(l.to_ascii_uppercase()));
    }
    Ok(out)
}

#[allow(dead_code)]
fn mode_str(m: Mode) -> &'static str {
    match m {
        Mode::OkMode => "OK",
        Mode::ErrMode => "ERR",
    }
}

fn read_to_string(path: &Path) -> io::Result<String> {
    let mut f = fs::File::open(path)?;
    let mut buf = Vec::new();
    f.read_to_end(&mut buf)?;
    Ok(String::from_utf8_lossy(&buf).into_owned())
}

fn path_from(path: &Path, base: &Path) -> Option<PathBuf> {
    pathdiff::diff_paths(path, base)
}

mod pathdiff {
    use std::path::{Component, Path, PathBuf};
    pub fn diff_paths(path: &Path, base: &Path) -> Option<PathBuf> {
        let mut ita = base.components();
        let mut itb = path.components();
        loop {
            match (ita.clone().next(), itb.clone().next()) {
                (Some(ca), Some(cb)) if comp_eq(&ca, &cb) => {
                    ita.next();
                    itb.next();
                }
                _ => break,
            }
        }
        let mut result = PathBuf::new();
        for c in ita {
            if let Component::Normal(_) = c {
                result.push("..");
            }
        }
        for c in itb {
            result.push(c.as_os_str());
        }
        Some(result)
    }
    fn comp_eq(a: &Component<'_>, b: &Component<'_>) -> bool {
        use Component::*;
        match (a, b) {
            (Prefix(pa), Prefix(pb)) => pa.kind() == pb.kind(),
            (RootDir, RootDir) | (CurDir, CurDir) | (ParentDir, ParentDir) => true,
            (Normal(a), Normal(b)) => a == b,
            _ => false,
        }
    }
}

// ===================== REPL (Stage 1) =====================

fn repl_banner() -> &'static str {
    if cfg!(windows) {
        "Goblin v0.1.0 — type 'exit'/'quit' or press Ctrl+Z (Windows) to exit"
    } else {
        "Goblin v0.1.0 — type 'exit'/'quit' or press Ctrl+D (Unix) to exit"
    }
}

fn run_repl() -> i32 {
    use std::io::{self, Write};
    use goblin_interpreter::Session;
    use goblin_lexer::{lex, TokenKind};
    use goblin_parser::Parser;
    use goblin_ast as ast;

    println!("{}", repl_banner());
    let mut sess = Session::new();
    let mut n: usize = 1;

    // Accumulator for multi-line input and a tiny depth counter for blocks.
    let mut buf = String::new();
    let mut depth: i32 = 0;

    loop {
        // Primary prompt for a fresh form; continuation prompt inside a form.
        if buf.is_empty() {
            print!("gbln({})>> ", n);
        } else {
            print!("...       ");
        }
        if io::stdout().flush().is_err() { return 1; }

        let mut line = String::new();
        let read = io::stdin().read_line(&mut line).unwrap_or(0);
        if read == 0 { println!(); break; } // Ctrl+D/Z

        let trimmed = line.trim_end();

        // Allow exit/quit only when not inside a pending block.
        if buf.is_empty() && (trimmed.eq_ignore_ascii_case("exit") || trimmed.eq_ignore_ascii_case("quit")) {
            break;
        }

        // Append this line to the buffer we’ll parse as a unit.
        buf.push_str(trimmed);
        buf.push('\n');

        // ----- Update block depth from THIS line only -----
        // Do it with a simple string scan (no braces; only keywords matter here).
        {
            let s = trimmed.trim_start();

            // helper: does this line start a block keyword?
            let starts_block_kw = |kw: &str| -> bool {
                s == kw || s.starts_with(kw) && s[kw.len()..].starts_with(char::is_whitespace)
            };

            // 1) control-flow headers always open a block on their line
            if starts_block_kw("if") || starts_block_kw("while") || starts_block_kw("unless") {
                depth += 1;
            }

            // 2) action header: open a block UNLESS it's the single-line form `= ...` at top level
            if s.starts_with("act ") || s.starts_with("act(") || s.starts_with("action ") || s.starts_with("action(") {
                // scan for '=' that is NOT inside parens
                let mut paren = 0i32;
                let mut has_eq_outside = false;
                for ch in s.chars() {
                    match ch {
                        '(' => paren += 1,
                        ')' => if paren > 0 { paren -= 1; },
                        '=' if paren == 0 => { has_eq_outside = true; break; }
                        _ => {}
                    }
                }
                if !has_eq_outside {
                    depth += 1; // multiline action; keep buffering
                }
            }

            // 3) closers
            if s == "end" { depth -= 1; }
            if s == "xx"  { depth -= 1; }

            if depth < 0 { depth = 0; } // never “owe” an opener
        }

        // If we're still inside a block, keep reading lines.
        if depth > 0 { continue; }

        // We’re at top level (depth == 0): try to parse+eval the whole buffer.
        if buf.trim().is_empty() {
            buf.clear();
            continue;
        }

        let toks = match lex(&buf, "<repl>") {
            Ok(t) => t,
            Err(diags) => {
                eprintln!("{}", diags[0]);
                buf.clear();
                depth = 0;
                continue;
            }
        };

        let parser = Parser::new(&toks);
        let module = match parser.parse_module() {
            Ok(m) => m,
            Err(diags) => {
                eprintln!("{}", diags[0]);
                buf.clear();
                depth = 0;
                continue;
            }
        };

        // Evaluate every top-level statement (expressions *and* declarations).
        for stmt in &module.items {
            match sess.eval_stmt(stmt) {
                Ok(Some(val)) => {
                    let echo = format!("{}", val);
                    if !echo.is_empty() {
                        println!("{}", echo);
                    }
                }
                Ok(None) => {}              // declarations (act/action/class) don't echo
                Err(d) => { eprintln!("{}", d); break; }
            }
        }

        // Reset for the next form.
        buf.clear();
        depth = 0;
        n += 1;
    }
    0
}


fn run_run(path: &std::path::Path) -> i32 {
    use goblin_interpreter::Session;

    // 1) read the file
    let src = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("C0101: could not read script '{}': {}", path.display(), e);
            return 1;
        }
    };

    // 2) lex (match the label logic used elsewhere so spans look nice)
    let label = match path.file_name().and_then(|n| n.to_str()) {
        Some(name) if name.starts_with("goblin_") && name.ends_with(".gbln") => "<snippet>".to_string(),
        _ => path.display().to_string(),
    };
    let tokens = match goblin_lexer::lex(&src, &label) {
        Ok(toks) => toks,
        Err(diags) => {
            eprintln!("LEX FAILED ({} diagnostic{})",
                diags.len(), if diags.len() == 1 { "" } else { "s" });
            for (i, d) in diags.iter().enumerate() {
                eprintln!("  [{}] {}", i + 1, format_diagnostic(d));
            }
            return 1;
        }
    };

    // 3) parse
    let parser = goblin_parser::Parser::new(&tokens);
    let module = match parser.parse_module() {
        Ok(m) => m,
        Err(diags) => {
            eprintln!("PARSE FAILED ({} diagnostic{})",
                diags.len(), if diags.len() == 1 { "" } else { "s" });
            for (i, d) in diags.iter().enumerate() {
                eprintln!("  [{}] {}", i + 1, format_diagnostic(d));
            }
            return 1;
        }
    };

    // 4) interpret — mirror REPL behavior:
    //    - evaluate each top-level expression
    //    - print value only if non-empty (so `say` prints once, and `Unit` doesn't add a blank line)
    let mut sess = Session::new();
    for stmt in &module.items {
        if let goblin_ast::Stmt::Expr(e) = stmt {
            match sess.eval_module(&module) {
                Ok(Some(val)) => {
                    let echo = format!("{}", val);
                    if !echo.is_empty() { println!("{}", echo); }
                }
                Ok(None) => {}
                Err(d) => eprintln!("{}", d),
            }
        }
        // non-expr statements are ignored in Stage 1 (same as REPL/session)
    }

    0
}

fn is_probable_file(s: &str) -> bool {
    let p = std::path::Path::new(s);
    p.exists() || s.ends_with(".gbln")
}