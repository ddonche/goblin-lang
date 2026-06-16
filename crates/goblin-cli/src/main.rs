// ---- version = "0.47.39"
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
use goblin_interpreter;
use std::collections::BTreeMap;
use goblin_interpreter::Value;
use serde_json::json;

pub mod config;

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

#[cfg(windows)]
fn enable_utf8_console() {
    use windows::Win32::System::Console::{SetConsoleCP, SetConsoleOutputCP};
    unsafe {
        // Ignore return values; if the OS refuses, we still run.
        let _ = SetConsoleCP(65001);
        let _ = SetConsoleOutputCP(65001);
    }
}

#[allow(dead_code)]
fn run_devserver(host: String, port: u16) -> i32 {
    // Block on the async devserver using a Tokio runtime
    let rt = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .expect("tokio runtime");

    let result = rt.block_on(async move {
        use goblin_devserver::{start, DevOptions};
        let opts = DevOptions { host, port, proxies: Vec::new() };
        start(opts).await
    });

    match result {
        Ok(()) => 0,
        Err(e) => {
            eprintln!("{}", e);
            1
        }
    }
}

#[allow(dead_code)]
fn read_module_paths_from_yaml(cwd: &Path) -> BTreeMap<String, PathBuf> {
    let mut out = BTreeMap::new();
    let p = cwd.join("goblin.yaml");
    let Ok(text) = std::fs::read_to_string(&p) else { return out; };

    // very small, indentation-based parser for:
    // module_paths:
    //   alias: ./dir
    let mut in_section = false;
    for raw in text.lines() {
        let line = raw.trim_end();
        let trimmed = line.trim_start();
        if trimmed.is_empty() || trimmed.starts_with('#') { continue; }

        if !in_section {
            if trimmed.starts_with("module_paths:") {
                in_section = true;
            }
            continue;
        }

        // we’re inside module_paths: — accept only indented "key: value"
        let leading_ws = line.len() - trimmed.len();
        if leading_ws == 0 {
            // section ended
            break;
        }

        // ignore comments-only lines
        if trimmed.starts_with('#') { continue; }

        // parse "key: value"
        if let Some((k, v)) = trimmed.split_once(':') {
            let key = k.trim();
            if key.is_empty() { continue; }
            let mut val = v.trim();

            // strip optional quotes
            if (val.starts_with('"') && val.ends_with('"')) || (val.starts_with('\'') && val.ends_with('\'')) {
                val = &val[1..val.len().saturating_sub(1)];
            }

            if !val.is_empty() {
                out.insert(key.to_string(), cwd.join(val));
            }
        }
    }
    out
}

fn main() {
    #[cfg(windows)]
        enable_utf8_console();

    let mut args = env::args().skip(1).collect::<Vec<_>>();

    // --version / -v
    if args.len() == 1 && (args[0] == "--version" || args[0] == "-v") {
        println!("Goblin v{}", env!("CARGO_PKG_VERSION"));
        return;
    }

    // --help / -h
    if args.len() == 1 && (args[0] == "--help" || args[0] == "-h") {
        println!(
            "Goblin Language CLI\n\
             usage:\n\
             \n  goblin new <project-name>\n\
             \n  goblin run [<file>]\n\
             \n  goblin repl\n\
             \n  goblin glam run <glam_name>::<action_name> [--test]\n\
             \n  goblin box dump [<dir>]\n\
             \n  goblin start [--host <host>] [--port <port>]\n\
             \n  goblin lex --check\n\
             \n  goblin parse <file>\n\
             \n  goblin gql-parse <file|->\n\
             \nOptions:\n  -h, --help       Show this help\n  -v, --version    Show version\n  --vm             Use the VM engine (run/repl) instead of the interpreter"
        );
        return;
    }

    // Handle 'goblin new <project-name>'
    if !args.is_empty() && args[0] == "new" {
        if args.len() < 2 {
            eprintln!("Usage: goblin new <project-name>");
            std::process::exit(1);
        }
        create_project(&args[1]);
        return;
    }

    // `goblin start [--host H] [--port P] [--proxy /pfx=URL]...`
    if !args.is_empty() && args[0] == "start" {
        args.remove(0);

        let mut host = String::from("0.0.0.0");
        let mut port: u16 = 5173;
        let mut proxies: Vec<(String, String)> = Vec::new();

        let mut i = 0;
        while i < args.len() {
            match args[i].as_str() {
                "--host" => {
                    if i + 1 >= args.len() { eprintln!("usage: goblin start [--host <host>] [--port <port>] [--proxy /pfx=URL]..."); std::process::exit(2); }
                    host = args[i + 1].clone(); i += 2;
                }
                "--port" | "-p" => {
                    if i + 1 >= args.len() { eprintln!("usage: goblin start [--host <host>] [--port <port>] [--proxy /pfx=URL]..."); std::process::exit(2); }
                    port = args[i + 1].parse().unwrap_or_else(|_| { eprintln!("invalid port: {}", args[i + 1]); std::process::exit(2); });
                    i += 2;
                }
                "--proxy" => {
                    if i + 1 >= args.len() { eprintln!("usage: goblin start --proxy /prefix=URL"); std::process::exit(2); }
                    let spec = &args[i + 1];
                    if let Some((prefix, url)) = spec.split_once('=') {
                        let pfx = prefix.trim().to_string();
                        let url = url.trim().to_string();
                        if !pfx.starts_with('/') { eprintln!("proxy prefix must start with '/': {}", pfx); std::process::exit(2); }
                        if !(url.starts_with("http://") || url.starts_with("https://")) { eprintln!("proxy URL must start with http:// or https://: {}", url); std::process::exit(2); }
                        proxies.push((pfx, url));
                    } else {
                        eprintln!("invalid --proxy spec (expected /prefix=URL): {}", spec);
                        std::process::exit(2);
                    }
                    i += 2;
                }
                other => {
                    eprintln!("unknown start option: {}", other);
                    eprintln!("usage: goblin start [--host <host>] [--port <port>] [--proxy /pfx=URL]...");
                    std::process::exit(2);
                }
            }
        }

        std::process::exit(run_devserver_with_proxies(host, port, proxies));
    }

    // Detect --vm flag or GOBLIN_ENGINE=vm env var anywhere in args.
    let use_vm = std::env::var("GOBLIN_ENGINE").unwrap_or_default() == "vm"
        || args.iter().any(|a| a == "--vm");
    // Strip --vm from args so subcommand parsers don't see it.
    args.retain(|a| a != "--vm");

    // REPL when no args
    if args.is_empty() {
        if use_vm { std::process::exit(run_repl_vm()); }
        std::process::exit(run_repl());
    }

    // `goblin-cli repl`
    if args.len() == 1 && args[0] == "repl" {
        if use_vm { std::process::exit(run_repl_vm()); }
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

    // `goblin glam run <glam_name>::<action_name> [--test]`
    if args.len() >= 2 && args[0] == "glam" && args[1] == "run" {
        if args.len() < 3 {
            eprintln!("usage: goblin glam run <glam_name>::<action_name> [--test]");
            std::process::exit(2);
        }
        let spec = args[2].clone();
        let rest = &args[3..];
        let test_mode = rest.iter().any(|a| a == "--test");
        if rest.iter().any(|a| a != "--test") {
            eprintln!("usage: goblin glam run <glam_name>::<action_name> [--test]");
            std::process::exit(2);
        }
        std::process::exit(run_glam_run(&spec, test_mode));
    }

    if args.len() >= 2 && args[0] == "box" && args[1] == "dump" {
        let dir = if args.len() >= 3 {
            PathBuf::from(&args[2])
        } else {
            env::current_dir().unwrap_or_else(|_| PathBuf::from("."))
        };

        let mut lines: Vec<String> = Vec::new();

        // 1) Static project box.toml
        let mut sess = goblin_interpreter::Session::new();
        let box_toml = dir.join("box.toml");
        if box_toml.exists() {
            if let Err(e) = goblin_interpreter::load_box_toml(&mut sess, &box_toml) {
                eprintln!("box.toml error: {}", e);
                std::process::exit(1);
            }
            for (k, v) in &sess.box_store {
                let display = if let goblin_interpreter::Value::Str(s) = v {
                    if s.contains("{#") {
                        format!("#{} = {} (contains runtime variable)", k, s)
                    } else {
                        format!("#{} = {}", k, v)
                    }
                } else {
                    format!("#{} = {}", k, v)
                };
                lines.push(display);
            }
        }

        // 2) Walk glams/ — read each glam.toml and show declared provides
        let glams_dir = dir.join("glams");
        if glams_dir.is_dir() {
            if let Ok(entries) = std::fs::read_dir(&glams_dir) {
                let mut glam_entries: Vec<_> = entries.filter_map(|e| e.ok()).collect();
                glam_entries.sort_by_key(|e| e.file_name());
                for entry in glam_entries {
                    let glam_dir = entry.path();
                    if !glam_dir.is_dir() { continue; }
                    let namespace = glam_dir.file_name()
                        .and_then(|n| n.to_str())
                        .unwrap_or("")
                        .to_string();
                    let glam_toml = glam_dir.join("glam.toml");
                    if !glam_toml.exists() { continue; }
                    let mut glam_sess = goblin_interpreter::Session::new();
                    // copy box_store so [needs] can resolve from project box
                    glam_sess.box_store = sess.box_store.clone();
                    if let Err(e) = goblin_interpreter::load_glam_box_toml(
                        &mut glam_sess, &glam_toml, Some(&namespace)
                    ) {
                        eprintln!("warning: {}: {}", glam_toml.display(), e);
                    }
                    for (k, v) in &glam_sess.box_store {
                        if k.starts_with(&format!("{}::", namespace)) {
                            let display = if let goblin_interpreter::Value::Str(s) = v {
                                if s.contains("{#") {
                                    format!("#{} = {} (contains runtime variable)", k, s)
                                } else {
                                    format!("#{} = {}", k, v)
                                }
                            } else {
                                format!("#{} = {}", k, v)
                            };
                            lines.push(display);
                        }
                    }
                }
            }
        }

        lines.sort();
        if lines.is_empty() {
            println!("(box is empty)");
        } else {
            for line in lines {
                println!("{}", line);
            }
        }
        std::process::exit(0);
    }

    // NEW: `goblin-cli run [<file>]`
    if !args.is_empty() && args[0] == "run" {
        args.remove(0);

        // If a file was provided, use it; otherwise resolve from goblin.yaml
        let target: PathBuf = if let Some(path) = args.get(0) {
            PathBuf::from(path)
        } else {
            match resolve_entry_from_yaml(&env::current_dir().unwrap_or_else(|_| PathBuf::from("."))) {
                Some(p) => p,
                None => {
                    eprintln!(
                        "No entry file provided and could not find `entry:` in goblin.yaml.\n\
                         usage: goblin-cli run <file>\n       (or add `entry: main.gbln` to goblin.yaml and run `goblin run`)"
                    );
                    std::process::exit(2);
                }
            }
        };

        // Capture anything after the filename as extra args
        let extra_args: Vec<String> = args.iter().skip(1).cloned().collect();
        if use_vm {
            std::process::exit(run_run_vm(target.as_path()));
        }
        std::process::exit(run_run_with_args(target.as_path(), extra_args));
    }

    // Run script file if a single path argument is provided
    if args.len() == 1 && is_probable_file(&args[0]) {
        if use_vm {
            std::process::exit(run_run_vm(Path::new(&args[0])));
        }
        std::process::exit(run_run(Path::new(&args[0])));
    }

    eprintln!(
        "usage:\n  goblin new <project-name>\n  goblin run [<file>]\n  goblin repl\n  goblin glam run <glam_name>::<action_name> [--test]\n  goblin box dump [<dir>]\n  goblin start [--host <host>] [--port <port>]\n  goblin lex --check\n  goblin parse <file>\n  goblin gql-parse <file|->"
    );
    std::process::exit(2);
}

fn create_project(name: &str) {
    let path = Path::new(name);
    
    if path.exists() {
        eprintln!("Error: Directory '{}' already exists", name);
        std::process::exit(1);
    }
    
    fs::create_dir(path).expect("Failed to create project directory");
    
    fs::write(path.join("goblin.yaml"), goblin_yaml(name))
        .expect("Failed to create goblin.yaml");
    
    fs::write(path.join("main.gob"), MAIN_GBLN)
        .expect("Failed to create main.gob");
    
    fs::write(path.join("README.md"), readme_md(name))
        .expect("Failed to create README.md");
    
    println!("Created new Goblin project: {}", name);
    println!("  cd {}", name);
    println!("  goblin main.gob");
}

fn goblin_yaml(name: &str) -> String {
    format!(r#"name: {}
version: 0.1.0
entry: main.gob

# Define module paths for your project
# Format: alias: ./path/to/folder
module_paths:
  # game: ./game
  # data: ./data
  # utils: ./utils
"#, name)
}

const MAIN_GBLN: &str = r#"/// Main entry point
/// Example imports (uncomment when you create modules):
/// import game/hero
/// import data/weapons

say "Welcome to the Horde!"
"#;

fn readme_md(name: &str) -> String {
    format!(r#"# {}

A Goblin project.

## Running
Run your main file:
    goblin main.gob

## Project Structure
Organize your code into modules by:
1. Adding folders (e.g., game/, data/)
2. Registering them in goblin.yaml under module_paths
3. Creating .gob files in those folders
4. Importing them: import game/hero
"#, name)
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
        Some(name) if name.starts_with("goblin_") && (name.ends_with(".gob") || name.ends_with(".gbln")) => "<snippet>".to_string(),
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
        if matches!(p.extension(), Some(ext) if ext == OsStr::new("gob") || ext == OsStr::new("gbln")) {
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
        TokenKind::HashIdent => ExpectTok::Kind("HASH_IDENT".into()),
        TokenKind::ClassIdent => ExpectTok::Kind("CLASS_IDENT".into()),
        TokenKind::Int => ExpectTok::Kind("INT".into()),
        TokenKind::Act => ExpectTok::Kind("ACT".into()),
        TokenKind::Action => ExpectTok::Kind("ACTION".into()),
        TokenKind::Import => ExpectTok::Kind("IMPORT".into()),
        TokenKind::Use    => ExpectTok::Kind("USE".into()),
        TokenKind::Export => ExpectTok::Kind("EXPORT".into()),
        TokenKind::Vault => ExpectTok::Kind("VAULT".into()),
        TokenKind::Float => ExpectTok::Kind("FLOAT".into()),
        TokenKind::String => ExpectTok::Kind("STRING".into()),
        TokenKind::Char     => ExpectTok::Kind("char".into()),
        TokenKind::Shadow => ExpectTok::Op("[=".into()),
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
        TokenKind::TripleBraceOpen  => ExpectTok::Kind("{{{".into()),
        TokenKind::TripleBraceClose => ExpectTok::Kind("}}}".into()),
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
        concat!(
            "Goblin v",
            env!("CARGO_PKG_VERSION"),
            " — type 'exit'/'quit' or press Ctrl+Z (Windows) to exit"
        )
    } else {
        concat!(
            "Goblin v",
            env!("CARGO_PKG_VERSION"),
            " — type 'exit'/'quit' or press Ctrl+D (Unix) to exit"
        )
    }
}

// ── VM execution entry points ────────────────────────────────────────────────

fn vm_error_to_diagnostic(
    e: &goblin_vm::error::GoblinError,
    filepath: &str,
    src: &str,
) -> goblin_diagnostics::Diagnostic {
    use goblin_diagnostics::{Diagnostic, Severity, Span};
    use goblin_vm::error::GoblinError;

    // Peel off WithLocation wrappers to get the line number and inner message.
    fn peel(e: &GoblinError) -> (&GoblinError, u32) {
        match e {
            GoblinError::WithLocation { inner, line } => {
                let (inner2, inner_line) = peel(inner);
                (inner2, if inner_line > 0 { inner_line } else { *line })
            }
            other => (other, 0),
        }
    }
    let (inner, line) = peel(e);
    let message = inner.to_string();

    // Find byte offset of the start of the given 1-based line.
    let (start_byte, end_byte) = {
        let mut off = 0usize;
        let mut found = (0usize, 0usize);
        for (i, ln) in src.split('\n').enumerate() {
            if i + 1 == line as usize {
                found = (off, off + ln.len());
                break;
            }
            off += ln.len() + 1;
        }
        found
    };

    let lineno = line.max(1);
    let span = Span::new(filepath, start_byte, end_byte, lineno, 1, lineno, 1);
    Diagnostic::new_with_code(Severity::Error, "VM", "runtime-error", &message, span)
}

fn run_run_vm(path: &std::path::Path) -> i32 {
    use std::time::Instant;

    let src = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("C0101: could not read script '{}': {}", path.display(), e);
            return 1;
        }
    };

    let filepath = path.display().to_string();
    let start = Instant::now();
    let result = goblin_vm::exec::execute_source(&src);
    let elapsed = start.elapsed();

    let code = match result {
        Ok(_) => 0,
        Err(e) => {
            let diag = vm_error_to_diagnostic(&e, &filepath, &src);
            eprintln!("{}", diag);
            1
        }
    };

    eprintln!(
        "goblin run --vm {} → exit {} in {}ms ({}.{:03}s)",
        path.display(), code,
        elapsed.as_millis(), elapsed.as_secs(), elapsed.subsec_millis(),
    );
    code
}

fn run_repl_vm() -> i32 {
    use std::io::{self, Write};
    use goblin_lexer::lex;
    use goblin_parser::Parser;
    use goblin_vm::session::{GcMode, Session};
    use goblin_vm::vm::Vm;
    use goblin_vm::compiler::compile_repl_snippet;
    use goblin_vm::exec::compile_class_methods_pub;
    use goblin_ast as ast;

    println!("{}", repl_banner());
    println!("  [VM mode — engine: goblin-vm]");
    println!();

    std::thread::Builder::new()
        .stack_size(8 * 1024 * 1024)
        .spawn(|| {
            let session = Session::new(GcMode::Auto);
            let mut vm = Vm::new(session);
            let mut known_globals: Vec<String> = Vec::new();
            let mut form_no: usize = 1;
            let mut buf = String::new();
            let mut depth: i32 = 0;

            loop {
                if buf.is_empty() {
                    print!("gbln-vm({}): ", form_no);
                } else {
                    print!("...         ");
                }
                if io::stdout().flush().is_err() { return 1; }

                let mut line = String::new();
                let read = io::stdin().read_line(&mut line).unwrap_or(0);
                if read == 0 { println!(); break; }

                let trimmed = line.trim_end();

                if buf.is_empty()
                    && (trimmed.eq_ignore_ascii_case("exit") || trimmed.eq_ignore_ascii_case("quit"))
                {
                    break;
                }

                buf.push_str(trimmed);
                buf.push('\n');

                // Track block depth
                {
                    let src_line = trimmed.trim_start();
                    let starts_block = |kw: &str| -> bool {
                        src_line == kw || (src_line.starts_with(kw) && src_line[kw.len()..].starts_with(char::is_whitespace))
                    };
                    if (starts_block("if") || starts_block("unless") || starts_block("while")
                        || starts_block("for") || starts_block("repeat") || starts_block("attempt")
                        || starts_block("judge") || starts_block("judge_all"))
                        && !src_line.contains("=>")
                    {
                        depth += 1;
                    }
                    if src_line.starts_with("act ") || src_line.starts_with("act(") {
                        depth += 1;
                    }
                    if src_line == "end" || src_line == "xx" { depth -= 1; }
                    if depth < 0 { depth = 0; }
                }

                if depth > 0 { continue; }

                let snippet = buf.trim_end().to_string();
                buf.clear();
                depth = 0;

                if snippet.is_empty() { form_no += 1; continue; }

                // Lex
                let tokens = match lex(&snippet, "<repl>") {
                    Ok(t) => t,
                    Err(diags) => {
                        if let Some(d) = diags.into_iter().next() { eprintln!("{d}"); }
                        form_no += 1;
                        continue;
                    }
                };

                // Parse
                let module = match Parser::new(&tokens).parse_module() {
                    Ok(m) => m,
                    Err(diags) => {
                        if let Some(d) = diags.into_iter().next() { eprintln!("{d}"); }
                        form_no += 1;
                        continue;
                    }
                };

                // Execute each statement individually — mirrors interpreter REPL exactly.
                let mut had_error = false;
                for stmt in &module.items {
                    let is_expr = matches!(stmt, ast::Stmt::Expr(_));

                    // Compile this single statement with current globals context.
                    let single = goblin_ast::Module { items: vec![stmt.clone()] };
                    let compiled = match compile_repl_snippet(&single, &known_globals) {
                        Ok(c) => c,
                        Err(e) => {
                            eprintln!("error: {e}");
                            had_error = true;
                            break;
                        }
                    };

                    // Register new classes/enums into the VM session.
                    for decl in &compiled.classes {
                        compile_class_methods_pub(decl, &mut vm.session);
                        vm.session.classes.insert(decl.name.clone(), decl.clone());
                    }
                    for decl in compiled.enums {
                        vm.session.enums.insert(decl.name.clone(), decl);
                    }

                    // Extend known globals with any new names declared by this statement.
                    for name in &compiled.global_names {
                        if !known_globals.contains(name) {
                            known_globals.push(name.clone());
                        }
                    }
                    vm.session.global_names = known_globals.clone();

                    // Execute the single-statement function.
                    match vm.execute_repl(compiled.entry, known_globals.len()) {
                        Ok(val) => {
                            if is_expr {
                                use goblin_vm::value::Value;
                                match &val {
                                    Value::Nil | Value::Unit => {}
                                    _ => {
                                        let s = goblin_vm::builtins::value_to_str(&val);
                                        if !s.is_empty() {
                                            println!("{s}");
                                        }
                                    }
                                }
                            }
                        }
                        Err(e) => {
                            eprintln!("error: {e}");
                            had_error = true;
                            break;
                        }
                    }
                }
                let _ = had_error;

                form_no += 1;
            }
            0
        })
        .unwrap()
        .join()
        .unwrap()
}


fn run_repl() -> i32 {
    use std::io::{self, Write};
    use goblin_interpreter::Session;
    use goblin_parser::Parser;
    use goblin_ast as ast;
    use goblin_lexer::lex;
 
    println!("{}", repl_banner());
 
    // Run REPL in a thread with 8MB stack (Windows default is only 1MB)
    std::thread::Builder::new()
        .stack_size(8 * 1024 * 1024)
        .spawn(|| {
            // --- helper: parse + dump AST (no eval) ---
            fn repl_dump_ast(src: &str) {
                let tokens = match lex(src, "<repl>") {
                    Ok(t) => t,
                    Err(diags) => {
                        if let Some(d) = diags.into_iter().next() {
                            eprintln!("{d}");
                        }
                        return;
                    }
                };
 
                let module = match Parser::new(&tokens).parse_module() {
                    Ok(m) => m,
                    Err(diags) => {
                        if let Some(d) = diags.into_iter().next() {
                            eprintln!("{d}");
                        }
                        return;
                    }
                };
 
                for (i, stmt) in module.items.iter().enumerate() {
                    println!("[{i}] {stmt:#?}");
                }
            }
 
            // --- helper: load + eval a file into the session ---
            fn repl_load_file(path: &str, sess: &mut Session) {
                let src = match std::fs::read_to_string(path) {
                    Ok(s) => s,
                    Err(e) => {
                        eprintln!("error: could not read '{}': {}", path, e);
                        return;
                    }
                };
 
                let tokens = match lex(&src, path) {
                    Ok(t) => t,
                    Err(diags) => {
                        if let Some(d) = diags.into_iter().next() {
                            eprintln!("{d}");
                        }
                        return;
                    }
                };
 
                let module = match Parser::new(&tokens).parse_module() {
                    Ok(m) => m,
                    Err(diags) => {
                        if let Some(d) = diags.into_iter().next() {
                            eprintln!("{d}");
                        }
                        return;
                    }
                };
 
                for stmt in &module.items {
                    let result = match stmt {
                        ast::Stmt::Expr(e) => sess.eval_expr(e).map(|val| {
                            let echo = format!("{val}");
                            if !echo.is_empty() {
                                println!("{echo}");
                            }
                        }),
                        _ => sess.eval_stmt(stmt).map(|_| ()),
                    };
 
                    if let Err(d) = result {
                        eprintln!("{d}");
                        return;
                    }
                }
 
                println!("loaded: {}", path);
            }
 
            let mut sess = Session::new();
            let mut form_no: usize = 1;
 
            let mut buf = String::new();
            let mut depth: i32 = 0;
 
            loop {
                if buf.is_empty() {
                    print!("gbln({}): ", form_no);
                } else {
                    print!("...       ");
                }
                if io::stdout().flush().is_err() {
                    return 1;
                }
 
                let mut line = String::new();
                let read = io::stdin().read_line(&mut line).unwrap_or(0);
                if read == 0 {
                    println!();
                    break;
                }
 
                let trimmed = line.trim_end();
 
                // exit/quit
                if buf.is_empty()
                    && (trimmed.eq_ignore_ascii_case("exit") || trimmed.eq_ignore_ascii_case("quit"))
                {
                    break;
                }
 
                // :load <path>
                if buf.is_empty() {
                    let s0 = trimmed.trim_start();
                    if let Some(rest) = s0.strip_prefix(":load") {
                        let path = rest.trim();
                        if path.is_empty() {
                            println!("usage: :load <path>");
                        } else {
                            repl_load_file(path, &mut sess);
                        }
                        form_no += 1;
                        continue;
                    }
                }
 
                // :ast <code>
                if buf.is_empty() {
                    let s0 = trimmed.trim_start();
                    if let Some(rest) = s0.strip_prefix(":ast") {
                        let payload = rest.trim();
                        if payload.is_empty() {
                            println!("usage: :ast <goblin code>");
                        } else {
                            repl_dump_ast(payload);
                        }
                        buf.clear();
                        depth = 0;
                        form_no += 1;
                        continue;
                    }
                }
 
                // enhance easter egg
                if buf.is_empty() {
                    let s0 = trimmed.trim_start();
                    if let Some(rest) = s0.strip_prefix("enhance") {
                        if rest.is_empty() || rest.starts_with(char::is_whitespace) {
                            let payload = rest.trim();
                            if payload.is_empty() {
                                println!("Enhance what?");
                            } else {
                                println!("{payload} has been enhanced.");
                            }
                            form_no += 1;
                            continue;
                        }
                    }
                }
 
                // Accumulate
                buf.push_str(trimmed);
                buf.push('\n');
 
                // Update block depth
                {
                    let src_line = trimmed.trim_start();
 
                    let starts_block_kw = |kw: &str| -> bool {
                        src_line == kw
                            || (src_line.starts_with(kw)
                                && src_line[kw.len()..].starts_with(char::is_whitespace))
                    };
 
                    if starts_block_kw("if")
                        || starts_block_kw("unless")
                        || starts_block_kw("while")
                        || starts_block_kw("for")
                        || starts_block_kw("repeat")
                        || starts_block_kw("attempt")
                        || starts_block_kw("judge")
                        || starts_block_kw("judge_all")
                    {
                        if !src_line.contains("=>") {
                            depth += 1;
                        }
                    }
 
                    if src_line.starts_with("act ")
                        || src_line.starts_with("act(")
                        || src_line.starts_with("action ")
                        || src_line.starts_with("action(")
                    {
                        let mut paren_depth = 0i32;
                        let mut has_eq_outside_parens = false;
                        for ch in src_line.chars() {
                            match ch {
                                '(' => paren_depth += 1,
                                ')' => {
                                    if paren_depth > 0 {
                                        paren_depth -= 1;
                                    }
                                }
                                '=' if paren_depth == 0 => {
                                    has_eq_outside_parens = true;
                                    break;
                                }
                                _ => {}
                            }
                        }
                        if !has_eq_outside_parens {
                            depth += 1;
                        }
                    }
 
                    if src_line == "end" { depth -= 1; }
                    if src_line == "xx"  { depth -= 1; }
                    if depth < 0 { depth = 0; }
                }
 
                if depth > 0 {
                    continue;
                }
 
                if buf.trim().is_empty() {
                    buf.clear();
                    continue;
                }
 
                // -------- LEX --------
                let tokens = match lex(&buf, "<repl>") {
                    Ok(t) => t,
                    Err(diags) => {
                        if let Some(d) = diags.into_iter().next() {
                            eprintln!("{d}");
                        }
                        buf.clear();
                        depth = 0;
                        form_no += 1;
                        continue;
                    }
                };
 
                // -------- PARSE --------
                let module = match Parser::new(&tokens).parse_module() {
                    Ok(m) => m,
                    Err(diags) => {
                        if let Some(d) = diags.into_iter().next() {
                            eprintln!("{d}");
                        }
                        buf.clear();
                        depth = 0;
                        form_no += 1;
                        continue;
                    }
                };
 
                // -------- EVAL --------
                let mut had_error = false;
 
                for stmt in &module.items {
                    let result = match stmt {
                        ast::Stmt::Expr(e) => sess.eval_expr(e).map(|val| {
                            let echo = format!("{val}");
                            if !echo.is_empty() {
                                println!("{echo}");
                            }
                        }),
                        _ => sess.eval_stmt(stmt).map(|_| ()),
                    };
 
                    if let Err(d) = result {
                        eprintln!("{d}");
                        had_error = true;
                        break;
                    }
                }
 
                buf.clear();
                depth = 0;
                form_no += 1;
 
                if had_error {
                    // proceed to next prompt
                }
            }
 
            0
        })
        .unwrap()
        .join()
        .unwrap()
}

fn run_run(path: &std::path::Path) -> i32 {
    use goblin_interpreter::{Session, Value};
    use std::time::Instant;
    
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
        Some(name) if name.starts_with("goblin_") && (name.ends_with(".gob") || name.ends_with(".gbln")) => "<snippet>".to_string(),
        _ => path.display().to_string(),
    };
    let tokens = match goblin_lexer::lex(&src, &label) {
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
    
    // 3) parse
    let parser = goblin_parser::Parser::new(&tokens);
    let module = match parser.parse_module() {
        Ok(m) => m,
        Err(diags) => {
            eprintln!(
                "PARSE FAILED ({} diagnostic{})",
                diags.len(),
                if diags.len() == 1 { "" } else { "s" }
            );
            for (i, d) in diags.iter().enumerate() {
                eprintln!("  [{}] {}", i + 1, format_diagnostic(d));
            }
            return 1;
        }
    };

    // ---- timing starts here (after successful lex + parse) ----
    let start = Instant::now();
    let box_toml_path: Option<PathBuf> = path.parent().map(|p| p.join("box.toml"));

    // 4) interpret with larger stack (8MB instead of default 1MB on Windows)
    let code = std::thread::Builder::new()
        .stack_size(8 * 1024 * 1024)
        .spawn(move || {
            let mut sess = Session::new();
            if let Some(ref box_toml) = box_toml_path {
                if box_toml.exists() {
                    if let Err(e) = goblin_interpreter::load_box_toml(&mut sess, box_toml) {
                        eprintln!("box.toml error: {}", e);
                        return 1;
                    }
                }
            }

            for stmt in &module.items {
                match stmt {
                    goblin_ast::Stmt::Expr(e) => {
                        match sess.eval_expr(e) {
                            Ok(val) => {
                                let is_api = std::env::var("GOBLIN_NONINTERACTIVE").unwrap_or_default() == "1";
                                if is_api && !matches!(val, Value::Unit) {
                                    let echo = format!("{}", val);
                                    if !echo.is_empty() && echo != "nil" {
                                        let status = sess.response.status.unwrap_or(200);
                                        let mut headers_obj = serde_json::Map::new();
                                        for (k, v) in &sess.response.headers {
                                            headers_obj.insert(k.clone(), serde_json::Value::String(v.clone()));
                                        }
                                        let headers_json = serde_json::Value::Object(headers_obj);
                                        let cookies_json = serde_json::Value::Array(
                                            sess.response.cookies.iter()
                                                .map(|c| serde_json::Value::String(c.clone()))
                                                .collect()
                                        );
                                        let envelope = json!({
                                            "status": status,
                                            "headers": headers_json,
                                            "cookies": cookies_json,
                                            "body": echo
                                        });
                                        println!("{}", envelope.to_string());
                                    }
                                }
                            }
                            Err(d) => {
                                eprintln!("{}", d);
                                return 1;
                            }
                        }
                    }
                    _ => {
                        if let Err(d) = sess.eval_stmt(stmt) {
                            eprintln!("{}", d);
                            return 1;
                        }
                    }
                }
            }
            0
        })
        .unwrap()
        .join()
        .unwrap();

    let elapsed = start.elapsed();
    eprintln!(
        "goblin run {} → exit {} in {}ms ({}.{:03}s)",
        path.display(),
        code,
        elapsed.as_millis(),
        elapsed.as_secs(),
        elapsed.subsec_millis(),
    );

    code
}

fn run_run_with_args(path: &std::path::Path, extra_args: Vec<String>) -> i32 {
    use goblin_interpreter::{Session, Value};
    use std::time::Instant;

    // 1) read the file
    let src = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("C0101: could not read script '{}': {}", path.display(), e);
            return 1;
        }
    };

    // 2) lex
    let label = match path.file_name().and_then(|n| n.to_str()) {
        Some(name) if name.starts_with("goblin_") && (name.ends_with(".gob") || name.ends_with(".gbln")) => "<snippet>".to_string(),
        _ => path.display().to_string(),
    };
    let tokens = match goblin_lexer::lex(&src, &label) {
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

    // 3) parse
    let parser = goblin_parser::Parser::new(&tokens);
    let module = match parser.parse_module() {
        Ok(m) => m,
        Err(diags) => {
            eprintln!(
                "PARSE FAILED ({} diagnostic{})",
                diags.len(),
                if diags.len() == 1 { "" } else { "s" }
            );
            for (i, d) in diags.iter().enumerate() {
                eprintln!("  [{}] {}", i + 1, format_diagnostic(d));
            }
            return 1;
        }
    };

    // ---- timing starts here (after successful lex + parse) ----
    let start = Instant::now();
    let box_toml_path: Option<PathBuf> = path.parent().map(|p| p.join("box.toml"));

    // 4) interpret with larger stack (8MB instead of default 1MB on Windows)
    let code = std::thread::Builder::new()
        .stack_size(8 * 1024 * 1024)
        .spawn(move || {
            let mut sess = Session::new();
            if let Some(ref box_toml) = box_toml_path {
                if box_toml.exists() {
                    if let Err(e) = goblin_interpreter::load_box_toml(&mut sess, box_toml) {
                        eprintln!("box.toml error: {}", e);
                        return 1;
                    }
                }
            }

            // Inject CLI args as global `args`
            let arr: Vec<Value> = extra_args.into_iter().map(Value::Str).collect();
            sess.set_global("args", Value::Array(arr));

            for stmt in &module.items {
                match stmt {
                    goblin_ast::Stmt::Expr(e) => match sess.eval_expr(e) {
                        Ok(val) => {
                            let is_api = std::env::var("GOBLIN_NONINTERACTIVE").unwrap_or_default() == "1";
                            if is_api && !matches!(val, Value::Unit) {
                                let echo = format!("{}", val);
                                if !echo.is_empty() && echo != "nil" {
                                    let status = sess.response.status.unwrap_or(200);
                                    let mut headers_obj = serde_json::Map::new();
                                    for (k, v) in &sess.response.headers {
                                        headers_obj.insert(k.clone(), serde_json::Value::String(v.clone()));
                                    }
                                    let headers_json = serde_json::Value::Object(headers_obj);
                                    let cookies_json = serde_json::Value::Array(
                                        sess.response.cookies.iter()
                                            .map(|c| serde_json::Value::String(c.clone()))
                                            .collect()
                                    );
                                    let envelope = json!({
                                        "status": status,
                                        "headers": headers_json,
                                        "cookies": cookies_json,
                                        "body": echo
                                    });
                                    println!("{}", envelope.to_string());
                                }
                            }
                        }
                        Err(d) => {
                            eprintln!("{}", d);
                            return 1;
                        }
                    },

                    _ => {
                        if let Err(d) = sess.eval_stmt(stmt) {
                            eprintln!("{}", d);
                            return 1;
                        }
                    }
                }
            }
            0
        })
        .unwrap()
        .join()
        .unwrap();

    let elapsed = start.elapsed();
    eprintln!(
        "goblin run {} → exit {} in {}ms ({}.{:03}s)",
        path.display(),
        code,
        elapsed.as_millis(),
        elapsed.as_secs(),
        elapsed.subsec_millis(),
    );

    code
}

// `goblin glam run <glam_name>::<action_name> [--test]` — load a GLAM directly
// (without a driver script's `use` statement) and invoke one of its actions,
// establishing the same GLAM execution context that a qualified
// `namespace::action(...)` call would. With `--test`, also loads (from glams/<ns>/)
// every provider GLAM referenced by the target's own [needs.actions], so `:need()`
// can dispatch to its configured local provider without a full app project.
fn run_glam_run(spec: &str, test_mode: bool) -> i32 {
    use goblin_interpreter::Session;

    let (glam_name, action_name) = match spec.split_once("::") {
        Some((g, a)) if !g.is_empty() && !a.is_empty() => (g, a),
        _ => {
            eprintln!("usage: goblin glam run <glam_name>::<action_name> [--test]");
            return 2;
        }
    };
    let glam_name = glam_name.to_string();
    let action_name = action_name.to_string();

    let cwd = env::current_dir().unwrap_or_else(|_| PathBuf::from("."));

    std::thread::Builder::new()
        .stack_size(8 * 1024 * 1024)
        .spawn(move || {
            let mut sess = Session::new();

            // Load the project's own box.toml first so the GLAM's [needs.values]
            // (which reference already-bound `#ns::var` box values) can resolve.
            let box_toml = cwd.join("box.toml");
            if box_toml.exists() {
                if let Err(e) = goblin_interpreter::load_box_toml(&mut sess, &box_toml) {
                    eprintln!("box.toml error: {}", e);
                    return 1;
                }
            }

            match goblin_interpreter::run_glam_action(&mut sess, &glam_name, &action_name, Vec::new(), test_mode) {
                Ok(val) => {
                    println!("{}", val);
                    0
                }
                Err(d) => {
                    eprintln!("{}", d);
                    1
                }
            }
        })
        .unwrap()
        .join()
        .unwrap()
}

fn is_probable_file(s: &str) -> bool {
    let p = std::path::Path::new(s);
    p.exists() || s.ends_with(".gob") || s.ends_with(".gbln")
}

fn resolve_entry_from_yaml(cwd: &std::path::Path) -> Option<std::path::PathBuf> {
    let p = cwd.join("goblin.yaml");
    let text = std::fs::read_to_string(&p).ok()?;
    for line in text.lines() {
        let t = line.trim();
        if t.starts_with("entry:") {
            if let Some(rest) = t.splitn(2, ':').nth(1) {
                let val = rest.trim().trim_matches('"').trim_matches('\'');
                if !val.is_empty() {
                    return Some(cwd.join(val));
                }
            }
        }
    }
    None
}

// =======================================================
// Devserver launcher with proxy support
// =======================================================
fn run_devserver_with_proxies(host: String, port: u16, _proxies: Vec<(String, String)>) -> i32 {
    // Create a Tokio runtime manually (CLI entrypoints can’t be async)
    let rt = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .expect("tokio runtime");

    // Block on async start
    let result = rt.block_on(async move {
        use goblin_devserver::{start, DevOptions};
        let opts = DevOptions { host, port, proxies: Vec::new() };
        start(opts).await
    });

    // Convert Result to exit code
    match result {
        Ok(()) => 0,
        Err(e) => {
            eprintln!("{}", e);
            1
        }
    }
}