//! Interpreter/VM conformance suite.
//!
//! Every `tests/conformance/**/*.gbln` case is run twice through the real
//! `goblin` binary: once with the tree-walking interpreter (`goblin run`) and
//! once with the VM (`goblin run --vm`). Each run's observable result is its
//! stdout, plus a final `<error>` line when the process exits non-zero (error
//! *messages* go to stderr and differ in wording between engines, so only the
//! fact of failure is compared).
//!
//! The observed result must equal the case's `.out` file. That file records the
//! *intended* Goblin behaviour, not whatever an engine happens to do today.
//!
//! Header directives (`///` lines at the top of a case):
//!   /// requires: db | http        skip unless that fixture is available
//!   /// known-gap: interp|vm <why>  this engine is known not to conform yet;
//!                                   reported, not failed (and flagged if it
//!                                   starts passing, so the marker gets removed)
//!   /// env: NAME=value              set an environment variable for the run
//!   /// undecided: <decision-id>    the engines disagree on semantics nobody has
//!                                   ruled on yet; both outputs are reported, no
//!                                   `.out` is required, nothing fails
//!
//! Environment:
//!   GOBLIN_CONF_FILTER=substr   run only matching cases
//!   GOBLIN_CONF_DB=postgres://  enable `requires: db` cases
//!   GOBLIN_CONF_REPORT=path     also write the report there
//!   GOBLIN_CONF_STRICT=1        known gaps and undecided cases fail too
//!                               (this is the "parity" bar)
//!   GOBLIN_CONF_TRACKER=path    also write the per-builtin parity table
//!                               (crates/outputs/VM_PARITY_TODO.md)
//!
//! Cases see GOBLIN_CONF_HTTP (base URL of a local fixture server),
//! GOBLIN_CONF_TMP (a fresh scratch dir per run) and, with a database,
//! GOBLIN_CONF_DB and DATABASE_URL (what the db_* builtins read).
//! The working directory is `tests/conformance`, so imports are written
//! relative to it.

use std::collections::{BTreeMap, BTreeSet};
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

const TIMEOUT: Duration = Duration::from_secs(60);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Engine { Interp, Vm }

impl Engine {
    fn name(self) -> &'static str { match self { Engine::Interp => "interp", Engine::Vm => "vm" } }
}

#[derive(Default, Clone)]
struct Header {
    requires: Vec<String>,
    known_gap: BTreeMap<String, String>,
    undecided: Option<String>,
    env: Vec<(String, String)>,
}

struct Case {
    rel: String,
    header: Header,
    expected: Option<String>,
    src: String,
}

fn suite_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../tests/conformance")
        .canonicalize().expect("tests/conformance must exist")
}

fn parse_header(src: &str) -> Header {
    let mut h = Header::default();
    for line in src.lines() {
        let t = line.trim();
        if t.is_empty() { continue; }
        let Some(rest) = t.strip_prefix("///") else { break };
        let rest = rest.trim();
        if let Some(v) = rest.strip_prefix("requires:") {
            h.requires.extend(v.split(',').map(|s| s.trim().to_string()).filter(|s| !s.is_empty()));
        } else if let Some(v) = rest.strip_prefix("known-gap:") {
            let v = v.trim();
            let (eng, why) = v.split_once(' ').unwrap_or((v, ""));
            h.known_gap.insert(eng.trim().to_string(), why.trim().to_string());
        } else if let Some(v) = rest.strip_prefix("env:") {
            if let Some((k, val)) = v.trim().split_once('=') {
                h.env.push((k.trim().to_string(), val.trim().to_string()));
            }
        } else if let Some(v) = rest.strip_prefix("undecided:") {
            h.undecided = Some(v.trim().to_string());
        }
    }
    h
}

fn collect(dir: &Path, root: &Path, out: &mut Vec<Case>) {
    let mut entries: Vec<_> = std::fs::read_dir(dir).unwrap().flatten().map(|e| e.path()).collect();
    entries.sort();
    for p in entries {
        let name = p.file_name().unwrap().to_string_lossy().to_string();
        if p.is_dir() {
            // `_support` and friends hold modules/fixtures imported by cases.
            if !name.starts_with('_') { collect(&p, root, out); }
        } else if name.ends_with(".gbln") {
            let src = std::fs::read_to_string(&p).unwrap();
            let rel = p.strip_prefix(root).unwrap().to_string_lossy().replace('\\', "/");
            let expected = std::fs::read_to_string(p.with_extension("out")).ok()
                .map(|s| s.replace("\r\n", "\n"));
            out.push(Case { rel, header: parse_header(&src), expected, src });
        }
    }
}

/// Minimal HTTP/1.1 fixture so http_* builtins can be exercised offline.
fn start_http_fixture() -> String {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    std::thread::spawn(move || {
        for stream in listener.incoming().flatten() {
            std::thread::spawn(move || { let _ = serve_one(stream); });
        }
    });
    format!("http://{}", addr)
}

fn serve_one(mut s: TcpStream) -> std::io::Result<()> {
    s.set_read_timeout(Some(Duration::from_secs(10)))?;
    let mut buf = Vec::new();
    let mut tmp = [0u8; 4096];
    let head_end = loop {
        let n = s.read(&mut tmp)?;
        if n == 0 { return Ok(()); }
        buf.extend_from_slice(&tmp[..n]);
        if let Some(i) = buf.windows(4).position(|w| w == b"\r\n\r\n") { break i + 4; }
    };
    let head = String::from_utf8_lossy(&buf[..head_end]).to_string();
    let mut lines = head.split("\r\n");
    let request_line = lines.next().unwrap_or("");
    let mut parts = request_line.split(' ');
    let method = parts.next().unwrap_or("").to_string();
    let path = parts.next().unwrap_or("/").to_string();
    let mut headers = BTreeMap::new();
    for l in lines {
        if let Some((k, v)) = l.split_once(':') { headers.insert(k.trim().to_ascii_lowercase(), v.trim().to_string()); }
    }
    let len: usize = headers.get("content-length").and_then(|v| v.parse().ok()).unwrap_or(0);
    while buf.len() < head_end + len {
        let n = s.read(&mut tmp)?;
        if n == 0 { break; }
        buf.extend_from_slice(&tmp[..n]);
    }
    let body = String::from_utf8_lossy(&buf[head_end..(head_end + len).min(buf.len())]).to_string();

    let (status, extra, out): (u16, Vec<(String, String)>, String) = match path.as_str() {
        "/text" => (200, vec![], "hello".into()),
        "/echo" => {
            let x_test = headers.get("x-test").cloned().unwrap_or_default();
            let ctype = headers.get("content-type").cloned().unwrap_or_default();
            (200, vec![], format!("{} x-test={} content-type={} body={}", method, x_test, ctype, body))
        }
        "/json" => (200, vec![("Content-Type".into(), "application/json".into())], "{\"a\":1,\"b\":[true,null]}".into()),
        "/headers" => (200, vec![("X-Reply".into(), "yes".into())], "ok".into()),
        "/redirect" => (302, vec![("Location".into(), "/text".into())], String::new()),
        "/missing" => (404, vec![], "nope".into()),
        "/fail" => (500, vec![], "boom".into()),
        _ => (404, vec![], "unknown fixture path".into()),
    };
    let reason = match status { 200 => "OK", 302 => "Found", 404 => "Not Found", _ => "Error" };
    let mut resp = format!("HTTP/1.1 {} {}\r\nContent-Length: {}\r\nConnection: close\r\n", status, reason, out.len());
    for (k, v) in extra { resp.push_str(&format!("{}: {}\r\n", k, v)); }
    resp.push_str("\r\n");
    resp.push_str(&out);
    s.write_all(resp.as_bytes())
}

struct Run { observed: String, stderr: String, ms: u128 }

fn run_case(bin: &Path, root: &Path, rel: &str, engine: Engine, env: &[(String, String)]) -> Run {
    let tmp = std::env::temp_dir().join(format!(
        "goblin-conf-{}-{}-{}", std::process::id(), engine.name(), rel.replace(['/', '.'], "_")));
    let _ = std::fs::remove_dir_all(&tmp);
    std::fs::create_dir_all(&tmp).unwrap();

    let mut cmd = Command::new(bin);
    cmd.arg("run");
    if engine == Engine::Vm { cmd.arg("--vm"); }
    cmd.arg(rel).current_dir(root)
        .env("GOBLIN_CONF_TMP", &tmp)
        .env_remove("GOBLIN_ENGINE")
        .stdin(Stdio::null()).stdout(Stdio::piped()).stderr(Stdio::piped());
    for (k, v) in env { cmd.env(k, v); }

    let start = Instant::now();
    let mut child = cmd.spawn().expect("spawn goblin");
    let mut so = child.stdout.take().unwrap();
    let mut se = child.stderr.take().unwrap();
    let t_out = std::thread::spawn(move || { let mut b = Vec::new(); let _ = so.read_to_end(&mut b); b });
    let t_err = std::thread::spawn(move || { let mut b = Vec::new(); let _ = se.read_to_end(&mut b); b });
    let status = loop {
        if let Some(st) = child.try_wait().unwrap() { break Some(st); }
        if start.elapsed() > TIMEOUT { let _ = child.kill(); let _ = child.wait(); break None; }
        std::thread::sleep(Duration::from_millis(5));
    };
    let ms = start.elapsed().as_millis();
    let stdout = String::from_utf8_lossy(&t_out.join().unwrap()).replace("\r\n", "\n");
    let stderr = String::from_utf8_lossy(&t_err.join().unwrap()).to_string();
    let _ = std::fs::remove_dir_all(&tmp);

    let mut observed = stdout;
    match status {
        Some(st) if st.success() => {}
        Some(_) => { if !observed.is_empty() && !observed.ends_with('\n') { observed.push('\n'); } observed.push_str("<error>\n"); }
        None => { if !observed.is_empty() && !observed.ends_with('\n') { observed.push('\n'); } observed.push_str("<timeout>\n"); }
    }
    Run { observed, stderr, ms }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum Verdict { Pass, Fail, KnownGap, GapNowPassing, Undecided, Skipped, NoExpectation }

fn first_stderr_line(s: &str) -> String {
    s.lines().find(|l| l.contains("error")).unwrap_or("").trim().chars().take(160).collect()
}

fn diff_summary(expected: &str, observed: &str) -> String {
    let e: Vec<_> = expected.lines().collect();
    let o: Vec<_> = observed.lines().collect();
    for i in 0..e.len().max(o.len()) {
        let a = e.get(i).copied().unwrap_or("<missing>");
        let b = o.get(i).copied().unwrap_or("<missing>");
        if a != b { return format!("line {}: expected `{}` got `{}`", i + 1, a, b); }
    }
    String::new()
}

#[test]
fn conformance() {
    let root = suite_root();
    let bin = PathBuf::from(env!("CARGO_BIN_EXE_goblin"));
    let mut cases = Vec::new();
    collect(&root, &root, &mut cases);
    if let Ok(f) = std::env::var("GOBLIN_CONF_FILTER") { cases.retain(|c| c.rel.contains(&f)); }
    assert!(!cases.is_empty(), "no conformance cases found");

    let strict = std::env::var("GOBLIN_CONF_STRICT").map(|v| v == "1").unwrap_or(false);
    let db = std::env::var("GOBLIN_CONF_DB").ok().filter(|s| !s.is_empty());
    let mut env = vec![("GOBLIN_CONF_HTTP".to_string(), start_http_fixture())];
    if let Some(d) = &db {
        env.push(("GOBLIN_CONF_DB".into(), d.clone()));
        env.push(("DATABASE_URL".into(), d.clone()));
    }

    // Work queue over (case, engine) pairs.
    let jobs: Vec<(usize, Engine)> = (0..cases.len()).flat_map(|i| [(i, Engine::Interp), (i, Engine::Vm)]).collect();
    let jobs = Arc::new(Mutex::new(jobs));
    let results: Arc<Mutex<BTreeMap<(usize, Engine), Run>>> = Arc::default();
    let cases = Arc::new(cases);
    let workers = std::thread::available_parallelism().map(|n| n.get()).unwrap_or(4).min(16);
    let mut handles = Vec::new();
    for _ in 0..workers {
        let (jobs, results, cases, bin, root, env) = (jobs.clone(), results.clone(), cases.clone(), bin.clone(), root.clone(), env.clone());
        let db_on = db.is_some();
        handles.push(std::thread::spawn(move || loop {
            let Some((i, eng)) = jobs.lock().unwrap().pop() else { break };
            let c = &cases[i];
            if c.header.requires.iter().any(|r| r == "db") && !db_on { continue; }
            let case_env: Vec<(String, String)> = env.iter().chain(c.header.env.iter()).cloned().collect();
            let r = run_case(&bin, &root, &c.rel, eng, &case_env);
            results.lock().unwrap().insert((i, eng), r);
        }));
    }
    for h in handles { h.join().unwrap(); }
    let results = results.lock().unwrap();

    let mut report = String::new();
    let mut counts: BTreeMap<(Engine, Verdict), usize> = BTreeMap::new();
    let mut failures = Vec::new();
    let mut gap_lines = Vec::new();
    let mut undecided_lines = Vec::new();
    let mut areas: BTreeMap<String, (usize, usize, usize)> = BTreeMap::new();
    let mut verdicts: BTreeMap<(usize, Engine), Verdict> = BTreeMap::new();

    for (i, c) in cases.iter().enumerate() {
        let area = c.rel.split('/').next().unwrap_or("").to_string();
        for eng in [Engine::Interp, Engine::Vm] {
            let Some(run) = results.get(&(i, eng)) else {
                *counts.entry((eng, Verdict::Skipped)).or_default() += 1;
                continue;
            };
            let gap = c.header.known_gap.get(eng.name()).or_else(|| c.header.known_gap.get("both"));
            let verdict = if let Some(id) = &c.header.undecided {
                undecided_lines.push(format!("- `{}` [{}] decision {} ({}ms):\n```\n{}```", c.rel, eng.name(), id, run.ms, run.observed));
                Verdict::Undecided
            } else if let Some(exp) = &c.expected {
                let ok = *exp == run.observed;
                match (ok, gap) {
                    (true, None) => Verdict::Pass,
                    (true, Some(_)) => { failures.push(format!("{} [{}]: passes now; remove its known-gap marker", c.rel, eng.name())); Verdict::GapNowPassing }
                    (false, Some(why)) => { gap_lines.push(format!("- `{}` [{}] {} — {}", c.rel, eng.name(), why, diff_summary(exp, &run.observed))); Verdict::KnownGap }
                    (false, None) => {
                        failures.push(format!("{} [{}]: {} {}", c.rel, eng.name(), diff_summary(exp, &run.observed), first_stderr_line(&run.stderr)));
                        Verdict::Fail
                    }
                }
            } else {
                failures.push(format!("{} [{}]: no .out file", c.rel, eng.name()));
                Verdict::NoExpectation
            };
            *counts.entry((eng, verdict)).or_default() += 1;
            verdicts.insert((i, eng), verdict);
            let a = areas.entry(area.clone()).or_default();
            a.0 += 1;
            if verdict == Verdict::Pass { a.1 += 1; }
            if matches!(verdict, Verdict::KnownGap | Verdict::Undecided) { a.2 += 1; }
        }
    }

    report.push_str("# Goblin interpreter/VM conformance report\n\n");
    report.push_str(&format!("{} cases. Each case runs on both engines.\n\n", cases.len()));
    report.push_str("| engine | pass | fail | known gap | undecided | skipped |\n|---|---|---|---|---|---|\n");
    for eng in [Engine::Interp, Engine::Vm] {
        let g = |v| counts.get(&(eng, v)).copied().unwrap_or(0);
        report.push_str(&format!("| {} | {} | {} | {} | {} | {} |\n", eng.name(), g(Verdict::Pass),
            g(Verdict::Fail) + g(Verdict::NoExpectation) + g(Verdict::GapNowPassing), g(Verdict::KnownGap), g(Verdict::Undecided), g(Verdict::Skipped)));
    }
    report.push_str("\n## By area (runs: pass / total, gap+undecided)\n\n");
    for (a, (t, p, g)) in &areas { report.push_str(&format!("- {}: {}/{} ({} gap/undecided)\n", a, p, t, g)); }
    if !failures.is_empty() {
        report.push_str("\n## Failures\n\n");
        for f in &failures { report.push_str(&format!("- {}\n", f)); }
    }
    if !gap_lines.is_empty() {
        report.push_str("\n## Known gaps\n\n");
        for g in &gap_lines { report.push_str(&format!("{}\n", g)); }
    }
    if !undecided_lines.is_empty() {
        report.push_str("\n## Undecided semantics\n\n");
        for u in &undecided_lines { report.push_str(&format!("{}\n", u)); }
    }
    let parity = failures.is_empty() && gap_lines.is_empty() && undecided_lines.is_empty()
        && counts.keys().all(|(_, v)| *v != Verdict::Skipped);
    report.push_str(&format!("\nParity demonstrated: {}\n", if parity { "YES" } else { "NO" }));

    println!("{}", report);
    if let Ok(p) = std::env::var("GOBLIN_CONF_REPORT") { std::fs::write(p, &report).unwrap(); }
    if let Ok(p) = std::env::var("GOBLIN_CONF_TRACKER") {
        std::fs::write(p, builtin_tracker(&root.join("../.."), &cases, &verdicts)).unwrap();
    }

    assert!(failures.is_empty(), "{} conformance failure(s); see report above", failures.len());
    if strict {
        assert!(parity, "strict mode: known gaps, undecided cases or skipped fixtures remain");
    }
}

/// Every builtin name either engine knows must be exercised by at least one
/// case, or be listed in `COVERAGE_EXEMPT.txt` with a reason. This keeps the
/// suite from silently falling behind the runtimes.
#[test]
fn builtin_coverage() {
    let root = suite_root();
    let repo = root.join("../..");
    let names = builtin_inventory(&repo);

    let mut corpus = String::new();
    let mut stack = vec![root.clone()];
    while let Some(d) = stack.pop() {
        for e in std::fs::read_dir(&d).unwrap().flatten() {
            let p = e.path();
            if p.is_dir() { stack.push(p); }
            else if p.extension().map(|x| x == "gbln").unwrap_or(false) {
                corpus.push_str(&std::fs::read_to_string(&p).unwrap()); corpus.push('\n');
            }
        }
    }
    let words: BTreeSet<&str> = corpus.split(|c: char| !(c.is_alphanumeric() || c == '_' || c == '!' || c == '?')).collect();
    let words_trimmed: BTreeSet<String> = words.iter().map(|w| w.trim_end_matches(['!', '?']).to_string()).collect();

    let exempt_src = std::fs::read_to_string(root.join("COVERAGE_EXEMPT.txt")).unwrap_or_default();
    let exempt: BTreeSet<String> = exempt_src.lines()
        .map(|l| l.split('#').next().unwrap_or("").trim().to_string())
        .filter(|l| !l.is_empty()).collect();

    let mut missing = Vec::new();
    for (name, engines) in &names {
        let bare = name.trim_end_matches(['!', '?']);
        if exempt.contains(name) || exempt.contains(bare) { continue; }
        if words.contains(name.as_str()) || words_trimmed.contains(bare) { continue; }
        missing.push(format!("{} ({})", name, engines.join("+")));
    }
    for e in &exempt {
        if !names.contains_key(e) && !names.keys().any(|n| n.trim_end_matches(['!', '?']) == e) {
            missing.push(format!("{} is exempted but no engine defines it; remove it from COVERAGE_EXEMPT.txt", e));
        }
    }
    assert!(missing.is_empty(), "builtins with no conformance case:\n  {}", missing.join("\n  "));
}

fn case_words(src: &str) -> BTreeSet<String> {
    src.split(|c: char| !(c.is_alphanumeric() || c == '_' || c == '!' || c == '?'))
        .map(|w| w.trim_end_matches(['!', '?']).to_string())
        .filter(|w| !w.is_empty())
        .collect()
}

/// The VM feature tracker: for every builtin either engine defines, how the
/// cases that mention it fare on each engine.
fn builtin_tracker(repo: &Path, cases: &[Case], verdicts: &BTreeMap<(usize, Engine), Verdict>) -> String {
    let names = builtin_inventory(repo);
    let words: Vec<BTreeSet<String>> = cases.iter().map(|c| case_words(&c.src)).collect();
    let mut rows = Vec::new();
    let mut tally: BTreeMap<&'static str, usize> = BTreeMap::new();
    for (name, engines) in &names {
        let using: Vec<usize> = (0..cases.len()).filter(|&i| words[i].contains(name)).collect();
        let cell = |eng: Engine| {
            let (mut pass, mut gap, mut other) = (0, 0, 0);
            for &i in &using {
                match verdicts.get(&(i, eng)) {
                    Some(Verdict::Pass) => pass += 1,
                    Some(Verdict::KnownGap) | Some(Verdict::Undecided) => gap += 1,
                    _ => other += 1,
                }
            }
            (pass, gap, other)
        };
        let (ip, ig, io) = cell(Engine::Interp);
        let (vp, vg, vo) = cell(Engine::Vm);
        let status = if using.is_empty() {
            "untested"
        } else if ig + io + vg + vo == 0 {
            "parity"
        } else if engines.len() == 1 && engines[0] == "vm" {
            "vm only"
        } else if engines.len() == 1 && engines[0] == "interp" {
            "interp only"
        } else if vg + vo > 0 && ig + io > 0 {
            "gaps on both"
        } else if vg + vo > 0 {
            "vm gap"
        } else {
            "interp gap"
        };
        *tally.entry(status).or_default() += 1;
        rows.push(format!("| `{}` | {} | {} | {}/{} | {}/{} |", name, engines.join("+"), status,
            ip, ip + ig + io, vp, vp + vg + vo));
    }
    let mut out = String::new();
    out.push_str("# VM feature tracker (tested)\n\n");
    out.push_str("Generated by the conformance suite (`GOBLIN_CONF_TRACKER=crates/outputs/VM_PARITY_TODO.md \\\n");
    out.push_str("cargo test --release -p goblin-cli --test conformance conformance`). Do not edit by hand.\n\n");
    out.push_str("Each builtin either engine defines, with the conformance cases that mention it:\n");
    out.push_str("passing cases / cases, per engine. A case that fails, is a known gap or is undecided counts\n");
    out.push_str("against the engine. `parity` means every such case passes on both engines; `vm only` and\n");
    out.push_str("`interp only` mean one engine lacks the builtin (see tests/conformance/DECISIONS.md);\n");
    out.push_str("`untested` means no case mentions it yet. Details of each gap are in the suite report.\n\n");
    out.push_str("Summary: ");
    out.push_str(&tally.iter().map(|(k, v)| format!("{} {}", k, v)).collect::<Vec<_>>().join(", "));
    out.push_str("\n\n| builtin | defined by | status | interp | vm |\n|---|---|---|---|---|\n");
    for r in rows { out.push_str(&r); out.push('\n'); }
    out
}

/// Builtin names, read straight from the engines' dispatch tables.
fn builtin_inventory(repo: &Path) -> BTreeMap<String, Vec<&'static str>> {
    let mut out: BTreeMap<String, Vec<&'static str>> = BTreeMap::new();

    // VM: the string arms of `builtin_by_name` in compiler.rs.
    let compiler = std::fs::read_to_string(repo.join("crates/goblin-vm/src/compiler.rs")).unwrap();
    let start = compiler.find("pub fn builtin_by_name").expect("builtin_by_name");
    let body = &compiler[start..];
    let end = body.find("\n}\n").unwrap_or(body.len());
    for line in body[..end].lines() {
        let Some((lhs, _)) = line.split_once("=>") else { continue };
        for name in quoted(lhs) {
            let n = name.trim_start_matches(':').trim_end_matches(['!', '?']).to_string();
            if !n.is_empty() { let v = out.entry(n).or_default(); if !v.contains(&"vm") { v.push("vm"); } }
        }
    }

    // Interpreter: the string arms of `call_action_by_name`'s top-level match
    // and of `eval_builtin`.
    let interp = std::fs::read_to_string(repo.join("crates/goblin-interpreter/src/lib.rs")).unwrap();
    for marker in ["fn eval_builtin", "fn call_action_by_name"] {
        let Some(s) = interp.find(marker) else { continue };
        let body = &interp[s..];
        let end = [body[1..].find("\nfn "), body[1..].find("\npub fn ")].into_iter().flatten().min().map(|i| i + 1).unwrap_or(body.len());
        for line in body[..end].lines() {
            let t = line.trim_start();
            if !t.starts_with('"') { continue; }
            let Some((lhs, _)) = t.split_once("=>") else { continue };
            for name in quoted(lhs) {
                let n = name.trim_start_matches(':').trim_end_matches(['!', '?']).to_string();
                if n.is_empty() || !n.chars().next().unwrap().is_ascii_alphabetic() { continue; }
                if n.chars().any(|c| !(c.is_ascii_alphanumeric() || c == '_' || c == '!' || c == '?')) { continue; }
                let v = out.entry(n).or_default(); if !v.contains(&"interp") { v.push("interp"); }
            }
        }
    }
    out
}

fn quoted(s: &str) -> Vec<String> {
    let mut v = Vec::new();
    let mut rest = s;
    while let Some(a) = rest.find('"') {
        let after = &rest[a + 1..];
        let Some(b) = after.find('"') else { break };
        v.push(after[..b].to_string());
        rest = &after[b + 1..];
    }
    v
}
