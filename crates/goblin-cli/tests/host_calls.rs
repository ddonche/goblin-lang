//! Database and HTTP calls made by actors do not hold the actor's worker
//! thread: the handler stops, the worker runs other actors, and the handler
//! goes on once the answer is there. Each case runs the `goblin` binary with
//! one worker thread against a slow HTTP server in this test process.

use std::io::{BufRead, BufReader, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

fn bin() -> PathBuf { PathBuf::from(env!("CARGO_BIN_EXE_goblin")) }

/// An HTTP server that answers `GET /<ms>` with "ok" after `ms` milliseconds.
fn slow_server() -> u16 {
    let l = TcpListener::bind("127.0.0.1:0").unwrap();
    let port = l.local_addr().unwrap().port();
    std::thread::spawn(move || {
        for s in l.incoming().flatten() {
            std::thread::spawn(move || answer(s));
        }
    });
    port
}

fn answer(mut s: TcpStream) {
    let mut buf = Vec::new();
    let mut b = [0u8; 1024];
    while !buf.windows(4).any(|w| w == b"\r\n\r\n") {
        match s.read(&mut b) { Ok(0) | Err(_) => return, Ok(n) => buf.extend_from_slice(&b[..n]) }
    }
    let head = String::from_utf8_lossy(&buf);
    let ms: u64 = head.split_whitespace().nth(1).and_then(|p| p.trim_start_matches('/').parse().ok()).unwrap_or(0);
    std::thread::sleep(Duration::from_millis(ms));
    let _ = s.write_all(b"HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok");
}

fn write_main(name: &str, src: &str) -> (PathBuf, PathBuf) {
    let dir = std::env::temp_dir().join(format!("goblin-host-{}-{name}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let file = dir.join("main.gbln");
    std::fs::write(&file, src).unwrap();
    (dir, file)
}

fn command(dir: &PathBuf, file: &PathBuf, extra: &[(&str, &str)]) -> Command {
    let mut cmd = Command::new(bin());
    cmd.arg("run").arg(file).current_dir(dir)
        .env("GOBLIN_ACTOR_THREADS", "1")
        .env_remove("GOBLIN_ENGINE");
    for (k, v) in extra { cmd.env(k, v); }
    cmd
}

/// Run `src`; each stdout line with the time it arrived since the start.
fn run_timed(name: &str, src: &str, extra: &[(&str, &str)]) -> Vec<(Duration, String)> {
    let (dir, file) = write_main(name, src);
    let t0 = Instant::now();
    let mut child = command(&dir, &file, extra).stdout(Stdio::piped()).stderr(Stdio::piped()).spawn().unwrap();
    let mut lines = Vec::new();
    for l in BufReader::new(child.stdout.take().unwrap()).lines() {
        lines.push((t0.elapsed(), l.unwrap()));
    }
    let out = child.wait_with_output().unwrap();
    assert!(out.status.success(), "{name} failed: {lines:?}\n{}", String::from_utf8_lossy(&out.stderr));
    lines
}

fn at(lines: &[(Duration, String)], text: &str) -> Duration {
    lines.iter().find(|(_, l)| l == text).unwrap_or_else(|| panic!("no line {text:?} in {lines:?}")).0
}

/// While one actor waits on a slow HTTP call, another actor on the same
/// (only) worker still answers. Before, the ping waited for the call.
#[test]
fn an_actor_waiting_on_http_leaves_its_worker_free() {
    let port = slow_server();
    let src = format!(r#"act slow(state, url)
    return :http_get(url){{"body"}}
xx
act echo(state, msg)
    return msg
xx
s | :actor("", slow)
p | :actor(0, echo)
:await(:request(p, 0))
say("start")
f | :request(s, "http://127.0.0.1:{port}/800")
j | 0
while j < 20
    :await(:request(p, j))
    j |= j + 1
xx
say("pings")
say(:await(f))
"#);
    let lines = run_timed("http-free", &src, &[]);
    let (start, pings, done) = (at(&lines, "start"), at(&lines, "pings"), at(&lines, "ok"));
    assert!(pings - start < Duration::from_millis(400), "pings took {:?}", pings - start);
    assert!(done - start >= Duration::from_millis(800), "the call ended too early: {:?}", done - start);
}

/// Twenty actors on one worker wait on their HTTP calls at the same time.
#[test]
fn http_calls_from_many_actors_overlap() {
    let port = slow_server();
    let src = format!(r#"act fetch(state, url)
    return state + :len(:http_get(url){{"body"}})
xx
hs | []
i | 0
while i < 20
    :put_last!(hs, :actor(0, fetch))
    i |= i + 1
xx
say("start")
fs | []
for h in hs
    :put_last!(fs, :request(h, "http://127.0.0.1:{port}/300"))
xx
total | 0
for f in fs
    total |= total + :await(f)
xx
say("total {{total}}")
"#);
    let lines = run_timed("http-overlap", &src, &[]);
    let took = at(&lines, "total 40") - at(&lines, "start");
    // One after another would take 6 s.
    assert!(took < Duration::from_millis(2500), "20 calls took {took:?}");
}

/// A call that fails inside an actor fails the same way as outside one.
#[test]
fn a_failed_http_call_in_an_actor_gives_the_usual_error() {
    let closed = TcpListener::bind("127.0.0.1:0").unwrap().local_addr().unwrap().port();
    let src = format!(r#"act fetch(state, url)
    attempt
        :http_get(url)
    rescue e
        return "{{e}}"
    xx
    return "no error"
xx
url | "http://127.0.0.1:{closed}/0"
outside | ""
attempt
    :http_get(url)
rescue e
    outside |= "{{e}}"
xx
a | :actor("", fetch)
inside | :await(:request(a, url))
say(:has(inside, "http error"))
say(inside == outside)
"#);
    let lines = run_timed("http-error", &src, &[]);
    let out: Vec<&str> = lines.iter().map(|(_, l)| l.as_str()).collect();
    assert_eq!(out, ["true", "true"]);
}

/// Inside a callback (`:map_fn`) the handler cannot stop; the call still
/// works, and the worker runs other actors while it waits.
#[test]
fn http_inside_a_callback_in_an_actor_works() {
    let port = slow_server();
    let src = format!(r#"act body(url)
    return :http_get(url){{"body"}}
xx
act fetch_all(state, urls)
    return :map_fn(urls, "body")
xx
a | :actor([], fetch_all)
u | "http://127.0.0.1:{port}/50"
say(:await(:request(a, [u, u, u])))
"#);
    let lines = run_timed("http-callback", &src, &[]);
    assert_eq!(lines.last().unwrap().1, "[ok, ok, ok]");
}

/// Database calls from actors overlap too, and give the same rows as
/// outside an actor. Runs when GOBLIN_CONF_DB names a Postgres database.
#[test]
fn db_calls_from_actors_overlap_and_match_outside() {
    let Ok(url) = std::env::var("GOBLIN_CONF_DB") else {
        eprintln!("skipped: GOBLIN_CONF_DB is not set");
        return;
    };
    let src = r#"act q(state, msg)
    :db_query("select pg_sleep(0.3) as s", [])
    return :db_query("select $1::int + 1 as n, 'x' as t", [msg])
xx
act one(state, msg)
    return :db_query_one("select $1::text as v", [msg])
xx
act ex(state, msg)
    return :db_exec("select 1", [])
xx
hs | []
i | 0
while i < 10
    :put_last!(hs, :actor(0, q))
    i |= i + 1
xx
say("start")
fs | []
i |= 0
while i < 10
    :put_last!(fs, :request(hs[i], i))
    i |= i + 1
xx
rows | []
for f in fs
    :put_last!(rows, :await(f))
xx
say("done")
say(rows[3] == :db_query("select $1::int + 1 as n, 'x' as t", [3]))
say(:await(:request(:actor(0, one), "hi")) == :db_query_one("select $1::text as v", ["hi"]))
say(:await(:request(:actor(0, ex), 0)) == :db_exec("select 1", []))
"#;
    let lines = run_timed("db-overlap", src, &[("DATABASE_URL", &url), ("GOBLIN_DB_POOL_MAX", "20")]);
    let took = at(&lines, "done") - at(&lines, "start");
    // One after another would take 3 s.
    assert!(took < Duration::from_millis(1500), "10 calls took {took:?}");
    let tail: Vec<&str> = lines.iter().skip(2).map(|(_, l)| l.as_str()).collect();
    assert_eq!(tail, ["true", "true", "true"]);
}
