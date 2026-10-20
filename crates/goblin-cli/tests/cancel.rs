//! `:cancel` stops an actor at its next checkpoint, wherever its handler is:
//! waiting on a channel, a reply, an HTTP call, inside a callback, or running
//! a long loop. The request it was handling fails with C0116, and the actors
//! it started are cancelled with it. Server shutdown gives the startup actors
//! a grace period, then cancels the ones still busy.

use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

fn bin() -> PathBuf { PathBuf::from(env!("CARGO_BIN_EXE_goblin")) }

fn scratch(name: &str) -> PathBuf {
    let d = std::env::temp_dir().join(format!("goblin-cancel-{}-{name}", std::process::id()));
    let _ = std::fs::remove_dir_all(&d);
    std::fs::create_dir_all(&d).unwrap();
    d
}

/// Run `src` on one worker; stdout and how long the run took. Fails if the
/// run takes longer than `limit` (the old behaviour was to hang).
fn run(name: &str, src: &str, limit: Duration) -> (String, Duration) {
    let dir = scratch(name);
    let file = dir.join("main.gbln");
    std::fs::write(&file, src).unwrap();
    let t0 = Instant::now();
    let mut child = Command::new(bin()).arg("run").arg(&file).current_dir(&dir)
        .env("GOBLIN_ACTOR_THREADS", "1").env_remove("GOBLIN_ENGINE")
        .stdout(Stdio::piped()).stderr(Stdio::piped()).spawn().unwrap();
    while child.try_wait().unwrap().is_none() {
        if t0.elapsed() > limit {
            let _ = child.kill();
            panic!("{name}: still running after {limit:?}");
        }
        std::thread::sleep(Duration::from_millis(20));
    }
    let took = t0.elapsed();
    let out = child.wait_with_output().unwrap();
    assert!(out.status.success(), "{name} failed: {}", String::from_utf8_lossy(&out.stderr));
    (String::from_utf8_lossy(&out.stdout).to_string(), took)
}

const MIDWAY: &str = "C0116: actor-cancelled: the actor was cancelled while it handled this request";

/// Waits a second, cancels `a`, and prints what awaiting `f` gives.
const CANCEL_AFTER_A_SECOND: &str = r#"nap | :channel(1)
:receive(nap, 1s)
:cancel(a)
attempt
    say(:await(f))
rescue e
    say("{e}")
xx
say("end")
"#;

fn cancel_case(name: &str, setup: &str) {
    let src = format!("{setup}{CANCEL_AFTER_A_SECOND}");
    let (out, took) = run(name, &src, Duration::from_secs(10));
    assert!(out.contains(MIDWAY), "{name}: {out}");
    assert!(out.ends_with("end\n"), "{name}: {out}");
    assert!(took < Duration::from_secs(3), "{name} took {took:?}");
}

/// A handler in a long loop stops at its next turn. Before, it ran to the end.
#[test]
fn cancel_stops_a_busy_handler() {
    cancel_case("busy", r#"act spin(state, msg)
    i | 0
    while i < 3000000000
        i |= i + 1
    xx
    return i
xx
a | :actor(0, spin)
f | :request(a, 1)
"#);
}

/// A handler waiting inside a callback stops too, and its own `rescue`
/// cannot keep it going. Before, it waited forever.
#[test]
fn cancel_stops_a_wait_inside_a_callback() {
    cancel_case("callback", r#"act wait_one(ch)
    return :receive(ch)
xx
act cb(state, ch)
    attempt
        :map_fn([ch], "wait_one")
    rescue e
        return "caught"
    xx
    return "done"
xx
ch | :channel(1)
a | :actor(0, cb)
f | :request(a, ch)
"#);
}

/// A handler waiting on a slow HTTP call stops without waiting for the answer.
#[test]
fn cancel_stops_a_handler_waiting_on_http() {
    let l = TcpListener::bind("127.0.0.1:0").unwrap();
    let port = l.local_addr().unwrap().port();
    std::thread::spawn(move || {
        for s in l.incoming().flatten() {
            std::thread::spawn(move || {
                let mut s = s;
                let mut b = [0u8; 1024];
                let _ = s.read(&mut b);
                std::thread::sleep(Duration::from_secs(20));
                let _ = s.write_all(b"HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok");
            });
        }
    });
    cancel_case("http", &format!(r#"act slow(state, url)
    return :http_get(url){{"body"}}
xx
a | :actor(0, slow)
f | :request(a, "http://127.0.0.1:{port}/")
"#));
}

/// Cancelling an actor cancels the actors it started. Before, the parent
/// waited for its child forever.
#[test]
fn cancel_reaches_the_actors_it_started() {
    cancel_case("children", r#"act child(state, ch)
    return :receive(ch)
xx
act parent(state, ch)
    c | :actor(0, child)
    return :await(:request(c, ch))
xx
ch | :channel(1)
a | :actor(0, parent)
f | :request(a, ch)
"#);
}

/// Requests queued behind the cancelled one fail without running.
#[test]
fn cancel_fails_the_queued_requests() {
    let (out, _) = run("queued", r#"act take(state, ch)
    return :receive(ch)
xx
ch | :channel(1)
a | :actor(0, take)
f1 | :request(a, ch)
f2 | :request(a, ch)
nap | :channel(1)
:receive(nap, 1s)
:cancel(a)
for f in [f1, f2]
    attempt
        say(:await(f))
    rescue e
        say("{e}")
    xx
xx
"#, Duration::from_secs(10));
    let lines: Vec<&str> = out.lines().collect();
    assert_eq!(lines.len(), 2, "{out}");
    assert!(lines[0].contains(MIDWAY), "{out}");
    assert!(lines[1].contains("cancelled before it handled this request"), "{out}");
}

fn free_port() -> u16 { TcpListener::bind("127.0.0.1:0").unwrap().local_addr().unwrap().port() }

/// Server shutdown: a startup actor stuck in its handler is cancelled once
/// the grace period is over, and stderr says so. Before, the server never
/// stopped.
#[cfg(unix)]
#[test]
fn shutdown_cancels_startup_actors_still_busy_after_the_grace_period() {
    let dir = scratch("shutdown");
    std::fs::create_dir_all(dir.join("www/api")).unwrap();
    std::fs::write(dir.join("startup.gbln"), r#"act stuck(state, ch)
    return :receive(ch)
xx
ch | :channel(1)
worker | :actor(0, stuck)
:post(worker, ch)
"#).unwrap();
    let port = free_port();
    let mut child = Command::new(bin())
        .args(["start", "--host", "127.0.0.1", "--port", &port.to_string(), "--startup", "startup.gbln"])
        .current_dir(&dir)
        .env("GOBLIN_DOCROOT", dir.join("www"))
        .env("GOBLIN_SHUTDOWN_GRACE", "1")
        .stdout(Stdio::null()).stderr(Stdio::piped())
        .spawn().unwrap();
    let deadline = Instant::now() + Duration::from_secs(20);
    while TcpStream::connect(("127.0.0.1", port)).is_err() {
        assert!(Instant::now() < deadline, "server did not start");
        std::thread::sleep(Duration::from_millis(50));
    }
    let t0 = Instant::now();
    Command::new("kill").args(["-TERM", &child.id().to_string()]).status().unwrap();
    while child.try_wait().unwrap().is_none() {
        if t0.elapsed() > Duration::from_secs(10) {
            let _ = child.kill();
            panic!("server did not stop");
        }
        std::thread::sleep(Duration::from_millis(50));
    }
    let took = t0.elapsed();
    let mut err = String::new();
    child.stderr.take().unwrap().read_to_string(&mut err).unwrap();
    assert!(took >= Duration::from_millis(900), "stopped before the grace period: {took:?}");
    assert!(took < Duration::from_secs(4), "took {took:?}");
    assert!(err.contains("1 actor(s) still busy after 1s were cancelled"), "{err}");
}
