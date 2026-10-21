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

/// A chain of twenty owners on one worker: cancelling the top one ends them
/// all, and the program exits. Before, each owner waited inside the one
/// above it, and past eight levels the worker stopped running the others.
#[test]
fn cancel_ends_a_deep_chain_of_owners_on_one_worker() {
    cancel_case("chain", r#"act node(state, depth)
    if depth == 0
        return :receive(:channel(1))
    xx
    c | :actor(0, node)
    return :await(:request(c, depth - 1))
xx
a | :actor(0, node)
f | :request(a, 20)
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

/// Starts a server with `startup`, stops it with SIGTERM and a one-second
/// grace period; how long it took to stop, and its stderr.
#[cfg(unix)]
fn stop_server(name: &str, startup: &str) -> (Duration, String) {
    stop_server_with_grace(name, startup, "1")
}

#[cfg(unix)]
fn stop_server_with_grace(name: &str, startup: &str, grace: &str) -> (Duration, String) {
    let dir = scratch(name);
    std::fs::create_dir_all(dir.join("www/api")).unwrap();
    std::fs::write(dir.join("startup.gbln"), startup).unwrap();
    let port = free_port();
    let mut child = Command::new(bin())
        .args(["start", "--host", "127.0.0.1", "--port", &port.to_string(), "--startup", "startup.gbln"])
        .current_dir(&dir)
        .env("GOBLIN_DOCROOT", dir.join("www"))
        .env("GOBLIN_SHUTDOWN_GRACE", grace)
        .stdout(Stdio::null()).stderr(Stdio::piped())
        .spawn().unwrap();
    let deadline = Instant::now() + Duration::from_secs(20);
    // A connect that reaches itself (the port is in the ephemeral range, so
    // the client can be handed the same port) is not the server.
    while TcpStream::connect(("127.0.0.1", port)).map_or(true, |s| s.local_addr().ok() == s.peer_addr().ok()) {
        assert!(Instant::now() < deadline, "server did not start");
        std::thread::sleep(Duration::from_millis(50));
    }
    let t0 = Instant::now();
    Command::new("kill").args(["-TERM", &child.id().to_string()]).status().unwrap();
    while child.try_wait().unwrap().is_none() {
        if t0.elapsed() > Duration::from_secs(10) {
            let _ = child.kill();
            panic!("{name}: server did not stop");
        }
        std::thread::sleep(Duration::from_millis(50));
    }
    let took = t0.elapsed();
    let mut err = String::new();
    child.stderr.take().unwrap().read_to_string(&mut err).unwrap();
    (took, err)
}

/// Server shutdown: a startup actor stuck in its handler is cancelled once
/// the grace period is over, and stderr says so. Before, the server never
/// stopped.
#[cfg(unix)]
#[test]
fn shutdown_cancels_startup_actors_still_busy_after_the_grace_period() {
    let (took, err) = stop_server("shutdown", r#"act stuck(state, ch)
    return :receive(ch)
xx
ch | :channel(1)
worker | :actor(0, stuck)
:post(worker, ch)
"#);
    assert!(took >= Duration::from_millis(900), "stopped before the grace period: {took:?}");
    assert!(took < Duration::from_secs(4), "took {took:?}");
    assert!(err.contains("1 actor(s) still busy after 1s were cancelled"), "{err}");
}

/// The startup actor itself is done, but an actor it started is stuck: the
/// owner, waiting for it, notices its own cancellation and cancels it.
/// Before, the server waited on the owner forever.
#[cfg(unix)]
#[test]
fn shutdown_cancels_a_stuck_actor_started_by_a_startup_actor() {
    let (took, err) = stop_server("shutdown-child", r#"act child(state, msg)
    return :receive(:channel(1))
xx
act parent(state, msg)
    c | :actor(0, child)
    :post(c, 1)
    return state
xx
p | :actor(0, parent)
:post(p, 1)
"#);
    assert!(took < Duration::from_secs(4), "took {took:?}");
    assert!(err.contains("1 actor(s) still busy after 1s were cancelled"), "{err}");
}

/// A shutdown grace that is not a usable number of seconds (`inf`, `-1`,
/// `abc`) falls back to the default with a warning. Before, `inf` crashed
/// the server while it shut down.
#[cfg(unix)]
#[test]
fn a_bad_shutdown_grace_falls_back_to_the_default() {
    for bad in ["inf", "-1", "abc"] {
        let (took, err) = stop_server_with_grace("bad-grace", "act idle(state, msg)\n    return state\nxx\nw | :actor(0, idle)\n", bad);
        assert!(took < Duration::from_secs(4), "{bad}: took {took:?}");
        assert!(err.contains("is not a number of seconds; using 10"), "{bad}: {err}");
        assert!(!err.contains("panicked"), "{bad}: {err}");
    }
}

/// Cancelled while the stop condition waits: the actor ends as cancelled
/// (C0116), not as failed. Before, it was reported as a failure (C0112).
#[test]
fn cancel_during_a_waiting_stop_condition_is_a_cancellation() {
    let (out, _) = run("stop-when", r#"ch | :channel(1)
act count(state, msg)
    return state + 1
xx
act wait_stop(state)
    :receive(ch)
    return false
xx
a | :actor(0, count, {stop_when: wait_stop})
say(:await(:request(a, 1)))
nap | :channel(1)
:receive(nap, 1s)
:cancel(a)
attempt
    :await(a)
rescue e
    say("{e}")
xx
"#, Duration::from_secs(10));
    let lines: Vec<&str> = out.lines().collect();
    assert_eq!(lines[0], "1", "{out}");
    assert!(lines[1].contains("C0116: actor-cancelled") && !lines[1].contains("C0112"), "{out}");
}
