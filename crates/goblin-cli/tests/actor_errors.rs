//! What happens when an actor's handler fails. A request gets the handler's
//! own error; requests queued behind it, later posts and `:await` on the
//! actor get C0112; a failure no request carries back (a posted message, the
//! stop condition) is written to stderr with the handler's name, so it is
//! never lost silently.

use std::io::Read;
use std::net::{TcpListener, TcpStream};
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

fn bin() -> PathBuf { PathBuf::from(env!("CARGO_BIN_EXE_goblin")) }

fn scratch(name: &str) -> PathBuf {
    let d = std::env::temp_dir().join(format!("goblin-actor-errors-{}-{name}", std::process::id()));
    let _ = std::fs::remove_dir_all(&d);
    std::fs::create_dir_all(&d).unwrap();
    d
}

/// Run `src` on one worker; stdout and stderr.
fn run(name: &str, src: &str) -> (String, String) {
    let dir = scratch(name);
    let file = dir.join("main.gbln");
    std::fs::write(&file, src).unwrap();
    let out = Command::new(bin()).arg("run").arg(&file).current_dir(&dir)
        .env("GOBLIN_ACTOR_THREADS", "1").env_remove("GOBLIN_ENGINE")
        .output().unwrap();
    let (so, se) = (String::from_utf8_lossy(&out.stdout).to_string(), String::from_utf8_lossy(&out.stderr).to_string());
    assert!(out.status.success(), "{name} failed:\n{so}\n{se}");
    (so, se)
}

const FAILED: &str = "C0112: actor-failed: the actor stopped because its handler failed";

/// The failing request gets the handler's error with its line; a request
/// queued behind it, a later post and `:await` on the actor get C0112, and
/// awaiting the failed request again gives the same error. Nothing goes to
/// stderr: every failure reached someone.
#[test]
fn a_failing_request_reaches_everyone_waiting() {
    let (out, err) = run("request", r#"act boom(state, msg)
    i | 0
    while i < 100000
        i |= i + 1
    xx
    x | 1 / msg
    return state + x
xx
a | :actor(0, boom)
f | :request(a, 0)
g | :request(a, 1)
for w in [f, g, f]
    attempt
        say(:await(w))
    rescue e
        say("{e}")
    xx
xx
attempt
    :post(a, 1)
rescue e
    say("{e}")
xx
attempt
    :await(a)
rescue e
    say("{e}")
xx
"#);
    let lines: Vec<&str> = out.lines().collect();
    assert_eq!(lines.len(), 5, "{out}");
    assert!(lines[0].contains("[main.gbln:6] division by zero") && !lines[0].contains("C0112"), "{out}");
    assert!(lines[1].contains(FAILED), "{out}");
    assert_eq!(lines[2], lines[0]);
    assert!(lines[3].contains(FAILED) && lines[4].contains(FAILED), "{out}");
    assert!(!err.contains("goblin: actor"), "{err}");
}

/// A posted message fails: nobody waits for an answer, so stderr says which
/// actor failed and why. Before, the failure vanished.
#[test]
fn a_failing_posted_message_is_reported_on_stderr() {
    let (out, err) = run("post", r#"act boom(state, msg)
    x | 1 / msg
    return state + x
xx
a | :actor(0, boom)
:post(a, 0)
say("posted")
"#);
    assert_eq!(out, "posted\n");
    assert!(err.contains("goblin: actor `boom` failed: [main.gbln:2] division by zero"), "{err}");
}

/// The same for an actor started by another actor and only posted to.
#[test]
fn a_failing_child_is_reported_on_stderr() {
    let (out, err) = run("child", r#"act worker(state, msg)
    x | 1 / 0
    return state
xx
act boss(state, msg)
    c | :actor(0, worker)
    :post(c, msg)
    return state + 1
xx
b | :actor(0, boss)
say(:await(:request(b, 1)))
"#);
    assert_eq!(out, "1\n");
    assert!(err.contains("goblin: actor `worker` failed: [main.gbln:2] division by zero"), "{err}");
}

/// A stop condition that fails ends the actor: stderr says so, and `:await`
/// on the actor gives C0112 with the reason.
#[test]
fn a_failing_stop_condition_is_reported() {
    let (out, err) = run("stop-when", r#"act count(state, msg)
    return state + 1
xx
act bad(state)
    return 1 / 0
xx
c | :actor(0, count, {stop_when: bad})
say(:await(:request(c, 1)))
attempt
    :await(c)
rescue e
    say("{e}")
xx
"#);
    let lines: Vec<&str> = out.lines().collect();
    assert_eq!(lines[0], "1");
    assert!(lines[1].contains(FAILED) && lines[1].contains("stop_when failed"), "{out}");
    assert!(err.contains("goblin: actor `count` failed: stop_when failed: [main.gbln:5] division by zero"), "{err}");
}

/// A failure two actors down reaches the program with both lines, and a
/// failure inside a callback (`:map_fn`) reaches the requester.
#[test]
fn failures_travel_through_actors_and_callbacks() {
    let (out, err) = run("chain", r#"act boom(state, msg)
    x | 1 / msg
    return state + x
xx
act middle(state, msg)
    b | :actor(0, boom)
    return :await(:request(b, msg))
xx
act helper(x)
    return 1 / x
xx
act each(state, xs)
    return :map_fn(xs, "helper")
xx
m | :actor(0, middle)
attempt
    :await(:request(m, 0))
rescue e
    say("{e}")
xx
c | :actor(0, each)
attempt
    :await(:request(c, [1, 0]))
rescue e
    say("{e}")
xx
"#);
    let lines: Vec<&str> = out.lines().collect();
    assert!(lines[0].contains("[main.gbln:7]") && lines[0].contains("[main.gbln:2] division by zero"), "{out}");
    assert!(lines[1].contains("[main.gbln:10] division by zero"), "{out}");
    assert!(!err.contains("goblin: actor"), "{err}");
}

fn free_port() -> u16 { TcpListener::bind("127.0.0.1:0").unwrap().local_addr().unwrap().port() }

/// A startup actor that fails on a posted message shows up in the server's
/// log. Before, a background worker could die without a trace.
#[cfg(unix)]
#[test]
fn a_failing_startup_actor_shows_in_the_server_log() {
    let dir = scratch("server");
    std::fs::create_dir_all(dir.join("www/api")).unwrap();
    std::fs::write(dir.join("startup.gbln"), r#"act job(state, msg)
    x | 1 / 0
    return state
xx
jobs | :actor(0, job)
:post(jobs, 1)
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
    // A connect that reaches itself (the port is in the ephemeral range, so
    // the client can be handed the same port) is not the server.
    while TcpStream::connect(("127.0.0.1", port)).map_or(true, |s| s.local_addr().ok() == s.peer_addr().ok()) {
        assert!(Instant::now() < deadline, "server did not start");
        std::thread::sleep(Duration::from_millis(50));
    }
    Command::new("kill").args(["-TERM", &child.id().to_string()]).status().unwrap();
    let t0 = Instant::now();
    while child.try_wait().unwrap().is_none() {
        if t0.elapsed() > Duration::from_secs(10) { let _ = child.kill(); panic!("server did not stop"); }
        std::thread::sleep(Duration::from_millis(50));
    }
    let mut err = String::new();
    child.stderr.take().unwrap().read_to_string(&mut err).unwrap();
    assert!(err.contains("goblin: actor `job` failed: [startup.gbln:2] division by zero"), "{err}");
}

/// `restart: "on_failure"`: stderr says the actor restarted and how many
/// restarts it has left; once they run out it says it stays stopped.
#[test]
fn a_restarting_actor_says_so_on_stderr() {
    let (out, err) = run("restart-log", r#"act flaky(state, msg)
    if msg == "boom" => return 1 / 0
    return state + 1
xx
a | :actor(0, flaky, {restart: "on_failure", max_restarts: 1})
:post(a, "boom")
say(:await(:request(a, "ok")))
:post(a, "boom")
attempt
    :await(a)
rescue e
    say(:has("{e}", "C0112"))
xx
"#);
    assert_eq!(out, "1\ntrue\n");
    assert!(err.contains("goblin: actor `flaky` failed: [main.gbln:2] division by zero; restarted with its starting state (1 of 1 restarts this minute)"), "{err}");
    assert!(err.contains("it already restarted 1 times within a minute, so it stays stopped"), "{err}");
}

/// A restart cancels the actors the failed run started; cancelling a
/// restarting actor ends it, it does not restart.
#[test]
fn a_restart_cancels_the_failed_runs_actors_and_cancel_is_final() {
    let (out, err) = run("restart-children", r#"act child(state, ch)
    return :receive(ch)
xx
act parent(state, msg)
    if msg == "start"
        c | :actor(0, child)
        :post(c, :channel(1))
        return c
    xx
    return 1 / 0
xx
p | :actor(0, parent, {restart: "on_failure"})
c | :await(:request(p, "start"))
:post(p, "boom")
attempt
    :await(c)
rescue e
    say(:has("{e}", "C0116"))
xx
act slow(state, msg)
    return :receive(:channel(1))
xx
s | :actor(0, slow, {restart: "on_failure"})
f | :request(s, 1)
nap | :channel(1)
:receive(nap, 0.3s)
:cancel(s)
attempt
    :await(s)
rescue e
    say(:has("{e}", "C0116"))
xx
"#);
    assert_eq!(out, "true\ntrue\n");
    assert!(!err.contains("`slow`"), "a cancelled actor must not restart: {err}");
}

/// A startup actor that restarts on failure keeps serving requests after a
/// crash, and the server log says it restarted.
#[cfg(unix)]
#[test]
fn a_restarting_startup_actor_keeps_serving() {
    let dir = scratch("restart-server");
    std::fs::create_dir_all(dir.join("www/api")).unwrap();
    std::fs::write(dir.join("startup.gbln"), r#"act job(state, msg)
    if msg == "boom" => return 1 / 0
    return state + 1
xx
jobs | :actor(0, job, {restart: "on_failure"})
"#).unwrap();
    std::fs::write(dir.join("www/api/hit.gbln"), ":post(jobs, \"boom\")\nsay(:await(:request(jobs, \"ok\")))\n").unwrap();
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
    let get = |path: &str| -> String {
        use std::io::Write;
        let mut s = TcpStream::connect(("127.0.0.1", port)).unwrap();
        write!(s, "GET {path} HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n").unwrap();
        let mut b = String::new();
        s.read_to_string(&mut b).unwrap();
        b
    };
    assert!(get("/api/hit").contains('1'));
    assert!(get("/api/hit").contains('1'), "the restarted actor starts again from 0");
    Command::new("kill").args(["-TERM", &child.id().to_string()]).status().unwrap();
    let t0 = Instant::now();
    while child.try_wait().unwrap().is_none() {
        if t0.elapsed() > Duration::from_secs(10) { let _ = child.kill(); panic!("server did not stop"); }
        std::thread::sleep(Duration::from_millis(50));
    }
    let mut err = String::new();
    child.stderr.take().unwrap().read_to_string(&mut err).unwrap();
    assert_eq!(err.matches("goblin: actor `job` failed: [startup.gbln:2] division by zero; restarted").count(), 2, "{err}");
}
