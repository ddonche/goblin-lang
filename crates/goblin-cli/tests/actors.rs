//! Lightweight actors (concurrency spec stage 4): many actors share a few
//! worker threads, a handler that waits gives its worker to the others, and
//! a handler that runs long cannot starve the actors next to it.
//! Each case runs the `goblin` binary with a fixed number of workers.

use std::path::PathBuf;
use std::process::Command;
use std::time::{Duration, Instant};

fn bin() -> PathBuf { PathBuf::from(env!("CARGO_BIN_EXE_goblin")) }

/// Run `src` with `workers` actor threads; stdout, or a panic with stderr.
fn run(name: &str, src: &str, workers: usize, extra: &[(&str, &str)]) -> String {
    let dir = std::env::temp_dir().join(format!("goblin-actors-{}-{name}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let file = dir.join("main.gbln");
    std::fs::write(&file, src).unwrap();
    let mut cmd = Command::new(bin());
    cmd.arg("run").arg(&file).current_dir(&dir)
        .env("GOBLIN_ACTOR_THREADS", workers.to_string())
        .env_remove("GOBLIN_ENGINE")
        .env_remove("GOBLIN_MAX_ACTORS");
    for (k, v) in extra { cmd.env(k, v); }
    let out = cmd.output().unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout).to_string();
    assert!(out.status.success(), "{name} failed:\n{stdout}\n{}", String::from_utf8_lossy(&out.stderr));
    stdout
}

fn threads_line(out: &str) -> usize {
    out.lines().find_map(|l| l.strip_prefix("Threads:")).map(|n| n.trim().parse().unwrap()).unwrap()
}

/// Ten thousand actors, each waiting in its handler on one channel, live on
/// two worker threads. The old one-thread-per-actor version stopped at 256.
#[test]
fn ten_thousand_waiting_actors_share_two_threads() {
    let src = r#"act take(total, ch)
    return total + :receive(ch)
xx
ch | :channel(10)
n | 10000
hs | []
i | 0
while i < n
    a | :actor(0, take)
    :post(a, ch)
    :put_last!(hs, a)
    i |= i + 1
xx
for l in :lines(:read_text("/proc/self/status"))
    if :starts_with(l, "Threads:")
        say(l)
    xx
xx
i |= 0
while i < n
    :post(ch, 1)
    i |= i + 1
xx
total | 0
i |= 0
while i < n
    :halt(hs[i])
    total |= total + :await(hs[i])
    i |= i + 1
xx
say(total)
"#;
    let out = run("many", src, 2, &[]);
    assert!(out.trim_end().ends_with("10000"), "{out}");
    // Main thread, two workers, the timer thread at most, plus slack for
    // the runtime's own helpers.
    assert!(threads_line(&out) <= 6, "{out}");
}

/// With one worker, an actor that awaits another actor's answer inside its
/// handler must not block the worker the other actor needs.
#[test]
fn awaiting_an_actor_on_the_same_worker_does_not_deadlock() {
    let src = r#"act inner(state, msg)
    return state + msg
xx
act outer(state, msg)
    got | :await(:request(state{"b"}, msg))
    :update!(state{"n"}, got)
    return state
xx
b | :actor(0, inner)
a | :actor({"b": b, "n": 0}, outer)
i | 0
while i < 500
    :await(:request(a, 1))
    i |= i + 1
xx
:halt(a)
say(:await(a){"n"})
"#;
    assert_eq!(run("same-worker", src, 1, &[]).trim(), "500");
}

/// Producer and consumer on one worker, through a channel far smaller than
/// what passes through it: both stop and go on many times.
#[test]
fn channel_pipeline_on_one_worker() {
    let src = r#"act produce(state, msg)
    i | 0
    ch | msg{"ch"}
    while i < msg{"n"}
        :post(ch, i)
        i |= i + 1
    xx
    :halt(ch)
    return state + 1
xx
act consume(total, ch)
    sum | 0
    v | :receive(ch)
    while v != nil
        sum |= sum + v
        v |= :receive(ch)
    xx
    return total + sum
xx
ch | :channel(4)
p | :actor(0, produce)
c | :actor(0, consume)
:post(c, ch)
:post(p, {"ch": ch, "n": 20000})
:halt(p)
:halt(c)
say(:await(c))
"#;
    assert_eq!(run("pipeline", src, 1, &[]).trim(), "199990000");
}

/// A handler that loops for a long time gives its worker up now and then:
/// with one worker, a quick actor still answers long before it finishes.
#[test]
fn a_long_handler_does_not_starve_its_neighbours() {
    let src = r#"act slow(state, ch)
    i | 0
    while i < 3000000
        i |= i + 1
    xx
    :post(ch, "slow")
    return state
xx
act quick(state, ch)
    :post(ch, "quick")
    return state
xx
ch | :channel(4)
s | :actor(0, slow)
q | :actor(0, quick)
:post(s, ch)
:post(q, ch)
say(:receive(ch))
say(:receive(ch))
"#;
    assert_eq!(run("fair", src, 1, &[]).trim(), "quick\nslow");
}

/// A wait inside a builtin that calls back into Goblin (here `:map_fn`)
/// cannot stop the handler, so the worker runs other actors while it waits.
#[test]
fn waiting_inside_a_callback_on_one_worker() {
    let src = r#"act double(state, msg)
    return msg * 2
xx
act ask(h)
    return :request(h, 21)
xx
act wait_for(f)
    return :await(f)
xx
act gather(state, msg)
    fs | :map_fn(state, "ask")
    return :map_fn(fs, "wait_for")
xx
hs | [:actor(0, double), :actor(0, double), :actor(0, double)]
g | :actor(hs, gather)
say(:await(:request(g, 0)))
"#;
    assert_eq!(run("callback", src, 1, &[]).trim(), "[42, 42, 42]");
}

/// An error raised where a handler waited is caught by the `attempt`
/// around the wait, after the handler went on.
#[test]
fn errors_at_a_wait_reach_the_handlers_rescue() {
    let src = r#"act broken(state, msg)
    return nil
xx
act asker(state, msg)
    attempt
        :await(:request(state{"b"}, msg))
        :update!(state{"seen"}, "no error")
    rescue e
        :update!(state{"seen"}, :has("{e}", "returned nil"))
    xx
    return state
xx
b | :actor(1, broken)
a | :actor({"b": b, "seen": nil}, asker)
say(:await(:request(a, 1)){"seen"})
"#;
    assert_eq!(run("rescue", src, 1, &[]).trim(), "true");
}

/// `:receive(ch, limit)` inside a handler gives nil when nothing arrives in
/// time, without holding the worker: another actor answers meanwhile.
#[test]
fn receive_with_a_limit_inside_a_handler() {
    let src = r#"act waiter(state, ch)
    v | :receive(ch, 0.3s)
    return {"got": v}
xx
act echo(state, msg)
    return msg
xx
ch | :channel(1)
w | :actor({"got": 0}, waiter)
e | :actor(0, echo)
:post(w, ch)
say(:await(:request(e, 7)))
say(:await(:request(w, ch)){"got"})
"#;
    let t = Instant::now();
    let out = run("limit", src, 1, &[]);
    assert_eq!(out.trim(), "7\nnil");
    assert!(t.elapsed() < Duration::from_secs(10));
}
