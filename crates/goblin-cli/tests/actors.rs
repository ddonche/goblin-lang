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
    run_files(name, src, &[], workers, extra)
}

/// `run`, with more source files next to `main.gbln`.
fn run_files(name: &str, src: &str, files: &[(&str, &str)], workers: usize, extra: &[(&str, &str)]) -> String {
    let dir = std::env::temp_dir().join(format!("goblin-actors-{}-{name}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    for (f, text) in files { std::fs::write(dir.join(f), text).unwrap(); }
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

/// A handler waiting in `:receive()` for its own next message gives its
/// worker up: another actor on the only worker answers meanwhile, and the
/// message that arrives later finishes the first one's request.
#[test]
fn receive_from_its_own_mailbox_leaves_the_worker_free() {
    let src = r#"act gather(state, first)
    second | :receive()
    return [first, second]
xx
act echo(state, msg)
    return msg
xx
g | :actor([], gather)
e | :actor(0, echo)
f | :request(g, "one")
say(:await(:request(e, 7)))
:post(g, "two")
say(:await(f))
"#;
    let out = run("own-mailbox", src, 1, &[]);
    assert_eq!(out.trim(), "7\n[one, two]");
}

/// Halting an actor whose handler waits in `:receive()` gives that wait
/// nil, the handler finishes, and the actor ends; cancelling it fails the
/// request with C0116.
#[test]
fn halt_or_cancel_reaches_a_handler_waiting_on_its_own_mailbox() {
    let src = r#"act gather(state, first)
    return [first, :receive()]
xx
g | :actor([], gather)
f | :request(g, "one")
nap | :channel(1)
:receive(nap, 0.3s)
:halt(g)
say(:await(f))
say(:await(g))
h | :actor([], gather)
f2 | :request(h, "one")
:receive(nap, 0.3s)
:cancel(h)
attempt
    :await(f2)
rescue e
    say(:has("{e}", "C0116"))
xx
"#;
    let out = run("own-mailbox-halt", src, 1, &[]);
    assert_eq!(out.trim(), "[one, nil]\n[one, nil]\ntrue");
}

fn status_kb(out: &str, key: &str) -> usize {
    out.lines().find_map(|l| l.strip_prefix(key)).map(|n| n.trim().trim_end_matches("kB").trim().parse().unwrap()).unwrap()
}

/// Actors started from a program with large constant tables share one copy
/// of them per worker: two thousand actors next to a 20,000-entry map and a
/// long string stay small. Each used to get its own copy (over 2 GB here).
#[test]
fn actors_share_the_programs_constants() {
    let src = r#"act build(n)
    m | {}
    i | 0
    while i < n
        :update!(m{"key-{i}"}, "value number {i}")
        i |= i + 1
    xx
    return m
xx
act long(piece, n)
    t | ""
    i | 0
    while i < n
        t |= t + piece
        i |= i + 1
    xx
    return t
xx
TABLE | build(20000)
TEXT | long("long text ", 2000)
act look(state, k)
    return state + :len(TEXT) + :len(TABLE{k})
xx
act make(n)
    hs | []
    i | 0
    while i < n
        :put_last!(hs, :actor(0, look))
        i |= i + 1
    xx
    return hs
xx
hs | make(2000)
total | 0
for h in hs
    total |= total + :await(:request(h, "key-7"))
xx
say(total)
for l in :lines(:read_text("/proc/self/status"))
    if :starts_with(l, "VmHWM:")
        say(l)
    xx
xx
"#;
    let out = run("constants", src, 2, &[]);
    assert!(out.starts_with(&format!("{}\n", 2000 * (20000 + 14))), "{out}");
    assert!(status_kb(&out, "VmHWM:") < 400 * 1024, "{out}");
}

/// A shared constant is still each actor's own value: one actor changing it
/// does not change it for the others on its worker, or for the program.
#[test]
fn a_shared_constant_changed_in_one_actor_stays_put_in_the_others() {
    let src = r#"act long(piece, n)
    t | ""
    i | 0
    while i < n
        t |= t + piece
        i |= i + 1
    xx
    return t
xx
NAMES | ["ann", "bob", "cy"]
TEXT | long("abc", 200)
act change(state, msg)
    :put_last!(NAMES, msg)
    TEXT |= TEXT + msg
    return {"n": :len(NAMES), "t": :len(TEXT)}
xx
act read(state, msg)
    return {"n": :len(NAMES), "t": :len(TEXT)}
xx
a | :actor({}, change)
b | :actor({}, read)
say(:await(:request(a, "dee")))
say(:await(:request(a, "eve")))
say(:await(:request(b, 0)))
c | :actor({}, read)
say(:await(:request(c, 0)))
say(:len(NAMES))
"#;
    assert_eq!(run("cow", src, 1, &[]).trim(),
        "{n: 4, t: 603}\n{n: 5, t: 606}\n{n: 3, t: 600}\n{n: 3, t: 600}\n3");
}

/// An actor started after an import sees the imported actions, though the
/// actors started before it shared the program as it was then.
#[test]
fn actors_started_after_an_import_see_it() {
    let lib = "act triple(x)\n    return x * 3\nxx\n";
    let src = r#"act first(state, msg)
    return msg + 1
xx
a | :actor(0, first)
say(:await(:request(a, 1)))
import extra as ex
act second(state, msg)
    return ex::triple(msg)
xx
b | :actor(0, second)
say(:await(:request(b, 5)))
c | :actor(0, first)
say(:await(:request(c, 9)))
"#;
    assert_eq!(run_files("import", src, &[("extra.gbln", lib)], 1, &[]).trim(), "2\n15\n10");
}

/// Actors get only the globals their code can use: a global list of actor
/// handles, kept by the program's top-level code, is not copied into each
/// new actor. Starting n actors used to cost O(n²) (10,000 took 2 GB).
#[test]
fn a_global_list_of_actors_is_not_copied_into_each_actor() {
    let src = r#"act count(state, msg)
    return state + msg
xx
hs | []
i | 0
while i < 20000
    :put_last!(hs, :actor(0, count))
    i |= i + 1
xx
total | 0
for h in hs
    total |= total + :await(:request(h, 1))
xx
say(total)
for l in :lines(:read_text("/proc/self/status"))
    if :starts_with(l, "VmHWM:")
        say(l)
    xx
xx
"#;
    let out = run("global-list", src, 2, &[]);
    assert!(out.starts_with("20000\n"), "{out}");
    assert!(status_kb(&out, "VmHWM:") < 400 * 1024, "{out}");
}

/// Every way an actor's code can use a global still finds it: a direct
/// read, a call to an action that reads one, interpolation, a closure kept
/// in a global, a module's variable through its alias, and a write.
#[test]
fn an_actor_still_sees_every_global_its_code_uses() {
    let lib = "RATE | 3\nact rate()\n    return RATE\nxx\n";
    let src = r#"import extra as ex
BASE | 100
LABEL | "total"
LIMIT | 7
COUNTER | 0
act limit()
    return LIMIT
xx
act add(state, msg)
    COUNTER |= COUNTER + 1
    return state + msg + BASE + limit() + ex::rate() + ex::RATE
xx
act show(state, msg)
    return "{LABEL}: {state} after {COUNTER}"
xx
act handle(state, msg)
    n | add(state, msg)
    return show(n, 0)
xx
a | :actor(0, handle)
say(:await(:request(a, 1)))
"#;
    assert_eq!(run_files("globals-used", src, &[("extra.gbln", lib)], 1, &[]).trim(), "total: 114 after 1");
}

/// A class field default that reads a global runs inside the actor when it
/// makes an object, so that global is copied into the actor too.
#[test]
fn an_actor_sees_globals_its_class_defaults_read() {
    let src = r#"xs | [1, 2, 3]
<>Box |
    name: ""
    size: :len(xs)
xx
act handle(state, msg)
    b <> Box | name: msg
    return b >> size
xx
a | :actor(0, handle)
say(:await(:request(a, "x")))
"#;
    assert_eq!(run_files("class-defaults", src, &[], 1, &[]).trim(), "3");
}
