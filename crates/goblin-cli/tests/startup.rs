//! Startup scripts (`goblin start --startup FILE`, `GOBLIN_STARTUP=FILE`):
//! the script runs once, every request gets its own copy of its variables, its
//! actors are shared by all requests, and they finish their accepted messages
//! when the server stops.

use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};

fn bin() -> PathBuf { PathBuf::from(env!("CARGO_BIN_EXE_goblin")) }

fn scratch(name: &str) -> PathBuf {
    let d = std::env::temp_dir().join(format!("goblin-startup-{}-{name}", std::process::id()));
    let _ = std::fs::remove_dir_all(&d);
    std::fs::create_dir_all(d.join("www/api")).unwrap();
    d
}

fn free_port() -> u16 { TcpListener::bind("127.0.0.1:0").unwrap().local_addr().unwrap().port() }

fn get(port: u16, path: &str) -> Option<String> {
    let mut s = TcpStream::connect(("127.0.0.1", port)).ok()?;
    s.set_read_timeout(Some(Duration::from_secs(10))).ok()?;
    write!(s, "GET {path} HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n").ok()?;
    let mut out = String::new();
    s.read_to_string(&mut out).ok()?;
    Some(out.split_once("\r\n\r\n").map(|(_, b)| b.to_string()).unwrap_or(out))
}

struct Server(Child);
impl Drop for Server {
    fn drop(&mut self) { let _ = self.0.kill(); let _ = self.0.wait(); }
}

fn start(dir: &Path, startup: &str, port: u16) -> Server {
    let child = Command::new(bin())
        .args(["start", "--host", "127.0.0.1", "--port", &port.to_string(), "--startup", startup])
        .current_dir(dir)
        .env("GOBLIN_DOCROOT", dir.join("www"))
        .stdout(Stdio::null()).stderr(Stdio::null())
        .spawn().unwrap();
    let deadline = Instant::now() + Duration::from_secs(20);
    while TcpStream::connect(("127.0.0.1", port)).is_err() {
        assert!(Instant::now() < deadline, "server did not start");
        std::thread::sleep(Duration::from_millis(50));
    }
    Server(child)
}

const STARTUP: &str = r#"act count(state, msg)
    return state + msg
xx
counter | :actor(0, count)
note | "start"
room | {"name": "lobby"}
rooms | :actor(room, count)
"#;

const HIT: &str = r#"say(note)
note |= "changed by a request"
:post(counter, 1)
say(:await(:request(counter, 0)))
attempt
    say(room)
rescue e
    say(:has("{e}", "C0115"))
xx
"#;

#[test]
fn requests_share_startup_actors_but_copy_its_variables() {
    let dir = scratch("server");
    std::fs::write(dir.join("startup.gbln"), STARTUP).unwrap();
    std::fs::write(dir.join("www/api/hit.gbln"), HIT).unwrap();
    let port = free_port();
    let _server = start(&dir, "startup.gbln", port);
    for n in 1..=3 {
        let body = get(port, "/api/hit").expect("request failed");
        // each request sees the startup value, not the previous request's change;
        // the counter actor is the same one every time; `room` was handed to an actor
        assert!(body.contains("start"), "request {n}: {body}");
        assert!(body.contains(&format!("{n}")), "request {n}: {body}");
        assert!(body.contains("true"), "request {n}: {body}");
        assert!(!body.contains("changed by a request"), "request {n}: {body}");
    }
}

#[test]
fn goblin_run_with_startup_env() {
    let dir = scratch("run");
    std::fs::write(dir.join("startup.gbln"), STARTUP).unwrap();
    std::fs::write(dir.join("hit.gbln"), HIT).unwrap();
    let out = Command::new(bin()).args(["run", "hit.gbln"]).current_dir(&dir)
        .env("GOBLIN_STARTUP", "startup.gbln").output().unwrap();
    assert!(out.status.success(), "{}", String::from_utf8_lossy(&out.stderr));
    assert_eq!(String::from_utf8_lossy(&out.stdout), "start\n1\ntrue\n");
}

#[test]
fn startup_actors_finish_their_messages_when_the_server_stops() {
    let dir = scratch("stop");
    let log = dir.join("log.txt");
    std::fs::write(dir.join("startup.gbln"), format!(r#"act slow(state, msg)
    i | 0
    while i < 200000
        i |= i + 1
    xx
    :append_file!("{}", :str(msg) + "\n")
    return state
xx
writer | :actor(0, slow)
"#, log.display())).unwrap();
    std::fs::write(dir.join("www/api/queue.gbln"), ":post(writer, 1)\n:post(writer, 2)\n:post(writer, 3)\nsay(\"queued\")\n").unwrap();
    let port = free_port();
    let mut server = start(&dir, "startup.gbln", port);
    assert!(get(port, "/api/queue").unwrap().contains("queued"));
    #[cfg(unix)]
    {
        Command::new("kill").args(["-TERM", &server.0.id().to_string()]).status().unwrap();
        let deadline = Instant::now() + Duration::from_secs(20);
        while server.0.try_wait().unwrap().is_none() {
            assert!(Instant::now() < deadline, "server did not stop");
            std::thread::sleep(Duration::from_millis(50));
        }
        assert_eq!(std::fs::read_to_string(&log).unwrap(), "1\n2\n3\n");
    }
}

#[test]
fn a_failing_startup_script_stops_the_server_from_starting() {
    let dir = scratch("bad");
    std::fs::write(dir.join("startup.gbln"), "x | 1 / 0\n").unwrap();
    let out = Command::new(bin())
        .args(["start", "--host", "127.0.0.1", "--port", &free_port().to_string(), "--startup", "startup.gbln"])
        .current_dir(&dir).output().unwrap();
    assert_eq!(out.status.code(), Some(1));
    assert!(String::from_utf8_lossy(&out.stderr).contains("startup script startup.gbln failed"));
}
