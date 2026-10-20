//! What `goblin start` sends for a VM script: status line, headers in key
//! order, cookies, body. The VM hands its response to the server directly
//! (it used to go through JSON text that the server parsed back); these bytes
//! are what the JSON route produced, so the change is invisible on the wire.

use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};

struct Server(Child);
impl Drop for Server {
    fn drop(&mut self) { let _ = self.0.kill(); let _ = self.0.wait(); }
}

fn raw(port: u16, path: &str) -> String {
    let mut s = TcpStream::connect(("127.0.0.1", port)).unwrap();
    s.set_read_timeout(Some(Duration::from_secs(10))).unwrap();
    write!(s, "GET {path} HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n").unwrap();
    let mut out = String::new();
    s.read_to_string(&mut out).unwrap();
    out
}

#[test]
fn vm_responses_on_the_wire() {
    let dir = std::env::temp_dir().join(format!("goblin-host-response-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(dir.join("www/api")).unwrap();
    let w = |p: &str, src: &str| std::fs::write(dir.join("www").join(p), src).unwrap();
    w("api/plain.gbln", "say(\"hello\")\n");
    w("api/headers.gbln", concat!(
        ":set_status(201)\n",
        ":set_header(\"X-Zeta\", \"last\")\n",
        ":set_header(\"Content-Type\", \"text/html; charset=utf-8\")\n",
        ":set_header(\"X-Alpha\", \"first\")\n",
        ":set_cookie(\"a\", \"1\")\n",
        ":set_cookie(\"b\", \"2\", {\"http_only\": true, \"path\": \"/\"})\n",
        "say(\"<p>h\u{e9}llo \\\"quoted\\\" \\\\ back</p>\")\n"));
    w("api/redirect.gbln", ":set_status(303)\n:set_header(\"Location\", \"/somewhere\")\nsay(\"ignored body\")\n");
    w("page.gbln", ":set_header(\"X-Page\", \"yes\")\nsay(\"static page\")\n");

    let port = TcpListener::bind("127.0.0.1:0").unwrap().local_addr().unwrap().port();
    let child = Command::new(PathBuf::from(env!("CARGO_BIN_EXE_goblin")))
        .args(["start", "--host", "127.0.0.1", "--port", &port.to_string()])
        .current_dir(&dir).env("GOBLIN_DOCROOT", dir.join("www")).env_remove("PORT")
        .stdout(Stdio::null()).stderr(Stdio::null()).spawn().unwrap();
    let _server = Server(child);
    let deadline = Instant::now() + Duration::from_secs(20);
    while TcpStream::connect(("127.0.0.1", port)).is_err() {
        assert!(Instant::now() < deadline, "server did not start");
        std::thread::sleep(Duration::from_millis(50));
    }

    let head = "Connection: close\r\nx-goblin-web-contract: v0\r\n";
    assert_eq!(raw(port, "/api/plain"), format!(
        "HTTP/1.1 200 OK\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: 5\r\n{head}\r\nhello"));
    assert_eq!(raw(port, "/api/headers"), format!(concat!(
        "HTTP/1.1 201 Created\r\nContent-Type: text/html; charset=utf-8\r\nContent-Length: 29\r\n{}",
        "Content-Type: text/html; charset=utf-8\r\nX-Alpha: first\r\nX-Zeta: last\r\n",
        "Set-Cookie: a=1\r\nSet-Cookie: b=2; HttpOnly; Path=/\r\n\r\n",
        "<p>h\u{e9}llo \"quoted\" \\ back</p>"), head));
    assert_eq!(raw(port, "/api/redirect"), format!(
        "HTTP/1.1 303 See Other\r\nContent-Length: 0\r\n{head}Location: /somewhere\r\n\r\n"));
    assert_eq!(raw(port, "/page.gbln"), format!(
        "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=utf-8\r\nContent-Length: 11\r\n{head}X-Page: yes\r\n\r\nstatic page"));
    let _ = std::fs::remove_dir_all(&dir);
}
