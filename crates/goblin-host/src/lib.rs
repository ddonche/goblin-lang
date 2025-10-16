//! goblin-host: Embedded HTTP host engine for Goblin (skeleton v0)

use std::path::PathBuf;
use std::fmt;
use std::path::Path;
use std::time::{SystemTime, UNIX_EPOCH};
use std::time::{Instant};

// ---------- Contract-facing types (names only, no logic yet) ----------

#[derive(Clone, Debug)]
pub struct Limits {
    pub max_headers_bytes: usize,
    pub max_body_bytes: usize,
    pub idle_timeout_ms: u64,
    pub max_conns: usize,
    pub header_line_bytes: usize,
}

impl Default for Limits {
    fn default() -> Self {
        Self {
            max_headers_bytes: 16 * 1024,
            header_line_bytes: 8 * 1024,
            max_body_bytes: 10 * 1024 * 1024,
            idle_timeout_ms: 60_000,
            max_conns: 10_000,
        }
    }
}

impl fmt::Debug for Step {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Step::Static(cfg) => f.debug_tuple("Static").field(cfg).finish(),
            Step::Proxy(cfg)  => f.debug_tuple("Proxy").field(cfg).finish(),
            Step::App(_)      => f.write_str("App(<webapp>)"),
        }
    }
}

// -----------------------------HELPERS---------------------------------
struct AccessLog {
    start: Instant,
    method: String,
    path: String,
}

impl AccessLog {
    fn new(method: &str, path: &str) -> Self {
        Self {
            start: Instant::now(),
            method: method.to_string(),
            path: path.to_string(),
        }
    }

    // NOTE: &self (does NOT move/consume the logger)
    fn done(&self, status: u16, bytes: usize) {
        let ms = self.start.elapsed().as_millis();
        println!("{} {} → {} {}B in {}ms", self.method, self.path, status, bytes, ms);
    }
}

/// Normalize an ETag token (handle W/ prefix, optional quotes/spaces).
/// Examples:
///   W/"abc"  -> W/abc
///   "abc"    -> abc        (no W/ prefix)
///   W/ "abc" -> W/abc
fn normalize_etag_token(s: &str) -> String {
    let s = s.trim();

    // Extract optional W/ prefix (with or without a following space)
    let (weak_prefix, rest) = if let Some(rest) = s.strip_prefix("W/") {
        ("W/", rest.trim_start())
    } else if let Some(rest) = s.strip_prefix("W/ ") {
        ("W/", rest.trim_start())
    } else {
        ("", s)
    };

    // Trim one pair of surrounding quotes if present
    let rest = rest.strip_prefix('"').and_then(|r| r.strip_suffix('"')).unwrap_or(rest);

    if weak_prefix.is_empty() {
        rest.to_string()
    } else {
        // Keep the W/ marker but store without quotes for comparison
        format!("W/{}", rest)
    }
}

fn http_date(ts: SystemTime) -> String {
    // very small RFC 1123 formatter (GMT)
    use chrono::{DateTime, Utc};
    let dt: DateTime<Utc> = ts.into();
    dt.format("%a, %d %b %Y %H:%M:%S GMT").to_string()
}

fn weak_etag(len: u64, mtime: Option<SystemTime>) -> String {
    let t = mtime
        .and_then(|t| t.duration_since(UNIX_EPOCH).ok())
        .map(|d| d.as_secs())
        .unwrap_or(0);
    format!("W/\"{:x}-{:x}\"", len, t)
}

/// Split an If-None-Match header into normalized tags (e.g., `W/"abc"`).
#[allow(dead_code)]
fn parse_if_none_match(raw: &str) -> Vec<String> {
    raw.split(',')
        .map(|s| s.trim())
        .filter(|s| !s.is_empty())
        .map(|tag| {
            // Many clients include quotes; keep W/ prefix if present but normalize spacing.
            // We’ll normalize by removing surrounding quotes around the opaque value.
            // Examples we accept:
            //  - W/"abc"   -> W/"abc"
            //  - "abc"     -> "abc"   (we’ll still compare with quotes kept on our etag)
            //  - W/ "abc"  -> W/"abc"
            //  - W/"a-b"   -> W/"a-b"
            let mut t = tag.to_string();

            // Collapse optional space after W/
            if t.starts_with("W/ ") {
                t = format!("W/{}", t.trim_start_matches("W/ ").trim_start());
            }

            // Remove extra spaces around quotes like W/ "abc" -> W/"abc"
            if let Some(pos) = t.find('"') {
                // ensure there’s no stray space before the quote
                if pos > 0 && t.as_bytes()[pos - 1].is_ascii_whitespace() {
                    t.remove(pos - 1);
                }
            }

            t
        })
        .collect()
}

#[derive(Clone, Debug)]
pub struct Caps {
    pub fs_read: bool,
    pub fs_write: bool,
    pub net_egress: bool,
    pub process_env: bool,
    pub timers: bool,
}

impl Default for Caps {
    fn default() -> Self {
        Self { fs_read: true, fs_write: true, net_egress: true, process_env: true, timers: true }
    }
}

// Request/Response handles are opaque for now; we’ll flesh these out.
#[derive(Debug)]
pub struct Request;
#[derive(Debug)]
pub struct Response;
#[derive(Debug, Clone)]
pub struct Ctx;

// ---------- Pipeline configuration ----------

#[derive(Clone, Debug)]
pub struct StaticCfg {
    pub dir: PathBuf,
    pub mount: String, // e.g., "/"
    pub spa_fallback: bool,
}

#[derive(Clone, Debug)]
pub struct ProxyCfg {
    pub prefix: String, // e.g., "/api"
    pub target: String, // e.g., "http://localhost:8080"
}

pub trait WebApp: Send + Sync {
    fn handle(&self, _req: Request, _res: Response, _ctx: Ctx);
}

pub enum Step {
    Static(StaticCfg),
    Proxy(ProxyCfg),
    App(Box<dyn WebApp>),
}

#[derive(Clone, Debug)]
pub struct ProxyRule {
    pub prefix: String,
    pub target: String,
    /// If true, strip `prefix` when building upstream path.
    pub strip_prefix: bool,
}

// ---------- Host builder & lifecycle ----------

#[derive(Debug)]
pub struct HostConfig {
    pub host: String, // "0.0.0.0"
    pub port: u16,    // 5173 by default in dev
    pub limits: Limits,
    pub caps: Caps,
    pub steps: Vec<Step>,
    pub proxies: Vec<ProxyRule>,
}

impl Default for HostConfig {
    fn default() -> Self {
        Self {
            host: "0.0.0.0".to_string(),
            port: 5173,
            limits: Limits::default(),
            caps: Caps::default(),
            steps: Vec::new(),
            proxies: Vec::new(),
        }
    }
}

pub struct HostBuilder {
    cfg: HostConfig,
}

impl HostBuilder {
    pub fn new() -> Self { Self { cfg: HostConfig::default() } }

    pub fn bind(mut self, host: impl Into<String>, port: u16) -> Self {
        self.cfg.host = host.into();
        self.cfg.port = port;
        self
    }

    pub fn proxy(mut self, prefix: impl Into<String>, target: impl Into<String>) -> Self {
        self.cfg.proxies.push(ProxyRule {
            prefix: prefix.into(),
            target: target.into(),
            strip_prefix: true,
        });
        self
    }

    pub fn limits(mut self, limits: Limits) -> Self { self.cfg.limits = limits; self }

    pub fn caps(mut self, caps: Caps) -> Self { self.cfg.caps = caps; self }

    pub fn add_static(mut self, cfg: StaticCfg) -> Self { self.cfg.steps.push(Step::Static(cfg)); self }

    pub fn add_proxy(mut self, cfg: ProxyCfg) -> Self { self.cfg.steps.push(Step::Proxy(cfg)); self }

    pub fn add_app(mut self, app: Box<dyn WebApp>) -> Self { self.cfg.steps.push(Step::App(app)); self }

    pub fn build(self) -> Host { Host { cfg: self.cfg } }
}

pub struct Host {
    cfg: HostConfig,
}

fn find_double_crlf(buf: &[u8]) -> Option<usize> {
    // return index just before the CRLFCRLF boundary
    buf.windows(4)
        .position(|w| w == b"\r\n\r\n")
        .map(|i| i + 4)
}

fn guess_mime(ext: &str) -> &'static str {
    match ext {
        "html" | "htm" => "text/html; charset=utf-8",
        "css"          => "text/css; charset=utf-8",
        "js"           => "application/javascript; charset=utf-8",
        "json"         => "application/json; charset=utf-8",
        "svg"          => "image/svg+xml",
        "png"          => "image/png",
        "jpg" | "jpeg" => "image/jpeg",
        "gif"          => "image/gif",
        "txt"          => "text/plain; charset=utf-8",
        _              => "application/octet-stream",
    }
}

/// Safe-ish join: denies absolute paths and any `..` segments.
fn safe_join(root: &Path, req_path: &str) -> Option<PathBuf> {
    if req_path.contains('\0') { return None; }

    // Strip query/fragment if present
    let p = req_path.split(['?', '#']).next().unwrap_or("");

    // Strip leading slash to keep relative
    let p = p.trim_start_matches('/');

    let mut out = PathBuf::from(root);
    for seg in Path::new(p) {
        let seg = seg.to_string_lossy();
        if seg.is_empty() { continue; }
        if seg == "." || seg == ".." { return None; }
        out.push(seg.as_ref());
    }
    Some(out)
}

impl Host {
    pub async fn run(&mut self) -> Result<(), HostError> {
        use tokio::net::TcpListener;

        let addr = format!("{}:{}", self.cfg.host, self.cfg.port);
        let listener = TcpListener::bind(&addr)
            .await
            .map_err(|e| HostError::Bind(e.to_string()))?;

        println!("GoblinHost listening on {}", addr);

        loop {
            tokio::select! {
                // Accept next connection
                accept_res = listener.accept() => {
                    let (mut socket, peer) = match accept_res {
                        Ok(pair) => pair,
                        Err(e) => {
                            eprintln!("accept error: {e}");
                            continue;
                        }
                    };

                    println!("Accepted connection from {}", peer);
                    let idle_ms = 60_000; // or: self.cfg.limits.idle_timeout_ms
                    let proxies = self.cfg.proxies.clone(); 

                    tokio::spawn(async move {
                        use tokio::io::{AsyncReadExt, AsyncWriteExt};
                        use tokio::fs;
                        use tokio::time::{timeout, Duration};
                        use std::collections::HashMap;
                        use std::path::PathBuf;

                        // placeholder logger so early exits (431/400) can log
                        let mut log = AccessLog::new("?", "?");

                        // Reusable socket loop: handle multiple requests on the same connection.
                        // We’ll close when the client requests it or on idle timeout/error.
                        'conn: loop {
                            // ---- read until CRLFCRLF (end of headers) with idle timeout ----
                            let mut buf = Vec::with_capacity(4096);
                            let mut tmp = [0u8; 512];

                            let request = loop {
                                // apply idle timeout to each read chunk
                                let read_res = timeout(Duration::from_millis(idle_ms), socket.read(&mut tmp)).await;
                                let n = match read_res {
                                    Err(_) => { // idle timeout
                                        // no response; just close the connection quietly
                                        break None;
                                    }
                                    Ok(Ok(0)) => break None, // client closed
                                    Ok(Ok(n)) => n,
                                    Ok(Err(e)) => {
                                        eprintln!("read error from {}: {}", peer, e);
                                        break None;
                                    }
                                };

                                buf.extend_from_slice(&tmp[..n]);

                                if let Some(pos) = find_double_crlf(&buf) {
                                    let head = buf[..pos].to_vec();
                                    break Some(head);
                                }

                                if buf.len() > 16 * 1024 {
                                    let resp = b"HTTP/1.1 431 Request Header Fields Too Large\r\n\
                                                 Connection: close\r\n\
                                                 Content-Length: 0\r\n\r\n";
                                    let _ = socket.write_all(resp).await;
                                    log.done(431, 0);
                                    break 'conn;
                                }
                            };

                            // If no header block, close the connection.
                            let Some(head) = request else {
                                break 'conn;
                            };

                            let head_str = match std::str::from_utf8(&head) {
                                Ok(s) => s,
                                Err(_) => {
                                    let resp = b"HTTP/1.1 400 Bad Request\r\n\
                                                 Connection: close\r\n\
                                                 Content-Length: 0\r\n\r\n";
                                    let _ = socket.write_all(resp).await;
                                    log.done(400, 0);
                                    break 'conn;
                                }
                            };

                            // ---- parse request line ----
                            let mut lines = head_str.split("\r\n");
                            let Some(req_line) = lines.next() else {
                                let resp = b"HTTP/1.1 400 Bad Request\r\n\
                                             Connection: close\r\n\
                                             Content-Length: 0\r\n\r\n";
                                let _ = socket.write_all(resp).await;
                                log.done(400, 0);
                                break 'conn;
                            };
                            let mut parts = req_line.split_whitespace();
                            let method = parts.next().unwrap_or("");
                            let path   = parts.next().unwrap_or("/");
                            let ver    = parts.next().unwrap_or("HTTP/1.1");

                            // now that we know method/path, re-init the logger
                            log = AccessLog::new(method, path);

                            // ---- parse headers into a map (lowercased names) ----
                            let mut headers_map: HashMap<String, String> = HashMap::new();
                            for line in lines {
                                if line.is_empty() { break; }
                                if let Some((name, val)) = line.split_once(':') {
                                    headers_map.insert(name.trim().to_ascii_lowercase(), val.trim().to_string());
                                }
                            }

                            // Connection strategy (HTTP/1.1 keep-alive by default; /1.0 is close unless keep-alive)
                            let conn_req = headers_map.get("connection").map(|s| s.to_ascii_lowercase());
                            let mut want_close = false;
                            let connection_header = if ver.eq_ignore_ascii_case("HTTP/1.0") {
                                // HTTP/1.0 defaults to close unless client sent 'keep-alive'
                                if matches!(conn_req.as_deref(), Some("keep-alive")) {
                                    "keep-alive"
                                } else {
                                    want_close = true;
                                    "close"
                                }
                            } else {
                                // HTTP/1.1 defaults to keep-alive unless client sent 'close'
                                if matches!(conn_req.as_deref(), Some("close")) {
                                    want_close = true;
                                    "close"
                                } else {
                                    "keep-alive"
                                }
                            };

                            let if_none_match: Option<&str> = headers_map.get("if-none-match").map(String::as_str);

                            // ---- built-in endpoints ----
                            if path == "/_health" {
                                let body = r#"{"ok":true}"#;
                                let headers = format!(
                                    "HTTP/1.1 200 OK\r\n\
                                     Content-Type: application/json; charset=utf-8\r\n\
                                     Content-Length: {}\r\n\
                                     Connection: {connection_header}\r\n\
                                     x-goblin-web-contract: {}\r\n\
                                     \r\n",
                                    body.len(),
                                    crate::CONTRACT_VERSION
                                );
                                if socket.write_all(headers.as_bytes()).await.is_ok() {
                                    let _ = socket.write_all(body.as_bytes()).await;
                                }
                                log.done(200, body.len());
                                if want_close { break 'conn; } else { continue 'conn; }
                            }

                            if path == "/_info" {
                                // Detect dev "mode" based on presence of ./public
                                let public = std::path::PathBuf::from("./public");
                                let mode = if public.is_dir() { "static" } else { "basic" };
                                let body = format!(
                                    r#"{{"ok":true,"mode":"{}","contract":"{}"}}"#,
                                    mode,
                                    crate::CONTRACT_VERSION
                                );
                                let headers = format!(
                                    "HTTP/1.1 200 OK\r\n\
                                     Content-Type: application/json; charset=utf-8\r\n\
                                     Content-Length: {}\r\n\
                                     Connection: {connection_header}\r\n\
                                     x-goblin-web-contract: {}\r\n\
                                     \r\n",
                                    body.len(),
                                    crate::CONTRACT_VERSION
                                );
                                if socket.write_all(headers.as_bytes()).await.is_ok() {
                                    let _ = socket.write_all(body.as_bytes()).await;
                                }
                                log.done(200, body.len());
                                if want_close { break 'conn; } else { continue 'conn; }
                            }

                            // === Dynamic Goblin API Execution (via CLI) ===
                            if path.starts_with("/api/") {
                                use tokio::io::AsyncWriteExt; // ensure in scope

                                let api_rel = path.trim_start_matches("/api/");
                                let script_path = std::path::PathBuf::from("./api").join(format!("{api_rel}.gbln"));

                                if script_path.exists() {
                                    match exec_goblin_script_via_cli_timeout(&script_path, 5000).await {
                                        Ok(body) => {
                                            let headers = format!(
                                                "HTTP/1.1 200 OK\r\n\
                                                 Content-Type: text/plain; charset=utf-8\r\n\
                                                 Content-Length: {}\r\n\
                                                 Connection: {connection_header}\r\n\
                                                 x-goblin-web-contract: {}\r\n\
                                                 \r\n",
                                                body.len(),
                                                crate::CONTRACT_VERSION
                                            );
                                            if socket.write_all(headers.as_bytes()).await.is_ok() {
                                                let _ = socket.write_all(body.as_bytes()).await;
                                            }
                                            log.done(200, body.len());
                                            if want_close { break 'conn; } else { continue 'conn; }
                                        }
                                        Err(ExecErr::Timeout) => {
                                            let body = "504 Gateway Timeout: Goblin script exceeded time limit";
                                            let headers = format!(
                                                "HTTP/1.1 504 Gateway Timeout\r\n\
                                                 Content-Type: text/plain; charset=utf-8\r\n\
                                                 Content-Length: {}\r\n\
                                                 Connection: {connection_header}\r\n\
                                                 x-goblin-web-contract: {}\r\n\
                                                 \r\n",
                                                body.len(),
                                                crate::CONTRACT_VERSION
                                            );
                                            let _ = socket.write_all(headers.as_bytes()).await;
                                            let _ = socket.write_all(body.as_bytes()).await;
                                            log.done(504, body.len());
                                            if want_close { break 'conn; } else { continue 'conn; }
                                        }
                                        Err(ExecErr::NonZero(err)) => {
                                            let body = format!("Goblin runtime error:\n{err}");
                                            let headers = format!(
                                                "HTTP/1.1 500 Internal Server Error\r\n\
                                                 Content-Type: text/plain; charset=utf-8\r\n\
                                                 Content-Length: {}\r\n\
                                                 Connection: {connection_header}\r\n\
                                                 x-goblin-web-contract: {}\r\n\
                                                 \r\n",
                                                body.len(),
                                                crate::CONTRACT_VERSION
                                            );
                                            let _ = socket.write_all(headers.as_bytes()).await;
                                            let _ = socket.write_all(body.as_bytes()).await;
                                            log.done(500, body.len());
                                            if want_close { break 'conn; } else { continue 'conn; }
                                        }
                                        Err(ExecErr::Spawn(err)) => {
                                            let body = format!("Goblin spawn error:\n{err}");
                                            let headers = format!(
                                                "HTTP/1.1 500 Internal Server Error\r\n\
                                                 Content-Type: text/plain; charset=utf-8\r\n\
                                                 Content-Length: {}\r\n\
                                                 Connection: {connection_header}\r\n\
                                                 x-goblin-web-contract: {}\r\n\
                                                 \r\n",
                                                body.len(),
                                                crate::CONTRACT_VERSION
                                            );
                                            let _ = socket.write_all(headers.as_bytes()).await;
                                            let _ = socket.write_all(body.as_bytes()).await;
                                            log.done(500, body.len());
                                            if want_close { break 'conn; } else { continue 'conn; }
                                        }
                                    }
                                } else {
                                    let body = "API script not found";
                                    let headers = format!(
                                        "HTTP/1.1 404 Not Found\r\n\
                                         Content-Type: text/plain; charset=utf-8\r\n\
                                         Content-Length: {}\r\n\
                                         Connection: {connection_header}\r\n\
                                         x-goblin-web-contract: {}\r\n\
                                         \r\n",
                                        body.len(),
                                        crate::CONTRACT_VERSION
                                    );
                                    let _ = socket.write_all(headers.as_bytes()).await;
                                    let _ = socket.write_all(body.as_bytes()).await;
                                    log.done(404, body.len());
                                    if want_close { break 'conn; } else { continue 'conn; }
                                }
                            }

                            // ---- PROXY (GET-only; dev convenience) ----
                            // Runs before static serving so /api etc. are forwarded to your backend.
                            if method.eq_ignore_ascii_case("GET") && !proxies.is_empty() {
                                if let Some(rule) = proxies.iter().find(|r| path.starts_with(&r.prefix)) {
                                    // Build upstream URL
                                    let tail = if rule.strip_prefix {
                                        path.strip_prefix(&rule.prefix).unwrap_or(path)
                                    } else {
                                        path
                                    };
                                    // Ensure the tail begins with '/'
                                    let tail = if tail.is_empty() || !tail.starts_with('/') {
                                        format!("/{}", tail)
                                    } else {
                                        tail.to_string()
                                    };
                                    let upstream = format!("{}{}", rule.target.trim_end_matches('/'), tail);

                                    // Forward minimal headers and GET to upstream
                                    let client = reqwest::Client::new();
                                    let mut req = client.get(&upstream);

                                    // pass through a couple of common headers
                                    if let Some(ua) = headers_map.get("user-agent") { req = req.header("user-agent", ua); }
                                    if let Some(acc) = headers_map.get("accept") { req = req.header("accept", acc); }

                                    match req.send().await {
                                        Ok(resp) => {
                                            // Read status and headers BEFORE consuming the body.
                                            let status = resp.status().as_u16();
                                            let headers_clone = resp.headers().clone();
                                            let ctype = headers_clone
                                                .get(reqwest::header::CONTENT_TYPE)
                                                .and_then(|v| v.to_str().ok())
                                                .unwrap_or("application/octet-stream");

                                            // Now it's safe to consume the response.
                                            let body_bytes = match resp.bytes().await {
                                                Ok(b) => b,
                                                Err(e) => {
                                                    let msg = format!("upstream read error: {e}");
                                                    let body = msg.as_bytes();
                                                    let headers = format!(
                                                        "HTTP/1.1 502 Bad Gateway\r\n\
                                                         Content-Type: text/plain; charset=utf-8\r\n\
                                                         Content-Length: {}\r\n\
                                                         Connection: {connection_header}\r\n\
                                                         x-goblin-web-contract: {}\r\n\
                                                         \r\n",
                                                        body.len(),
                                                        crate::CONTRACT_VERSION
                                                    );
                                                    let _ = socket.write_all(headers.as_bytes()).await;
                                                    let _ = socket.write_all(body).await;
                                                    log.done(502, body.len());
                                                    if want_close { break 'conn; } else { continue 'conn; }
                                                }
                                            };

                                            // NOTE: Using "OK" as reason phrase is fine for browsers.
                                            let headers = format!(
                                                "HTTP/1.1 {status} OK\r\n\
                                                 Content-Type: {ctype}\r\n\
                                                 Content-Length: {}\r\n\
                                                 Connection: {connection_header}\r\n\
                                                 x-goblin-web-contract: {}\r\n\
                                                 \r\n",
                                                body_bytes.len(),
                                                crate::CONTRACT_VERSION
                                            );
                                            if socket.write_all(headers.as_bytes()).await.is_ok() {
                                                let _ = socket.write_all(&body_bytes).await;
                                            }
                                            log.done(status, body_bytes.len());
                                            if want_close { break 'conn; } else { continue 'conn; }
                                        }
                                        Err(e) => {
                                            let msg = format!("upstream connect error: {e}");
                                            let body = msg.as_bytes();
                                            let headers = format!(
                                                "HTTP/1.1 502 Bad Gateway\r\n\
                                                 Content-Type: text/plain; charset=utf-8\r\n\
                                                 Content-Length: {}\r\n\
                                                 Connection: {connection_header}\r\n\
                                                 x-goblin-web-contract: {}\r\n\
                                                 \r\n",
                                                body.len(),
                                                crate::CONTRACT_VERSION
                                            );
                                            let _ = socket.write_all(headers.as_bytes()).await;
                                            let _ = socket.write_all(body).await;
                                            log.done(502, body.len());
                                            if want_close { break 'conn; } else { continue 'conn; }
                                        }
                                    }
                                }
                            }

                            // --- try to serve from ./public if it exists ---
                            let public = PathBuf::from("./public");
                            if public.is_dir() {
                                // map "/" to index.html
                                let mut candidate = if path == "/" {
                                    public.join("index.html")
                                } else {
                                    match safe_join(&public, path) {
                                        Some(p) => p,
                                        None => {
                                            let resp = b"HTTP/1.1 403 Forbidden\r\n\
                                                         Connection: close\r\n\
                                                         Content-Length: 0\r\n\r\n";
                                            let _ = socket.write_all(resp).await;
                                            log.done(403, 0);
                                            break 'conn;
                                        }
                                    }
                                };

                                // if directory, append index.html
                                if let Ok(meta) = fs::metadata(&candidate).await {
                                    if meta.is_dir() {
                                        candidate.push("index.html");
                                    }
                                }

                                if let Ok(bytes) = fs::read(&candidate).await {
                                    // gather meta for caching headers
                                    let meta = fs::metadata(&candidate).await.ok();
                                    let len_u64 = meta.as_ref().map(|m| m.len()).unwrap_or(bytes.len() as u64);
                                    let mtime = meta.as_ref().and_then(|m| m.modified().ok());
                                    let etag = weak_etag(len_u64, mtime);

                                    // If-None-Match -> 304 (normalize and handle comma-separated lists)
                                    if let Some(raw) = if_none_match {
                                        let ours_norm = normalize_etag_token(&etag);
                                        let client_has_match = raw
                                            .split(',')
                                            .map(|t| normalize_etag_token(t))
                                            .any(|t| t == ours_norm);

                                        if client_has_match {
                                            let headers = format!(
                                                "HTTP/1.1 304 Not Modified\r\n\
                                                 ETag: {etag}\r\n\
                                                 Connection: {connection_header}\r\n\
                                                 x-goblin-web-contract: {}\r\n\
                                                 \r\n",
                                                crate::CONTRACT_VERSION
                                            );
                                            let _ = socket.write_all(headers.as_bytes()).await;
                                            log.done(304, 0);
                                            if want_close { break 'conn; } else { continue 'conn; }
                                        }
                                    }

                                    let ext = candidate
                                        .extension()
                                        .and_then(|e| e.to_str())
                                        .unwrap_or("")
                                        .to_ascii_lowercase();
                                    let mime = guess_mime(&ext);
                                    let last_mod = mtime.map(http_date);
                                    let last_mod_hdr = last_mod
                                        .as_ref()
                                        .map(|s| format!("Last-Modified: {s}\r\n"))
                                        .unwrap_or_default();

                                    let headers = format!(
                                        "HTTP/1.1 200 OK\r\n\
                                         Content-Type: {mime}\r\n\
                                         Content-Length: {}\r\n\
                                         ETag: {etag}\r\n\
                                         {last_mod_hdr}\
                                         Connection: {connection_header}\r\n\
                                         x-goblin-web-contract: {}\r\n\
                                         \r\n",
                                        bytes.len(),
                                        crate::CONTRACT_VERSION
                                    );
                                    if socket.write_all(headers.as_bytes()).await.is_ok() {
                                        let _ = socket.write_all(&bytes).await;
                                    }
                                    log.done(200, bytes.len());
                                    if want_close { break 'conn; } else { continue 'conn; }
                                } else {
                                    // --- SPA fallback: if path looks like a client route (no dot), serve /index.html
                                    let looks_like_route = !path.split('/').last().unwrap_or("").contains('.');
                                    let index_path = public.join("index.html");

                                    if looks_like_route {
                                        if let Ok(bytes) = fs::read(&index_path).await {
                                            let meta = fs::metadata(&index_path).await.ok();
                                            let len_u64 = meta.as_ref().map(|m| m.len()).unwrap_or(bytes.len() as u64);
                                            let mtime = meta.as_ref().and_then(|m| m.modified().ok());
                                            let etag = weak_etag(len_u64, mtime);

                                            if let Some(raw) = if_none_match {
                                                let ours_norm = normalize_etag_token(&etag);
                                                let client_has_match = raw
                                                    .split(',')
                                                    .map(|t| normalize_etag_token(t))
                                                    .any(|t| t == ours_norm);
                                                if client_has_match {
                                                    let headers = format!(
                                                        "HTTP/1.1 304 Not Modified\r\n\
                                                         ETag: {etag}\r\n\
                                                         Connection: {connection_header}\r\n\
                                                         x-goblin-web-contract: {}\r\n\
                                                         \r\n",
                                                        crate::CONTRACT_VERSION
                                                    );
                                                    let _ = socket.write_all(headers.as_bytes()).await;
                                                    log.done(304, 0);
                                                    if want_close { break 'conn; } else { continue 'conn; }
                                                }
                                            }

                                            let headers = format!(
                                                "HTTP/1.1 200 OK\r\n\
                                                 Content-Type: text/html; charset=utf-8\r\n\
                                                 Content-Length: {}\r\n\
                                                 ETag: {etag}\r\n\
                                                 Connection: {connection_header}\r\n\
                                                 x-goblin-web-contract: {}\r\n\
                                                 \r\n",
                                                bytes.len(),
                                                crate::CONTRACT_VERSION
                                            );
                                            if socket.write_all(headers.as_bytes()).await.is_ok() {
                                                let _ = socket.write_all(&bytes).await;
                                            }
                                            log.done(200, bytes.len());
                                            if want_close { break 'conn; } else { continue 'conn; }
                                        }
                                    }

                                    // Real 404 (no SPA fallback or missing index.html)
                                    let resp = b"HTTP/1.1 404 Not Found\r\n\
                                                 Content-Type: text/plain; charset=utf-8\r\n\
                                                 Connection: close\r\n\
                                                 Content-Length: 13\r\n\r\n404 not found";
                                    let _ = socket.write_all(resp).await;
                                    log.done(404, 13);
                                    break 'conn;
                                }
                            }

                            // --- fallback: simple text (no public dir present) ---
                            let body = format!("GoblinHost is alive.\nPath: {path}\n");
                            let headers = format!(
                                "HTTP/1.1 200 OK\r\n\
                                 Content-Type: text/plain; charset=utf-8\r\n\
                                 Content-Length: {}\r\n\
                                 Connection: {connection_header}\r\n\
                                 x-goblin-web-contract: {}\r\n\
                                 \r\n",
                                body.len(),
                                crate::CONTRACT_VERSION
                            );
                            if socket.write_all(headers.as_bytes()).await.is_ok() {
                                let _ = socket.write_all(body.as_bytes()).await;
                            }
                            log.done(200, body.len());
                            if want_close { break 'conn; } else { continue 'conn; }
                        } // end 'conn loop
                    });
                }

                // Ctrl+C
                _ = tokio::signal::ctrl_c() => {
                    println!("Ctrl+C received — shutting down GoblinHost");
                    break;
                }
            }
        }

        Ok(())
    }
}

// ---------- Errors ----------

#[derive(thiserror::Error, Debug)]
pub enum HostError {
    #[error("bind failed: {0}")]
    Bind(String),
    #[error("io error: {0}")]
    Io(String),
    #[error("internal: {0}")]
    Internal(String),
}

// Re-export version string for info endpoints.
pub const CONTRACT_VERSION: &str = "v0";

// REPLACE the old exec_goblin_script_via_cli(...) with this async version:

#[derive(Debug)]
enum ExecErr {
    Spawn(String),
    Timeout,
    NonZero(String),
}

async fn exec_goblin_script_via_cli_timeout(
    script_path: &std::path::Path,
    timeout_ms: u64,
) -> Result<String, ExecErr> {
    use tokio::process::Command;
    use tokio::time::{timeout, Duration};
    use std::process::Stdio;

    let mut cmd = Command::new("goblin");
    cmd.kill_on_drop(true); // child will be terminated if dropped
    cmd.arg(script_path.as_os_str())
       .env("GOBLIN_NONINTERACTIVE", "1")
       .stdin(Stdio::null())
       .stdout(Stdio::piped())
       .stderr(Stdio::piped());

    let child = cmd.spawn()
        .map_err(|e| ExecErr::Spawn(format!("failed to spawn goblin CLI: {e}")))?;

    match timeout(Duration::from_millis(timeout_ms), child.wait_with_output()).await {
        Err(_) => {
            // Timed out: the future (and thus the Child) will be dropped here.
            // Because kill_on_drop(true) is set, the subprocess is terminated.
            Err(ExecErr::Timeout)
        }
        Ok(Ok(out)) => {
            if !out.status.success() {
                Err(ExecErr::NonZero(String::from_utf8_lossy(&out.stderr).into_owned()))
            } else {
                Ok(String::from_utf8_lossy(&out.stdout).into_owned())
            }
        }
        Ok(Err(e)) => Err(ExecErr::Spawn(format!("wait_with_output failed: {e}"))),
    }
}