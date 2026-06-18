//! goblin-host: Embedded HTTP host engine for Goblin (skeleton v0)

use std::path::PathBuf;
use std::fmt;
use std::path::Path;
use std::time::{SystemTime, UNIX_EPOCH};
use std::time::{Instant};

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

    fn done(&self, status: u16, bytes: usize) {
        let ms = self.start.elapsed().as_millis();
        println!("{} {} → {} {}B in {}ms", self.method, self.path, status, bytes, ms);
    }
}

fn normalize_etag_token(s: &str) -> String {
    let s = s.trim();
    let (weak_prefix, rest) = if let Some(rest) = s.strip_prefix("W/") {
        ("W/", rest.trim_start())
    } else if let Some(rest) = s.strip_prefix("W/ ") {
        ("W/", rest.trim_start())
    } else {
        ("", s)
    };
    let rest = rest.strip_prefix('"').and_then(|r| r.strip_suffix('"')).unwrap_or(rest);
    if weak_prefix.is_empty() {
        rest.to_string()
    } else {
        format!("W/{}", rest)
    }
}

fn http_date(ts: SystemTime) -> String {
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

#[allow(dead_code)]
fn parse_if_none_match(raw: &str) -> Vec<String> {
    raw.split(',')
        .map(|s| s.trim())
        .filter(|s| !s.is_empty())
        .map(|tag| {
            let mut t = tag.to_string();
            if t.starts_with("W/ ") {
                t = format!("W/{}", t.trim_start_matches("W/ ").trim_start());
            }
            if let Some(pos) = t.find('"') {
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

#[derive(Debug)]
pub struct Request;
#[derive(Debug)]
pub struct Response;
#[derive(Debug, Clone)]
pub struct Ctx;

#[derive(Clone, Debug)]
pub struct StaticCfg {
    pub dir: PathBuf,
    pub mount: String,
    pub spa_fallback: bool,
}

#[derive(Clone, Debug)]
pub struct ProxyCfg {
    pub prefix: String,
    pub target: String,
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
    pub strip_prefix: bool,
}

#[derive(Debug)]
pub struct HostConfig {
    pub host: String,
    pub port: u16,
    pub limits: Limits,
    pub caps: Caps,
    pub steps: Vec<Step>,
    pub proxies: Vec<ProxyRule>,
    pub use_vm: bool,
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
            use_vm: false,
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
    pub fn vm(mut self, enabled: bool) -> Self { self.cfg.use_vm = enabled; self }
    pub fn build(self) -> Host { Host { cfg: self.cfg } }
}

pub struct Host {
    cfg: HostConfig,
}

fn find_double_crlf(buf: &[u8]) -> Option<usize> {
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

fn safe_join(root: &Path, req_path: &str) -> Option<PathBuf> {
    if req_path.contains('\0') { return None; }
    let p = req_path.split(['?', '#']).next().unwrap_or("");
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

fn host_without_port(host: &str) -> &str {
    let host = host.trim();
    if let Some(stripped) = host.strip_prefix('[').and_then(|h| h.strip_suffix(']')) {
        return stripped;
    }
    if let Some((left, right)) = host.rsplit_once(':') {
        if !left.is_empty() && right.chars().all(|c| c.is_ascii_digit()) {
            return left;
        }
    }
    host
}

fn site_name_from_host(host: &str) -> Option<&str> {
    let host = host_without_port(host);
    let (site, rest) = host.split_once('.')?;
    if site.is_empty() || site.eq_ignore_ascii_case("www") {
        return None;
    }
    if rest.eq_ignore_ascii_case("localhost") {
        return Some(site);
    }
    if !rest.contains('.') {
        return None;
    }
    Some(site)
}

fn resolve_request_root(base_docroot: &Path, host_header: Option<&str>) -> PathBuf {
    let Some(host_header) = host_header else {
        return base_docroot.to_path_buf();
    };
    let Some(site_name) = site_name_from_host(host_header) else {
        return base_docroot.to_path_buf();
    };
    let candidate = base_docroot.join(site_name);
    if candidate.is_dir() {
        candidate
    } else {
        base_docroot.to_path_buf()
    }
}

fn urldecode(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%' && i + 2 < bytes.len() {
            if let Ok(hex) = std::str::from_utf8(&bytes[i+1..i+3]) {
                if let Ok(b) = u8::from_str_radix(hex, 16) {
                    out.push(b as char);
                    i += 3;
                    continue;
                }
            }
        } else if bytes[i] == b'+' {
            out.push(' ');
            i += 1;
            continue;
        }
        out.push(bytes[i] as char);
        i += 1;
    }
    out
}

impl Host {
    pub async fn run(&mut self) -> Result<(), HostError> {
        use tokio::net::TcpListener;

        let port = std::env::var("PORT")
            .ok()
            .and_then(|s| s.parse::<u16>().ok())
            .unwrap_or(self.cfg.port);

        let addr = format!("{}:{}", self.cfg.host, port);
        let listener = TcpListener::bind(&addr)
            .await
            .map_err(|e| HostError::Bind(e.to_string()))?;

        let docroot = std::env::var("GOBLIN_DOCROOT")
            .map(PathBuf::from)
            .unwrap_or_else(|_| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));

        println!("GoblinHost listening on {}", addr);
        println!("[serve] docroot = {}", docroot.display());

        loop {
            tokio::select! {
                accept_res = listener.accept() => {
                    let (mut socket, peer) = match accept_res {
                        Ok(pair) => pair,
                        Err(e) => {
                            eprintln!("accept error: {e}");
                            continue;
                        }
                    };

                    println!("Accepted connection from {}", peer);
                    let idle_ms = 60_000;
                    let proxies = self.cfg.proxies.clone();
                    let docroot = docroot.clone();
                    let use_vm = self.cfg.use_vm;

                    tokio::spawn(async move {
                        use tokio::io::{AsyncReadExt, AsyncWriteExt};
                        use tokio::fs;
                        use tokio::time::{timeout, Duration};
                        use std::collections::HashMap;

                        let mut log = AccessLog::new("?", "?");

                        'conn: loop {
                            let mut buf = Vec::with_capacity(4096);
                            let mut tmp = [0u8; 512];

                            let request = loop {
                                let read_res = timeout(Duration::from_millis(idle_ms), socket.read(&mut tmp)).await;
                                let n = match read_res {
                                    Err(_) => { break None; }
                                    Ok(Ok(0)) => break None,
                                    Ok(Ok(n)) => n,
                                    Ok(Err(e)) => {
                                        eprintln!("read error from {}: {}", peer, e);
                                        break None;
                                    }
                                };
                                buf.extend_from_slice(&tmp[..n]);
                                if let Some(pos) = find_double_crlf(&buf) {
                                    let head_end = pos.saturating_sub(4);
                                    let head = buf[..head_end].to_vec();
                                    let rest = buf[pos..].to_vec();
                                    break Some((head, rest));
                                }
                                if buf.len() > 16 * 1024 {
                                    let resp = b"HTTP/1.1 431 Request Header Fields Too Large\r\nConnection: close\r\nContent-Length: 0\r\n\r\n";
                                    let _ = socket.write_all(resp).await;
                                    log.done(431, 0);
                                    break 'conn;
                                }
                            };

                            let Some((head, mut body_buf)) = request else { break 'conn; };

                            let head_str = match std::str::from_utf8(&head) {
                                Ok(s) => s,
                                Err(_) => {
                                    let resp = b"HTTP/1.1 400 Bad Request\r\nConnection: close\r\nContent-Length: 0\r\n\r\n";
                                    let _ = socket.write_all(resp).await;
                                    log.done(400, 0);
                                    break 'conn;
                                }
                            };

                            let mut lines = head_str.split("\r\n");
                            let Some(req_line) = lines.next() else {
                                let resp = b"HTTP/1.1 400 Bad Request\r\nConnection: close\r\nContent-Length: 0\r\n\r\n";
                                let _ = socket.write_all(resp).await;
                                log.done(400, 0);
                                break 'conn;
                            };

                            let mut parts = req_line.split_whitespace();
                            let method = parts.next().unwrap_or("");
                            let full_path = parts.next().unwrap_or("/");
                            let (path, query_string) = if let Some(idx) = full_path.find('?') {
                                (&full_path[..idx], &full_path[idx + 1..])
                            } else {
                                (full_path, "")
                            };
                            let ver = parts.next().unwrap_or("HTTP/1.1");

                            log = AccessLog::new(method, path);

                            let mut headers_map: HashMap<String, String> = HashMap::new();
                            for line in lines {
                                if line.is_empty() { break; }
                                if let Some((name, val)) = line.split_once(':') {
                                    headers_map.insert(name.trim().to_ascii_lowercase(), val.trim().to_string());
                                }
                            }

                            let content_length = headers_map
                                .get("content-length")
                                .and_then(|s| s.parse::<usize>().ok())
                                .unwrap_or(0);

                            while body_buf.len() < content_length {
                                let read_res = timeout(Duration::from_millis(idle_ms), socket.read(&mut tmp)).await;
                                let n = match read_res {
                                    Err(_) => { break 'conn; }
                                    Ok(Ok(0)) => { break 'conn; }
                                    Ok(Ok(n)) => n,
                                    Ok(Err(e)) => {
                                        eprintln!("read body error from {}: {}", peer, e);
                                        break 'conn;
                                    }
                                };
                                body_buf.extend_from_slice(&tmp[..n]);
                            }

                            if body_buf.len() > content_length {
                                body_buf.truncate(content_length);
                            }

                            let body_text = String::from_utf8_lossy(&body_buf).into_owned();

                            let host_header = headers_map.get("host").map(String::as_str);
                            let request_root = resolve_request_root(&docroot, host_header);

                            let conn_req = headers_map.get("connection").map(|s| s.to_ascii_lowercase());
                            let mut want_close = false;
                            let connection_header = if ver.eq_ignore_ascii_case("HTTP/1.0") {
                                if matches!(conn_req.as_deref(), Some("keep-alive")) {
                                    "keep-alive"
                                } else {
                                    want_close = true;
                                    "close"
                                }
                            } else {
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
                                    "HTTP/1.1 200 OK\r\nContent-Type: application/json; charset=utf-8\r\nContent-Length: {}\r\nConnection: {connection_header}\r\nx-goblin-web-contract: {}\r\n\r\n",
                                    body.len(), crate::CONTRACT_VERSION
                                );
                                if socket.write_all(headers.as_bytes()).await.is_ok() {
                                    let _ = socket.write_all(body.as_bytes()).await;
                                }
                                log.done(200, body.len());
                                if want_close { break 'conn; } else { continue 'conn; }
                            }

                            if path == "/_info" {
                                let host_json = host_header
                                    .map(|h| format!("\"{}\"", h.replace('\\', "\\\\").replace('"', "\\\"")))
                                    .unwrap_or_else(|| "null".to_string());
                                let body = format!(
                                    r#"{{"ok":true,"docroot":"{}","request_root":"{}","host":{},"contract":"{}"}}"#,
                                    docroot.display(), request_root.display(), host_json, crate::CONTRACT_VERSION
                                );
                                let headers = format!(
                                    "HTTP/1.1 200 OK\r\nContent-Type: application/json; charset=utf-8\r\nContent-Length: {}\r\nConnection: {connection_header}\r\nx-goblin-web-contract: {}\r\n\r\n",
                                    body.len(), crate::CONTRACT_VERSION
                                );
                                if socket.write_all(headers.as_bytes()).await.is_ok() {
                                    let _ = socket.write_all(body.as_bytes()).await;
                                }
                                log.done(200, body.len());
                                if want_close { break 'conn; } else { continue 'conn; }
                            }

                            // === Dynamic Goblin API Execution (via CLI) ===
                            if path.starts_with("/api/") {
                                use tokio::io::AsyncWriteExt;

                                let api_rel = path.trim_start_matches("/api/");
                                let script_path = request_root.join("api").join(format!("{api_rel}.gbln"));

                                if script_path.exists() {
                                    let authorization = headers_map.iter()
                                        .find(|(k, _)| k.eq_ignore_ascii_case("authorization"))
                                        .map(|(_, v)| v.clone())
                                        .unwrap_or_default();

                                    let host = host_header.unwrap_or("").to_string();

                                    let headers_json = serde_json::to_string(&headers_map)
                                        .unwrap_or_else(|_| "{}".to_string());

                                    // Trusted auth context comes from trusted upstream request headers.
                                    // Keep this generic — no vendor-specific auth here.
                                    let auth_user_id = headers_map.iter()
                                        .find(|(k, _)| k.eq_ignore_ascii_case("x-goblin-auth-user-id"))
                                        .map(|(_, v)| v.clone())
                                        .unwrap_or_default();

                                    let auth_email = headers_map.iter()
                                        .find(|(k, _)| k.eq_ignore_ascii_case("x-goblin-auth-email"))
                                        .map(|(_, v)| v.clone())
                                        .unwrap_or_default();

                                    let auth_role = headers_map.iter()
                                        .find(|(k, _)| k.eq_ignore_ascii_case("x-goblin-auth-role"))
                                        .map(|(_, v)| v.clone())
                                        .unwrap_or_default();

                                    let auth_json = headers_map.iter()
                                        .find(|(k, _)| k.eq_ignore_ascii_case("x-goblin-auth-json"))
                                        .map(|(_, v)| v.clone())
                                        .unwrap_or_default();

                                    let request_file_path = if !body_buf.is_empty() {
                                        let tmp = std::env::temp_dir().join(format!(
                                            "goblin-request-{}",
                                            SystemTime::now()
                                                .duration_since(UNIX_EPOCH)
                                                .unwrap()
                                                .as_nanos()
                                        ));

                                        if let Err(e) = std::fs::write(&tmp, &body_buf) {
                                            let body = format!("request body temp write failed: {e}");
                                            let headers = format!(
                                                "HTTP/1.1 500 Internal Server Error\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: {}\r\nConnection: {connection_header}\r\nx-goblin-web-contract: {}\r\n\r\n",
                                                body.len(), crate::CONTRACT_VERSION
                                            );
                                            let _ = socket.write_all(headers.as_bytes()).await;
                                            let _ = socket.write_all(body.as_bytes()).await;
                                            log.done(500, body.len());
                                            if want_close { break 'conn; } else { continue 'conn; }
                                        }

                                        tmp.to_string_lossy().to_string()
                                    } else {
                                        String::new()
                                    };

                                    let goblin_body = if request_file_path.is_empty() {
                                        body_text.as_str()
                                    } else {
                                        ""
                                    };

                                    let exec_result = if use_vm {
                                        exec_goblin_script_via_vm(
                                            &script_path,
                                            30000,
                                            query_string,
                                            method,
                                            path,
                                            &host,
                                            goblin_body,
                                            &request_file_path,
                                            &authorization,
                                            &headers_json,
                                            &auth_user_id,
                                            &auth_email,
                                            &auth_role,
                                            &auth_json,
                                        ).await
                                    } else {
                                        exec_goblin_script_via_cli_timeout(
                                            &script_path,
                                            30000,
                                            query_string,
                                            method,
                                            path,
                                            &host,
                                            goblin_body,
                                            &request_file_path,
                                            &authorization,
                                            &headers_json,
                                            &auth_user_id,
                                            &auth_email,
                                            &auth_role,
                                            &auth_json,
                                        ).await
                                    };
                                    match exec_result {
                                        Ok(body) => {
                                            let parsed: Result<serde_json::Value, _> = serde_json::from_str(&body);

                                            if let Ok(v) = parsed {
                                                let is_envelope =
                                                    v.get("status").is_some()
                                                    || v.get("body").is_some()
                                                    || v.get("headers").is_some()
                                                    || v.get("cookies").is_some();

                                                if !is_envelope {
                                                    let headers = format!(
                                                        "HTTP/1.1 200 OK\r\nContent-Type: application/json; charset=utf-8\r\nContent-Length: {}\r\nConnection: {connection_header}\r\nx-goblin-web-contract: {}\r\n\r\n",
                                                        body.len(), crate::CONTRACT_VERSION
                                                    );
                                                    if socket.write_all(headers.as_bytes()).await.is_ok() {
                                                        let _ = socket.write_all(body.as_bytes()).await;
                                                    }
                                                    log.done(200, body.len());
                                                    if want_close { break 'conn; } else { continue 'conn; }
                                                }

                                                let status = v.get("status")
                                                    .and_then(|x| x.as_u64())
                                                    .unwrap_or(200) as u16;

                                                let response_body = v.get("body")
                                                    .and_then(|x| x.as_str())
                                                    .unwrap_or("")
                                                    .to_string();

                                                let mut extra_headers = String::new();

                                                if let Some(headers_obj) = v.get("headers").and_then(|x| x.as_object()) {
                                                    for (k, val) in headers_obj {
                                                        if let Some(s) = val.as_str() {
                                                            extra_headers.push_str(&format!("{k}: {s}\r\n"));
                                                        }
                                                    }
                                                }

                                                if let Some(cookies_arr) = v.get("cookies").and_then(|x| x.as_array()) {
                                                    for c in cookies_arr {
                                                        if let Some(s) = c.as_str() {
                                                            extra_headers.push_str(&format!("Set-Cookie: {s}\r\n"));
                                                        }
                                                    }
                                                }

                                                let reason = match status {
                                                    200 => "OK",
                                                    201 => "Created",
                                                    204 => "No Content",
                                                    301 => "Moved Permanently",
                                                    302 => "Found",
                                                    303 => "See Other",
                                                    304 => "Not Modified",
                                                    307 => "Temporary Redirect",
                                                    308 => "Permanent Redirect",
                                                    400 => "Bad Request",
                                                    401 => "Unauthorized",
                                                    403 => "Forbidden",
                                                    404 => "Not Found",
                                                    405 => "Method Not Allowed",
                                                    409 => "Conflict",
                                                    422 => "Unprocessable Entity",
                                                    429 => "Too Many Requests",
                                                    500 => "Internal Server Error",
                                                    502 => "Bad Gateway",
                                                    503 => "Service Unavailable",
                                                    504 => "Gateway Timeout",
                                                    _ => "OK",
                                                };

                                                // For redirects, send no body and no Content-Type.
                                                let is_redirect = status >= 300 && status < 400;

                                                let headers = if is_redirect {
                                                    format!(
                                                        "HTTP/1.1 {status} {reason}\r\nContent-Length: 0\r\nConnection: {connection_header}\r\nx-goblin-web-contract: {}\r\n{extra_headers}\r\n",
                                                        crate::CONTRACT_VERSION
                                                    )
                                                } else {
                                                    let content_type = v.get("headers")
                                                        .and_then(|h| h.get("Content-Type"))
                                                        .and_then(|x| x.as_str())
                                                        .unwrap_or("text/plain; charset=utf-8");
                                                    format!(
                                                        "HTTP/1.1 {status} {reason}\r\nContent-Type: {content_type}\r\nContent-Length: {}\r\nConnection: {connection_header}\r\nx-goblin-web-contract: {}\r\n{extra_headers}\r\n",
                                                        response_body.len(),
                                                        crate::CONTRACT_VERSION
                                                    )
                                                };

                                                if socket.write_all(headers.as_bytes()).await.is_ok() {
                                                    if !is_redirect {
                                                        let _ = socket.write_all(response_body.as_bytes()).await;
                                                    }
                                                }
                                                log.done(status, if is_redirect { 0 } else { response_body.len() });
                                                if want_close { break 'conn; } else { continue 'conn; }
                                            }

                                            let headers = format!(
                                                "HTTP/1.1 200 OK\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: {}\r\nConnection: {connection_header}\r\nx-goblin-web-contract: {}\r\n\r\n",
                                                body.len(), crate::CONTRACT_VERSION
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
                                                "HTTP/1.1 504 Gateway Timeout\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: {}\r\nConnection: {connection_header}\r\nx-goblin-web-contract: {}\r\n\r\n",
                                                body.len(), crate::CONTRACT_VERSION
                                            );
                                            let _ = socket.write_all(headers.as_bytes()).await;
                                            let _ = socket.write_all(body.as_bytes()).await;
                                            log.done(504, body.len());
                                            if want_close { break 'conn; } else { continue 'conn; }
                                        }
                                        Err(ExecErr::NonZero(err)) => {
                                            let body = format!("Goblin runtime error:\n{err}");
                                            let headers = format!(
                                                "HTTP/1.1 500 Internal Server Error\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: {}\r\nConnection: {connection_header}\r\nx-goblin-web-contract: {}\r\n\r\n",
                                                body.len(), crate::CONTRACT_VERSION
                                            );
                                            let _ = socket.write_all(headers.as_bytes()).await;
                                            let _ = socket.write_all(body.as_bytes()).await;
                                            log.done(500, body.len());
                                            if want_close { break 'conn; } else { continue 'conn; }
                                        }
                                        Err(ExecErr::Spawn(err)) => {
                                            let body = format!("Goblin spawn error:\n{err}");
                                            let headers = format!(
                                                "HTTP/1.1 500 Internal Server Error\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: {}\r\nConnection: {connection_header}\r\nx-goblin-web-contract: {}\r\n\r\n",
                                                body.len(), crate::CONTRACT_VERSION
                                            );
                                            let _ = socket.write_all(headers.as_bytes()).await;
                                            let _ = socket.write_all(body.as_bytes()).await;
                                            log.done(500, body.len());
                                            if want_close { break 'conn; } else { continue 'conn; }
                                        }
                                    }
                                } else {
                                    let body = format!("API script not found: {}", script_path.display());
                                    let headers = format!(
                                        "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: {}\r\nConnection: {connection_header}\r\nx-goblin-web-contract: {}\r\n\r\n",
                                        body.len(), crate::CONTRACT_VERSION
                                    );
                                    let _ = socket.write_all(headers.as_bytes()).await;
                                    let _ = socket.write_all(body.as_bytes()).await;
                                    log.done(404, body.len());
                                    if want_close { break 'conn; } else { continue 'conn; }
                                }
                            }

                            // ---- PROXY (GET-only; dev convenience) ----
                            if method.eq_ignore_ascii_case("GET") && !proxies.is_empty() {
                                if let Some(rule) = proxies.iter().find(|r| path.starts_with(&r.prefix)) {
                                    let tail = if rule.strip_prefix {
                                        path.strip_prefix(&rule.prefix).unwrap_or(path)
                                    } else {
                                        path
                                    };
                                    let tail = if tail.is_empty() || !tail.starts_with('/') {
                                        format!("/{}", tail)
                                    } else {
                                        tail.to_string()
                                    };
                                    let upstream = format!("{}{}", rule.target.trim_end_matches('/'), tail);

                                    let client = reqwest::Client::new();
                                    let mut req = client.get(&upstream);
                                    if let Some(ua) = headers_map.get("user-agent") { req = req.header("user-agent", ua); }
                                    if let Some(acc) = headers_map.get("accept") { req = req.header("accept", acc); }

                                    match req.send().await {
                                        Ok(resp) => {
                                            let status = resp.status().as_u16();
                                            let headers_clone = resp.headers().clone();
                                            let ctype = headers_clone
                                                .get(reqwest::header::CONTENT_TYPE)
                                                .and_then(|v| v.to_str().ok())
                                                .unwrap_or("application/octet-stream");
                                            let body_bytes = match resp.bytes().await {
                                                Ok(b) => b,
                                                Err(e) => {
                                                    let msg = format!("upstream read error: {e}");
                                                    let body = msg.as_bytes();
                                                    let headers = format!(
                                                        "HTTP/1.1 502 Bad Gateway\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: {}\r\nConnection: {connection_header}\r\nx-goblin-web-contract: {}\r\n\r\n",
                                                        body.len(), crate::CONTRACT_VERSION
                                                    );
                                                    let _ = socket.write_all(headers.as_bytes()).await;
                                                    let _ = socket.write_all(body).await;
                                                    log.done(502, body.len());
                                                    if want_close { break 'conn; } else { continue 'conn; }
                                                }
                                            };
                                            let headers = format!(
                                                "HTTP/1.1 {status} OK\r\nContent-Type: {ctype}\r\nContent-Length: {}\r\nConnection: {connection_header}\r\nx-goblin-web-contract: {}\r\n\r\n",
                                                body_bytes.len(), crate::CONTRACT_VERSION
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
                                                "HTTP/1.1 502 Bad Gateway\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: {}\r\nConnection: {connection_header}\r\nx-goblin-web-contract: {}\r\n\r\n",
                                                body.len(), crate::CONTRACT_VERSION
                                            );
                                            let _ = socket.write_all(headers.as_bytes()).await;
                                            let _ = socket.write_all(body).await;
                                            log.done(502, body.len());
                                            if want_close { break 'conn; } else { continue 'conn; }
                                        }
                                    }
                                }
                            }

                            // --- serve from resolved request root ---
                            let root = request_root.clone();

                            let mut candidate = if path == "/" {
                                root.join("index.html")
                            } else {
                                match safe_join(&root, path) {
                                    Some(p) => p,
                                    None => {
                                        let resp = b"HTTP/1.1 403 Forbidden\r\nConnection: close\r\nContent-Length: 0\r\n\r\n";
                                        let _ = socket.write_all(resp).await;
                                        log.done(403, 0);
                                        break 'conn;
                                    }
                                }
                            };

                            if let Ok(meta) = fs::metadata(&candidate).await {
                                if meta.is_dir() {
                                    candidate.push("index.html");
                                }
                            }

                            if let Ok(bytes) = fs::read(&candidate).await {
                                // Never serve .gbln files as raw static content — always execute via VM
                                let ext = candidate.extension().and_then(|e| e.to_str()).unwrap_or("").to_ascii_lowercase();
                                if use_vm && ext == "gbln" {
                                    let script_path = candidate.clone();
                                    let s_host = host_header.unwrap_or("").to_string();
                                    let s_auth = headers_map.iter().find(|(k, _)| k.eq_ignore_ascii_case("authorization")).map(|(_, v)| v.clone()).unwrap_or_default();
                                    let s_headers_json = serde_json::to_string(&headers_map).unwrap_or_else(|_| "{}".to_string());
                                    let s_auth_user_id = headers_map.iter().find(|(k, _)| k.eq_ignore_ascii_case("x-goblin-auth-user-id")).map(|(_, v)| v.clone()).unwrap_or_default();
                                    let s_auth_email = headers_map.iter().find(|(k, _)| k.eq_ignore_ascii_case("x-goblin-auth-email")).map(|(_, v)| v.clone()).unwrap_or_default();
                                    let s_auth_role = headers_map.iter().find(|(k, _)| k.eq_ignore_ascii_case("x-goblin-auth-role")).map(|(_, v)| v.clone()).unwrap_or_default();
                                    let s_auth_json = headers_map.iter().find(|(k, _)| k.eq_ignore_ascii_case("x-goblin-auth-json")).map(|(_, v)| v.clone()).unwrap_or_default();
                                    let result = exec_goblin_script_via_vm(
                                        &script_path,
                                        30000,
                                        query_string,
                                        method,
                                        path,
                                        &s_host,
                                        &body_text,
                                        "",
                                        &s_auth,
                                        &s_headers_json,
                                        &s_auth_user_id,
                                        &s_auth_email,
                                        &s_auth_role,
                                        &s_auth_json,
                                    ).await;
                                    match result {
                                        Ok(envelope_json) => {
                                            let parsed: serde_json::Value = serde_json::from_str(&envelope_json).unwrap_or(serde_json::Value::Null);
                                            let body_str = parsed.get("body").and_then(|b| b.as_str()).unwrap_or("").to_string();
                                            let status = parsed.get("status").and_then(|s| s.as_u64()).unwrap_or(200) as u16;
                                            let is_redirect = status >= 300 && status < 400;
                                            let reason = match status {
                                                200 => "OK", 201 => "Created", 204 => "No Content",
                                                301 => "Moved Permanently", 302 => "Found",
                                                303 => "See Other", 307 => "Temporary Redirect",
                                                308 => "Permanent Redirect",
                                                400 => "Bad Request", 401 => "Unauthorized",
                                                403 => "Forbidden", 404 => "Not Found",
                                                500 => "Internal Server Error", _ => "OK",
                                            };
                                            let mut extra_headers = String::new();
                                            if let Some(ho) = parsed.get("headers").and_then(|h| h.as_object()) {
                                                for (k, val) in ho {
                                                    if let Some(s) = val.as_str() {
                                                        extra_headers.push_str(&format!("{k}: {s}\r\n"));
                                                    }
                                                }
                                            }
                                            let resp_headers = if is_redirect {
                                                format!(
                                                    "HTTP/1.1 {status} {reason}\r\nContent-Length: 0\r\nConnection: {connection_header}\r\nx-goblin-web-contract: {}\r\n{extra_headers}\r\n",
                                                    crate::CONTRACT_VERSION
                                                )
                                            } else {
                                                let content_type = parsed.get("headers").and_then(|h| h.get("Content-Type")).and_then(|v| v.as_str()).unwrap_or("text/html; charset=utf-8");
                                                format!(
                                                    "HTTP/1.1 {status} {reason}\r\nContent-Type: {content_type}\r\nContent-Length: {}\r\nConnection: {connection_header}\r\nx-goblin-web-contract: {}\r\n{extra_headers}\r\n",
                                                    body_str.len(), crate::CONTRACT_VERSION
                                                )
                                            };
                                            if socket.write_all(resp_headers.as_bytes()).await.is_ok() {
                                                if !is_redirect {
                                                    let _ = socket.write_all(body_str.as_bytes()).await;
                                                }
                                            }
                                            log.done(status, if is_redirect { 0 } else { body_str.len() });
                                        }
                                        Err(ExecErr::Timeout) => {
                                            let body = "504 Gateway Timeout";
                                            let resp_headers = format!("HTTP/1.1 504 Gateway Timeout\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: {}\r\nConnection: {connection_header}\r\nx-goblin-web-contract: {}\r\n\r\n", body.len(), crate::CONTRACT_VERSION);
                                            let _ = socket.write_all(resp_headers.as_bytes()).await;
                                            let _ = socket.write_all(body.as_bytes()).await;
                                            log.done(504, body.len());
                                        }
                                        Err(ExecErr::NonZero(e) | ExecErr::Spawn(e)) => {
                                            let body = format!("Goblin script error:\n{e}");
                                            let resp_headers = format!("HTTP/1.1 500 Internal Server Error\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: {}\r\nConnection: {connection_header}\r\nx-goblin-web-contract: {}\r\n\r\n", body.len(), crate::CONTRACT_VERSION);
                                            let _ = socket.write_all(resp_headers.as_bytes()).await;
                                            let _ = socket.write_all(body.as_bytes()).await;
                                            log.done(500, body.len());
                                        }
                                    }
                                    if want_close { break 'conn; } else { continue 'conn; }
                                }

                                let meta = fs::metadata(&candidate).await.ok();
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
                                            "HTTP/1.1 304 Not Modified\r\nETag: {etag}\r\nConnection: {connection_header}\r\nx-goblin-web-contract: {}\r\n\r\n",
                                            crate::CONTRACT_VERSION
                                        );
                                        let _ = socket.write_all(headers.as_bytes()).await;
                                        log.done(304, 0);
                                        if want_close { break 'conn; } else { continue 'conn; }
                                    }
                                }

                                let mime = guess_mime(&ext);
                                let last_mod = mtime.map(http_date);
                                let last_mod_hdr = last_mod.as_ref().map(|s| format!("Last-Modified: {s}\r\n")).unwrap_or_default();

                                let headers = format!(
                                    "HTTP/1.1 200 OK\r\nContent-Type: {mime}\r\nContent-Length: {}\r\nETag: {etag}\r\n{last_mod_hdr}Connection: {connection_header}\r\nx-goblin-web-contract: {}\r\n\r\n",
                                    bytes.len(), crate::CONTRACT_VERSION
                                );
                                if socket.write_all(headers.as_bytes()).await.is_ok() {
                                    let _ = socket.write_all(&bytes).await;
                                }
                                log.done(200, bytes.len());
                                if want_close { break 'conn; } else { continue 'conn; }
                            } else {
                                let not_found_path = root.join("404.html");
                                let (body, content_type) = match tokio::fs::read(&not_found_path).await {
                                    Ok(bytes) => (bytes, "text/html; charset=utf-8"),
                                    Err(_) => (b"404 not found".to_vec(), "text/plain; charset=utf-8"),
                                };
                                let headers = format!(
                                    "HTTP/1.1 404 Not Found\r\nContent-Type: {content_type}\r\nContent-Length: {}\r\nConnection: close\r\nx-goblin-web-contract: {}\r\n\r\n",
                                    body.len(), crate::CONTRACT_VERSION
                                );
                                let _ = socket.write_all(headers.as_bytes()).await;
                                let _ = socket.write_all(&body).await;
                                log.done(404, body.len());
                                break 'conn;
                            }

                        } // end 'conn loop
                    });
                }

                _ = tokio::signal::ctrl_c() => {
                    println!("Ctrl+C received — shutting down GoblinHost");
                    break;
                }
            }
        }

        Ok(())
    }
}

#[derive(thiserror::Error, Debug)]
pub enum HostError {
    #[error("bind failed: {0}")]
    Bind(String),
    #[error("io error: {0}")]
    Io(String),
    #[error("internal: {0}")]
    Internal(String),
}

pub const CONTRACT_VERSION: &str = "v0";

#[derive(Debug)]
enum ExecErr {
    Spawn(String),
    Timeout,
    NonZero(String),
}

static VM_EXEC_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

async fn exec_goblin_script_via_vm(
    script_path: &std::path::Path,
    timeout_ms: u64,
    query_string: &str,
    method: &str,
    path: &str,
    host: &str,
    body: &str,
    request_file_path: &str,
    authorization: &str,
    headers_json: &str,
    auth_user_id: &str,
    auth_email: &str,
    auth_role: &str,
    auth_json: &str,
) -> Result<String, ExecErr> {
    use tokio::time::{timeout, Duration};

    let src = std::fs::read_to_string(script_path)
        .map_err(|e| ExecErr::Spawn(format!("read script: {e}")))?;

    let script_path_str = script_path.to_string_lossy().to_string();

    let query_string = query_string.to_string();
    let method = method.to_string();
    let path = path.to_string();
    let host = host.to_string();
    let body = body.to_string();
    let request_file_path = request_file_path.to_string();
    let authorization = authorization.to_string();
    let headers_json = headers_json.to_string();
    let auth_user_id = auth_user_id.to_string();
    let auth_email = auth_email.to_string();
    let auth_role = auth_role.to_string();
    let auth_json = auth_json.to_string();

    let is_render = goblin_vm::render::is_render_source(&src);

    let task = tokio::task::spawn_blocking(move || {
        let _guard = VM_EXEC_LOCK.lock().unwrap_or_else(|e| e.into_inner());
        std::env::set_var("GOBLIN_NONINTERACTIVE", "1");
        std::env::set_var("GOBLIN_QUERY_STRING", &query_string);
        std::env::set_var("GOBLIN_METHOD", &method);
        std::env::set_var("GOBLIN_PATH", &path);
        std::env::set_var("GOBLIN_HOST", &host);
        std::env::set_var("GOBLIN_BODY", &body);
        std::env::set_var("GOBLIN_REQUEST_FILE", &request_file_path);
        std::env::set_var("GOBLIN_UPLOAD_PATH", &request_file_path);
        std::env::set_var("GOBLIN_AUTHORIZATION", &authorization);
        std::env::set_var("GOBLIN_HEADERS_JSON", &headers_json);
        std::env::set_var("AUTH_USER_ID", &auth_user_id);
        std::env::set_var("AUTH_EMAIL", &auth_email);
        std::env::set_var("AUTH_ROLE", &auth_role);
        std::env::set_var("AUTH_JSON", &auth_json);

        if is_render {
            use goblin_vm::value::Value;
            let html = goblin_vm::render::render_template(&script_path_str, Value::Nil)?;
            let html_str = match html {
                Value::Str(s) => s,
                other => format!("{other:?}"),
            };
            let mut response = goblin_vm::session::ResponseState::default();
            response.status = Some(200);
            response.headers.insert("Content-Type".to_string(), "text/html; charset=utf-8".to_string());
            Ok((html_str, response))
        } else {
            goblin_vm::exec::execute_source_api(&src, &script_path_str)
        }
    });

    match timeout(Duration::from_millis(timeout_ms), task).await {
        Err(_) => Err(ExecErr::Timeout),
        Ok(Ok(Ok((output, response)))) => {
            let status = response.status.unwrap_or(200);
            let headers_obj: serde_json::Map<String, serde_json::Value> = response.headers
                .iter()
                .map(|(k, v)| (k.clone(), serde_json::Value::String(v.clone())))
                .collect();
            let cookies_arr: Vec<serde_json::Value> = response.cookies
                .iter()
                .map(|c| serde_json::Value::String(c.clone()))
                .collect();
            let envelope = serde_json::json!({
                "status": status,
                "headers": headers_obj,
                "cookies": cookies_arr,
                "body": output,
            });
            Ok(envelope.to_string())
        }
        Ok(Ok(Err(e))) => Err(ExecErr::NonZero(e.to_string())),
        Ok(Err(e)) => Err(ExecErr::Spawn(format!("task panic: {e}"))),
    }
}

async fn exec_goblin_script_via_cli_timeout(
    script_path: &std::path::Path,
    timeout_ms: u64,
    query_string: &str,
    method: &str,
    path: &str,
    host: &str,
    body: &str,
    request_file_path: &str,
    authorization: &str,
    headers_json: &str,
    auth_user_id: &str,
    auth_email: &str,
    auth_role: &str,
    auth_json: &str,
) -> Result<String, ExecErr> {
    use tokio::process::Command;
    use tokio::time::{timeout, Duration};
    use std::process::Stdio;

    let mut cmd = Command::new("goblin");
    cmd.kill_on_drop(true);
    cmd.arg(script_path.as_os_str())
       .env("GOBLIN_NONINTERACTIVE", "1")
       .env("GOBLIN_QUERY_STRING", query_string)
       .env("GOBLIN_METHOD", method)
       .env("GOBLIN_PATH", path)
       .env("GOBLIN_HOST", host)
       .env("GOBLIN_BODY", body)
       .env("GOBLIN_REQUEST_FILE", request_file_path)
       .env("GOBLIN_UPLOAD_PATH", request_file_path)
       .env("GOBLIN_AUTHORIZATION", authorization)
       .env("GOBLIN_HEADERS_JSON", headers_json)
       .env("AUTH_USER_ID", auth_user_id)
       .env("AUTH_EMAIL", auth_email)
       .env("AUTH_ROLE", auth_role)
       .env("AUTH_JSON", auth_json)
       .stdin(Stdio::null())
       .stdout(Stdio::piped())
       .stderr(Stdio::piped());
    let child = cmd.spawn()
        .map_err(|e| ExecErr::Spawn(format!("failed to spawn goblin CLI: {e}")))?;

    match timeout(Duration::from_millis(timeout_ms), child.wait_with_output()).await {
        Err(_) => Err(ExecErr::Timeout),
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
