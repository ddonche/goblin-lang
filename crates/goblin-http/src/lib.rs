/// Synchronous outbound HTTP for the Goblin VM builtin layer.
///
/// No async, no tokio — safe to call directly from the VM dispatch loop.
/// 4xx/5xx responses are returned as Ok with the status code; only network-level
/// failures (connection refused, DNS failure, timeout) return Err.

/// Successful or HTTP-error response from an outbound request.
pub struct HttpResponse {
    pub status: u16,
    pub body: String,
}

#[derive(Debug)]
pub enum HttpError {
    Transport(String),
}

impl std::fmt::Display for HttpError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HttpError::Transport(e) => write!(f, "transport error: {e}"),
        }
    }
}

impl std::error::Error for HttpError {}

pub type Headers = Vec<(String, String)>;

fn make_agent() -> ureq::Agent {
    ureq::AgentBuilder::new()
        .timeout_connect(std::time::Duration::from_secs(15))
        .timeout_read(std::time::Duration::from_secs(60))
        .build()
}

/// Perform an HTTP request. `body` and `content_type` are only sent when `body` is Some.
/// On 4xx/5xx the response body is still captured and returned as Ok.
pub fn request(
    method: &str,
    url: &str,
    body: Option<&str>,
    content_type: &str,
    headers: &Headers,
) -> Result<HttpResponse, HttpError> {
    let agent = make_agent();
    let mut req = agent.request(method, url);
    for (k, v) in headers {
        req = req.set(k.as_str(), v.as_str());
    }
    let result = if let Some(body_str) = body {
        if !content_type.is_empty() {
            req = req.set("Content-Type", content_type);
        }
        req.send_string(body_str)
    } else {
        req.call()
    };
    match result {
        Ok(resp) => {
            let status = resp.status();
            let body = resp.into_string()
                .map_err(|e| HttpError::Transport(e.to_string()))?;
            Ok(HttpResponse { status, body })
        }
        Err(ureq::Error::Status(code, resp)) => {
            let body = resp.into_string().unwrap_or_default();
            Ok(HttpResponse { status: code, body })
        }
        Err(ureq::Error::Transport(t)) => {
            Err(HttpError::Transport(t.to_string()))
        }
    }
}

pub fn get(url: &str, headers: &Headers) -> Result<HttpResponse, HttpError> {
    request("GET", url, None, "", headers)
}

pub fn post(url: &str, body: &str, content_type: &str, headers: &Headers) -> Result<HttpResponse, HttpError> {
    request("POST", url, Some(body), content_type, headers)
}

pub fn put(url: &str, body: &str, content_type: &str, headers: &Headers) -> Result<HttpResponse, HttpError> {
    request("PUT", url, Some(body), content_type, headers)
}

pub fn delete(url: &str, headers: &Headers) -> Result<HttpResponse, HttpError> {
    request("DELETE", url, None, "", headers)
}
