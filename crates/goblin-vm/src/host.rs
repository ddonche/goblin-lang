//! Calls that wait on the outside world: `:db_query`, `:db_query_one`,
//! `:db_exec` and the `:http_*` builtins.
//!
//! Outside an actor they run on the calling thread and wait, as before.
//! Inside an actor's handler they do not hold the actor's worker: the call
//! is started elsewhere (a database statement on the pool's own runtime, an
//! HTTP request on a thread kept for blocking calls), the handler stops, and
//! the worker runs other actors until the answer is there (`actor::start_io`).

use crate::builtins::fmt_value_raw;
use crate::collections;
use crate::error::GoblinError;
use crate::value::{BuiltinId, Value};

/// The builtins that go through here.
pub fn is_host_call(id: BuiltinId) -> bool {
    matches!(id,
        BuiltinId::DbQuery | BuiltinId::DbQueryOne | BuiltinId::DbExec
        | BuiltinId::HttpGet | BuiltinId::HttpPost | BuiltinId::HttpPut
        | BuiltinId::HttpDelete | BuiltinId::HttpRequest)
}

/// A host call with its arguments checked and copied out of the session,
/// so it can run on another thread.
pub enum HostCall {
    Db { id: BuiltinId, sql: String, params: Vec<DbParam> },
    Http { method: String, url: String, body: Option<String>, content_type: String, headers: Vec<(String, String)> },
}

/// A database parameter.
pub enum DbParam { Null, Bool(bool), Int(i64), Float(f64), Str(String) }

/// Turns the outcome into a value. Runs on the thread of the session that
/// made the call, once the outcome is there.
pub type Finish = Box<dyn FnOnce() -> Result<Value, GoblinError> + Send>;

fn arg(args: &[Value], i: usize, id: BuiltinId) -> Result<Value, GoblinError> {
    args.get(i).cloned().ok_or_else(|| GoblinError::Runtime(format!("builtin {:?}: expected arg {i}", id)))
}

fn text(v: Value, what: &'static str) -> Result<String, GoblinError> {
    match v {
        Value::Str(s) => Ok(s.into_string()),
        other => Err(GoblinError::type_error("str", other.type_name(), what)),
    }
}

/// Check a host call's arguments; the errors are the ones the builtins give.
pub fn prepare(id: BuiltinId, args: &[Value]) -> Result<HostCall, GoblinError> {
    let read = |i: usize| arg(args, i, id);
    match id {
        BuiltinId::DbQuery | BuiltinId::DbQueryOne | BuiltinId::DbExec => {
            let fn_name = db_name(id);
            if args.len() != 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: fn_name.into() });
            }
            let sql = text(read(0)?, fn_name)?;
            let items = match read(1)? {
                v @ (Value::Array(_) | Value::Collection(_)) => crate::builtins::value_to_items(v, "db params")?,
                Value::Seq(s) => s.items,
                _ => return Err(GoblinError::Runtime(format!("{fn_name} params argument must be an array"))),
            };
            let mut params = Vec::with_capacity(items.len());
            for v in &items {
                params.push(match v {
                    Value::Str(s) => DbParam::Str(s.to_string()),
                    Value::Int(n) => DbParam::Int(*n),
                    Value::Float(x) => DbParam::Float(*x),
                    Value::Bool(b) => DbParam::Bool(*b),
                    Value::Nil => DbParam::Null,
                    _ => return Err(GoblinError::Runtime(format!(
                        "{fn_name} got an unsupported parameter type (supported: str, int, float, bool, nil)"))),
                });
            }
            Ok(HostCall::Db { id, sql, params })
        }
        BuiltinId::HttpGet | BuiltinId::HttpDelete => {
            let (name, method) = if id == BuiltinId::HttpGet { ("http_get", "GET") } else { ("http_delete", "DELETE") };
            if args.is_empty() || args.len() > 2 {
                return Err(GoblinError::Runtime(format!("{name}: expected 1-2 args (url[, headers]), got {}", args.len())));
            }
            let url = text(read(0)?, name)?;
            let headers = if args.len() == 2 { http_extract_headers(read(1)?, name)? } else { vec![] };
            Ok(HostCall::Http { method: method.into(), url, body: None, content_type: String::new(), headers })
        }
        BuiltinId::HttpPost | BuiltinId::HttpPut => {
            let (name, method, url_what, body_what) = if id == BuiltinId::HttpPost {
                ("http_post", "POST", "http_post url", "http_post body")
            } else {
                ("http_put", "PUT", "http_put url", "http_put body")
            };
            if args.len() < 2 || args.len() > 4 {
                return Err(GoblinError::Runtime(format!("{name}: expected 2-4 args (url, body[, content_type[, headers]]), got {}", args.len())));
            }
            let url = text(read(0)?, url_what)?;
            let body = text(read(1)?, body_what)?;
            let content_type = if args.len() >= 3 {
                match read(2)? { Value::Str(s) => s.into_string(), _ => "application/json".to_string() }
            } else { "application/json".to_string() };
            let headers = if args.len() == 4 { http_extract_headers(read(3)?, name)? } else { vec![] };
            Ok(HostCall::Http { method: method.into(), url, body: Some(body), content_type, headers })
        }
        BuiltinId::HttpRequest => {
            if args.len() != 5 {
                return Err(GoblinError::Runtime(format!("http_request: expected 5 args (method, url, body, content_type, headers), got {}", args.len())));
            }
            let method = text(read(0)?, "http_request method")?;
            let url = text(read(1)?, "http_request url")?;
            let body = match read(2)? {
                Value::Str(s) => Some(s.into_string()),
                Value::Nil => None,
                other => return Err(GoblinError::type_error("str or nil", other.type_name(), "http_request body")),
            };
            let content_type = match read(3)? {
                Value::Str(s) => s.into_string(),
                Value::Nil => "application/json".to_string(),
                other => return Err(GoblinError::type_error("str or nil", other.type_name(), "http_request content_type")),
            };
            let headers = http_extract_headers(read(4)?, "http_request")?;
            Ok(HostCall::Http { method, url, body, content_type, headers })
        }
        _ => Err(GoblinError::Runtime(format!("internal: {:?} is not a host call", id))),
    }
}

fn db_name(id: BuiltinId) -> &'static str {
    match id { BuiltinId::DbQuery => "db_query", BuiltinId::DbQueryOne => "db_query_one", _ => "db_exec" }
}

fn http_extract_headers(v: Value, caller: &str) -> Result<Vec<(String, String)>, GoblinError> {
    match v {
        Value::Map(m) => Ok(m.into_iter().map(|(k, v)| (k, match v {
            Value::Str(s) => s.into_string(), other => fmt_value_raw(&other),
        })).collect()),
        Value::MapOrd(m) => Ok(m.into_iter().map(|(k, v)| (k, match v {
            Value::Str(s) => s.into_string(), other => fmt_value_raw(&other),
        })).collect()),
        Value::Collection(c) if collections::is_map_collection(&c) => Ok(collections::to_pairs(&c).into_iter().map(|(k, v)| (
            match k { Value::Str(s) => s.into_string(), other => fmt_value_raw(&other) },
            match v { Value::Str(s) => s.into_string(), other => fmt_value_raw(&other) },
        )).collect()),
        Value::Nil => Ok(vec![]),
        other => Err(GoblinError::Runtime(format!("{caller}: headers must be a map, got {}", other.type_name()))),
    }
}

#[cfg(not(target_arch = "wasm32"))]
mod native {
    use super::*;
    use goblin_db::{DbError, DbRow, DbValue, Kind, Outcome};

    fn db_params(params: Vec<DbParam>) -> Vec<DbValue> {
        params.into_iter().map(|p| match p {
            DbParam::Null => DbValue::Null,
            DbParam::Bool(b) => DbValue::Bool(b),
            DbParam::Int(n) => DbValue::Int(n),
            DbParam::Float(x) => DbValue::Float(x),
            DbParam::Str(s) => DbValue::Str(s),
        }).collect()
    }

    fn kind(id: BuiltinId) -> Kind {
        match id { BuiltinId::DbQuery => Kind::Query, BuiltinId::DbQueryOne => Kind::QueryOne, _ => Kind::Exec }
    }

    // Rows come back as map collections (and query results as an array
    // collection): shared on read, where the legacy MapOrd/Array forms were
    // deep-copied every time a variable holding them was read.
    fn row_value(row: DbRow) -> Value {
        collections::into_collection(Value::MapOrd(row.into_iter().map(|(k, c)| (k, match c {
            DbValue::Null => Value::Nil,
            DbValue::Bool(b) => Value::Bool(b),
            DbValue::Int(n) => Value::Int(n),
            DbValue::Float(x) => Value::Float(x),
            DbValue::Str(s) => Value::Str(s.into()),
        })).collect::<indexmap::IndexMap<_, _>>().into()))
    }

    fn db_value(r: Result<Outcome, DbError>) -> Result<Value, GoblinError> {
        match r.map_err(|e| GoblinError::Runtime(e.to_string()))? {
            Outcome::Rows(rows) => Ok(collections::into_collection(Value::Array(rows.into_iter().map(row_value).collect()))),
            Outcome::Row(row) => Ok(row.map(row_value).unwrap_or(Value::Nil)),
            Outcome::Affected(n) => {
                let mut m = indexmap::IndexMap::new();
                m.insert("ok".to_string(), Value::Bool(true));
                m.insert("rows_affected".to_string(), Value::Int(n as i64));
                Ok(collections::into_collection(Value::MapOrd(Box::new(m))))
            }
        }
    }

    fn http_value(r: Result<goblin_http::HttpResponse, goblin_http::HttpError>) -> Result<Value, GoblinError> {
        match r {
            Ok(resp) => {
                let mut m = std::collections::BTreeMap::new();
                m.insert("status".to_string(), Value::Int(resp.status as i64));
                m.insert("body".to_string(), Value::Str(resp.body.into()));
                m.insert("ok".to_string(), Value::Bool(resp.status < 400));
                Ok(Value::Map(m))
            }
            Err(e) => Err(GoblinError::Runtime(format!("http error: {e}"))),
        }
    }

    impl HostCall {
        /// Run on this thread and wait for the answer.
        pub fn run_here(self) -> Result<Value, GoblinError> {
            match self {
                HostCall::Db { id, sql, params } => {
                    let ps = db_params(params);
                    db_value(match kind(id) {
                        Kind::Query => goblin_db::query(&sql, &ps).map(Outcome::Rows),
                        Kind::QueryOne => goblin_db::query_one(&sql, &ps).map(Outcome::Row),
                        Kind::Exec => goblin_db::exec(&sql, &ps).map(Outcome::Affected),
                    })
                }
                HostCall::Http { method, url, body, content_type, headers } => {
                    http_value(goblin_http::request(&method, &url, body.as_deref(), &content_type, &headers))
                }
            }
        }

        /// Start without waiting; `done` gets the outcome on another thread.
        pub fn start(self, done: impl FnOnce(Finish) + Send + 'static) {
            match self {
                HostCall::Db { id, sql, params } => {
                    goblin_db::start(kind(id), sql, db_params(params), move |r| done(Box::new(move || db_value(r))));
                }
                HostCall::Http { method, url, body, content_type, headers } => {
                    crate::actor::run_blocking(Box::new(move || {
                        let r = goblin_http::request(&method, &url, body.as_deref(), &content_type, &headers);
                        done(Box::new(move || http_value(r)));
                    }));
                }
            }
        }
    }
}

#[cfg(target_arch = "wasm32")]
impl HostCall {
    fn unavailable(&self) -> GoblinError {
        match self {
            HostCall::Db { id, .. } => GoblinError::Runtime(format!("{}: databases are not available in WASM builds", db_name(*id))),
            HostCall::Http { .. } => GoblinError::Runtime("outbound HTTP is not available in WASM builds".into()),
        }
    }
    pub fn run_here(self) -> Result<Value, GoblinError> { Err(self.unavailable()) }
    pub fn start(self, done: impl FnOnce(Finish) + Send + 'static) {
        let e = self.unavailable().to_string();
        done(Box::new(move || Err(GoblinError::Runtime(e))));
    }
}
