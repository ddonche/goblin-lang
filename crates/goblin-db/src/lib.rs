//! Postgres access shared by the interpreter and the VM (`db_query`,
//! `db_query_one`, `db_exec`).
//!
//! Both engines convert their own values to [`DbValue`] parameters and convert
//! result cells back, so the two runtimes see identical database behaviour.
//!
//! Connections are pooled per process: the first call for a given
//! `DATABASE_URL` builds one Tokio runtime and one `PgPool`, and every later
//! call reuses them. (Before this crate, every call built a new runtime and
//! opened a new connection.) The pool size defaults to 10 and can be set with
//! `GOBLIN_DB_POOL_MAX`.

use sqlx::postgres::{PgPoolOptions, PgRow};
use sqlx::{Column, PgPool, Row};
use std::collections::HashMap;
use std::sync::{mpsc, Mutex, OnceLock};

/// A parameter or result cell.
#[derive(Debug, Clone, PartialEq)]
pub enum DbValue {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(String),
}

/// One result row: column name → cell, in column order.
pub type DbRow = Vec<(String, DbValue)>;

#[derive(Debug, Clone, PartialEq)]
pub enum DbError {
    /// `DATABASE_URL` is not set.
    MissingUrl,
    Connect(String),
    Query(String),
    Exec(String),
    Runtime(String),
}

impl std::fmt::Display for DbError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DbError::MissingUrl => write!(f, "DATABASE_URL is not set"),
            DbError::Connect(e) => write!(f, "failed to connect to database: {e}"),
            DbError::Query(e) => write!(f, "database query failed: {e}"),
            DbError::Exec(e) => write!(f, "database exec failed: {e}"),
            DbError::Runtime(e) => write!(f, "database runtime error: {e}"),
        }
    }
}

struct Pooled {
    rt: tokio::runtime::Runtime,
    pool: PgPool,
}

fn pools() -> &'static Mutex<HashMap<String, &'static Pooled>> {
    static POOLS: OnceLock<Mutex<HashMap<String, &'static Pooled>>> = OnceLock::new();
    POOLS.get_or_init(|| Mutex::new(HashMap::new()))
}

fn pooled() -> Result<&'static Pooled, DbError> {
    let url = std::env::var("DATABASE_URL").map_err(|_| DbError::MissingUrl)?;
    let mut map = pools().lock().unwrap_or_else(|e| e.into_inner());
    if let Some(p) = map.get(&url) {
        return Ok(p);
    }
    let rt = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(2)
        .thread_name("goblin-db")
        .enable_all()
        .build()
        .map_err(|e| DbError::Runtime(e.to_string()))?;
    let max = std::env::var("GOBLIN_DB_POOL_MAX").ok().and_then(|v| v.parse().ok()).unwrap_or(10);
    // Connect on the pool's own runtime (not `block_on`, which panics when the
    // caller is already inside a Tokio runtime).
    let (tx, rx) = mpsc::sync_channel(1);
    let connect_url = url.clone();
    rt.spawn(async move {
        let _ = tx.send(PgPoolOptions::new().max_connections(max).connect(&connect_url).await);
    });
    let pool = rx.recv()
        .map_err(|e| DbError::Runtime(e.to_string()))?
        .map_err(|e| DbError::Connect(e.to_string()))?;
    // Lives for the rest of the process, like the connection pool it holds.
    let p: &'static Pooled = Box::leak(Box::new(Pooled { rt, pool }));
    map.insert(url, p);
    Ok(p)
}

/// Run `f` on the pool's runtime and wait for it. Works whether or not the
/// caller is itself inside a Tokio runtime (goblin-host runs the VM from one).
fn run<T: Send + 'static>(
    f: impl FnOnce(PgPool) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<T, DbError>> + Send>>,
) -> Result<T, DbError> {
    let p = pooled()?;
    let fut = f(p.pool.clone());
    let (tx, rx) = mpsc::sync_channel(1);
    p.rt.spawn(async move {
        let _ = tx.send(fut.await);
    });
    rx.recv().map_err(|e| DbError::Runtime(e.to_string()))?
}

fn bind<'q>(
    mut q: sqlx::query::Query<'q, sqlx::Postgres, sqlx::postgres::PgArguments>,
    params: &[DbValue],
) -> sqlx::query::Query<'q, sqlx::Postgres, sqlx::postgres::PgArguments> {
    for p in params {
        q = match p {
            DbValue::Str(s) => q.bind(s.clone()),
            DbValue::Int(n) => q.bind(*n),
            DbValue::Float(x) => q.bind(*x),
            DbValue::Bool(b) => q.bind(*b),
            DbValue::Null => q.bind(Option::<String>::None),
        };
    }
    q
}

/// Converts a result cell. Text, integers, floats and booleans map directly;
/// `numeric` reads as a float; `json`/`jsonb` as their JSON text; dates and
/// times as Postgres' text form. Other types (and SQL NULL) read as Null.
fn cell(row: &PgRow, col: &str) -> DbValue {
    use rust_decimal::prelude::ToPrimitive;
    if let Ok(v) = row.try_get::<String, _>(col) { return DbValue::Str(v); }
    if let Ok(v) = row.try_get::<i64, _>(col) { return DbValue::Int(v); }
    if let Ok(v) = row.try_get::<i32, _>(col) { return DbValue::Int(v as i64); }
    if let Ok(v) = row.try_get::<i16, _>(col) { return DbValue::Int(v as i64); }
    if let Ok(v) = row.try_get::<f64, _>(col) { return DbValue::Float(v); }
    if let Ok(v) = row.try_get::<f32, _>(col) { return DbValue::Float(v as f64); }
    if let Ok(v) = row.try_get::<bool, _>(col) { return DbValue::Bool(v); }
    if let Ok(v) = row.try_get::<rust_decimal::Decimal, _>(col) {
        return v.to_f64().map(DbValue::Float).unwrap_or(DbValue::Null);
    }
    if let Ok(v) = row.try_get::<serde_json::Value, _>(col) { return DbValue::Str(v.to_string()); }
    if let Ok(v) = row.try_get::<chrono::DateTime<chrono::Utc>, _>(col) {
        return DbValue::Str(v.format("%Y-%m-%d %H:%M:%S%.f+00").to_string());
    }
    if let Ok(v) = row.try_get::<chrono::NaiveDateTime, _>(col) {
        return DbValue::Str(v.format("%Y-%m-%d %H:%M:%S%.f").to_string());
    }
    if let Ok(v) = row.try_get::<chrono::NaiveDate, _>(col) { return DbValue::Str(v.to_string()); }
    if let Ok(v) = row.try_get::<chrono::NaiveTime, _>(col) { return DbValue::Str(v.to_string()); }
    DbValue::Null
}

fn to_row(row: &PgRow) -> DbRow {
    row.columns().iter().map(|c| (c.name().to_string(), cell(row, c.name()))).collect()
}

pub fn query(sql: &str, params: &[DbValue]) -> Result<Vec<DbRow>, DbError> {
    let (sql, params) = (sql.to_string(), params.to_vec());
    run(move |pool| Box::pin(async move {
        let rows = bind(sqlx::query(&sql), &params).fetch_all(&pool).await
            .map_err(|e| DbError::Query(e.to_string()))?;
        Ok(rows.iter().map(to_row).collect())
    }))
}

pub fn query_one(sql: &str, params: &[DbValue]) -> Result<Option<DbRow>, DbError> {
    let (sql, params) = (sql.to_string(), params.to_vec());
    run(move |pool| Box::pin(async move {
        let row = bind(sqlx::query(&sql), &params).fetch_optional(&pool).await
            .map_err(|e| DbError::Query(e.to_string()))?;
        Ok(row.as_ref().map(to_row))
    }))
}

/// Returns the number of rows affected.
pub fn exec(sql: &str, params: &[DbValue]) -> Result<u64, DbError> {
    let (sql, params) = (sql.to_string(), params.to_vec());
    run(move |pool| Box::pin(async move {
        let res = bind(sqlx::query(&sql), &params).execute(&pool).await
            .map_err(|e| DbError::Exec(e.to_string()))?;
        Ok(res.rows_affected())
    }))
}
