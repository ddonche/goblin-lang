use crate::{Diag, Session, Span, Value};
use crate::actions::utils::want_str;
use crate::diagnostics::rtcode;
use goblin_diagnostics::{Diagnostic, Severity};
use indexmap::IndexMap;
use sqlx::{Column, PgPool, Row};

fn want_params_array<'a>(v: &'a Value, sp: &Span, fn_name: &str) -> Result<&'a [Value], Diag> {
    match v {
        Value::Array(xs) => Ok(xs),
        _ => Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::DB_INVALID_PARAMS,
                "db-invalid-params",
                format!("{fn_name} params argument must be an array"),
                sp.clone(),
            )
            .with_help("Pass an array like [user_id, portal] as the second argument.")
            .with_link("https://goblinlang.org/docs/errors#DB0003"),
        ),
    }
}

fn bind_query_param<'q>(
    query: sqlx::query::Query<'q, sqlx::Postgres, sqlx::postgres::PgArguments>,
    v: &Value,
    sp: &Span,
    fn_name: &str,
) -> Result<sqlx::query::Query<'q, sqlx::Postgres, sqlx::postgres::PgArguments>, Diag> {
    let q = match v {
        Value::Str(s) => query.bind(s.clone()),
        Value::Int(n) => query.bind(*n),
        Value::Float(x) => query.bind(*x),
        Value::Bool(b) => query.bind(*b),
        Value::Nil => query.bind(Option::<String>::None),
        other => {
            let _ = other;
            return Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    rtcode::DB_INVALID_PARAMS,
                    "db-invalid-params",
                    format!("{fn_name} got an unsupported parameter type"),
                    sp.clone(),
                )
                .with_help("Supported database parameter types are Str, Int, Float, Bool, and Nil.")
                .with_link("https://goblinlang.org/docs/errors#DB0003"),
            );
        }
    };
    Ok(q)
}

fn sql_cell_to_value(row: &sqlx::postgres::PgRow, col_name: &str) -> Value {
    if let Ok(v) = row.try_get::<String, _>(col_name) {
        return Value::Str(v);
    }
    if let Ok(v) = row.try_get::<i64, _>(col_name) {
        return Value::Int(v);
    }
    if let Ok(v) = row.try_get::<i32, _>(col_name) {
        return Value::Int(v as i64);
    }
    if let Ok(v) = row.try_get::<f64, _>(col_name) {
        return Value::Float(v);
    }
    if let Ok(v) = row.try_get::<bool, _>(col_name) {
        return Value::Bool(v);
    }

    Value::Nil
}

fn db_url(sp: &Span) -> Result<String, Diag> {
    std::env::var("DATABASE_URL").map_err(|_| {
        Diagnostic::new_with_code(
            Severity::Error,
            rtcode::DB_MISSING_URL,
            "db-missing-url",
            "DATABASE_URL is not set",
            sp.clone(),
        )
        .with_help("Set DATABASE_URL in the environment before calling a database action.")
        .with_link("https://goblinlang.org/docs/errors#DB0004")
    })
}

fn runtime(sp: &Span) -> Result<tokio::runtime::Runtime, Diag> {
    tokio::runtime::Runtime::new().map_err(|e| {
        Diagnostic::new_with_code(
            Severity::Error,
            rtcode::INTERNAL,
            "tokio-runtime-failed",
            &format!("failed to create tokio runtime: {e}"),
            sp.clone(),
        )
        .with_help("Check that Tokio is available and configured in this interpreter crate.")
        .with_link("https://goblinlang.org/docs/errors#R0000")
    })
}

pub fn db_query(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 2 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY,
                "wrong-arity",
                &format!("Wrong number of arguments (expected 2, got {})", args.len()),
                sp.clone(),
            )
            .with_help("Usage: db_query(sql, params)")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }

    let sql = want_str(&args[0], "db_query sql", sp)?.to_string();
    let params = want_params_array(&args[1], sp, "db_query")?;
    let db_url = db_url(sp)?;
    let rt = runtime(sp)?;

    let rows = rt.block_on(async {
        let pool = PgPool::connect(&db_url).await.map_err(|e| {
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::DB_CONNECT_FAILED,
                "db-connect-failed",
                &format!("failed to connect to database: {e}"),
                sp.clone(),
            )
            .with_help("Verify DATABASE_URL and make sure the database is reachable.")
            .with_link("https://goblinlang.org/docs/errors#DB0001")
        })?;

        let mut query = sqlx::query(&sql);

        for param in params {
            query = bind_query_param(query, param, sp, "db_query")?;
        }

        query.fetch_all(&pool).await.map_err(|e| {
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::DB_QUERY_FAILED,
                "db-query-failed",
                &format!("database query failed: {e}"),
                sp.clone(),
            )
            .with_help("Check SQL syntax and parameter bindings.")
            .with_link("https://goblinlang.org/docs/errors#DB0002")
        })
    })?;

    let mut out = Vec::with_capacity(rows.len());

    for row in rows {
        let mut map = IndexMap::new();

        for col in row.columns() {
            let name = col.name().to_string();
            let value = sql_cell_to_value(&row, &name);
            map.insert(name, value);
        }

        out.push(Value::MapOrd(map));
    }

    Ok(Value::Array(out))
}

pub fn db_query_one(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 2 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY,
                "wrong-arity",
                &format!("Wrong number of arguments (expected 2, got {})", args.len()),
                sp.clone(),
            )
            .with_help("Usage: db_query_one(sql, params)")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }

    let sql = want_str(&args[0], "db_query_one sql", sp)?.to_string();
    let params = want_params_array(&args[1], sp, "db_query_one")?;
    let db_url = db_url(sp)?;
    let rt = runtime(sp)?;

    let maybe_row = rt.block_on(async {
        let pool = PgPool::connect(&db_url).await.map_err(|e| {
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::DB_CONNECT_FAILED,
                "db-connect-failed",
                &format!("failed to connect to database: {e}"),
                sp.clone(),
            )
            .with_help("Verify DATABASE_URL and make sure the database is reachable.")
            .with_link("https://goblinlang.org/docs/errors#DB0001")
        })?;

        let mut query = sqlx::query(&sql);

        for param in params {
            query = bind_query_param(query, param, sp, "db_query_one")?;
        }

        query.fetch_optional(&pool).await.map_err(|e| {
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::DB_QUERY_FAILED,
                "db-query-failed",
                &format!("database query failed: {e}"),
                sp.clone(),
            )
            .with_help("Check SQL syntax and parameter bindings.")
            .with_link("https://goblinlang.org/docs/errors#DB0002")
        })
    })?;

    let Some(row) = maybe_row else {
        return Ok(Value::Nil);
    };

    let mut map = IndexMap::new();

    for col in row.columns() {
        let name = col.name().to_string();
        let value = sql_cell_to_value(&row, &name);
        map.insert(name, value);
    }

    Ok(Value::MapOrd(map))
}

pub fn db_exec(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 2 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY,
                "wrong-arity",
                &format!("Wrong number of arguments (expected 2, got {})", args.len()),
                sp.clone(),
            )
            .with_help("Usage: db_exec(sql, params)")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }

    let sql = want_str(&args[0], "db_exec sql", sp)?.to_string();
    let params = want_params_array(&args[1], sp, "db_exec")?;
    let db_url = db_url(sp)?;
    let rt = runtime(sp)?;

    let rows_affected = rt.block_on(async {
        let pool = PgPool::connect(&db_url).await.map_err(|e| {
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::DB_CONNECT_FAILED,
                "db-connect-failed",
                &format!("failed to connect to database: {e}"),
                sp.clone(),
            )
            .with_help("Verify DATABASE_URL and make sure the database is reachable.")
            .with_link("https://goblinlang.org/docs/errors#DB0001")
        })?;

        let mut query = sqlx::query(&sql);

        for param in params {
            query = bind_query_param(query, param, sp, "db_exec")?;
        }

        let result = query.execute(&pool).await.map_err(|e| {
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::DB_EXEC_FAILED,
                "db-exec-failed",
                &format!("database exec failed: {e}"),
                sp.clone(),
            )
            .with_help("Check SQL syntax and parameter bindings.")
            .with_link("https://goblinlang.org/docs/errors#DB0005")
        })?;

        Ok::<u64, Diag>(result.rows_affected())
    })?;

    let mut out = IndexMap::new();
    out.insert("ok".to_string(), Value::Bool(true));
    out.insert("rows_affected".to_string(), Value::Int(rows_affected as i64));

    Ok(Value::MapOrd(out))
}