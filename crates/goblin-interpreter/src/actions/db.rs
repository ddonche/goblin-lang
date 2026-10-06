use crate::{Diag, Session, Span, Value};
use crate::actions::utils::want_str;
use crate::diagnostics::rtcode;
use goblin_diagnostics::{Diagnostic, Severity};
use indexmap::IndexMap;

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

fn to_db_params(params: &[Value], sp: &Span, fn_name: &str) -> Result<Vec<goblin_db::DbValue>, Diag> {
    params.iter().map(|v| Ok(match v {
        Value::Str(s) => goblin_db::DbValue::Str(s.clone()),
        Value::Int(n) => goblin_db::DbValue::Int(*n),
        Value::Float(x) => goblin_db::DbValue::Float(*x),
        Value::Bool(b) => goblin_db::DbValue::Bool(*b),
        Value::Nil => goblin_db::DbValue::Null,
        _ => {
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
    })).collect()
}

fn row_to_value(row: goblin_db::DbRow) -> Value {
    let mut map = IndexMap::new();
    for (name, cell) in row {
        map.insert(name, match cell {
            goblin_db::DbValue::Null => Value::Nil,
            goblin_db::DbValue::Bool(b) => Value::Bool(b),
            goblin_db::DbValue::Int(n) => Value::Int(n),
            goblin_db::DbValue::Float(x) => Value::Float(x),
            goblin_db::DbValue::Str(s) => Value::Str(s),
        });
    }
    Value::MapOrd(map)
}

fn db_error(e: goblin_db::DbError, sp: &Span) -> Diag {
    use goblin_db::DbError;
    let (code, kind, help, link) = match &e {
        DbError::MissingUrl => (rtcode::DB_MISSING_URL, "db-missing-url",
            "Set DATABASE_URL in the environment before calling a database action.", "DB0004"),
        DbError::Connect(_) => (rtcode::DB_CONNECT_FAILED, "db-connect-failed",
            "Verify DATABASE_URL and make sure the database is reachable.", "DB0001"),
        DbError::Query(_) => (rtcode::DB_QUERY_FAILED, "db-query-failed",
            "Check SQL syntax and parameter bindings.", "DB0002"),
        DbError::Exec(_) => (rtcode::DB_EXEC_FAILED, "db-exec-failed",
            "Check SQL syntax and parameter bindings.", "DB0005"),
        DbError::Runtime(_) => (rtcode::INTERNAL, "db-runtime-failed",
            "The database runtime failed; this is an internal error.", "R0000"),
    };
    Diagnostic::new_with_code(Severity::Error, code, kind, &e.to_string(), sp.clone())
        .with_help(help)
        .with_link(&format!("https://goblinlang.org/docs/errors#{link}"))
}

fn want_two_args(args: &[Value], sp: &Span, usage: &str) -> Result<(), Diag> {
    if args.len() != 2 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY,
                "wrong-arity",
                &format!("Wrong number of arguments (expected 2, got {})", args.len()),
                sp.clone(),
            )
            .with_help(usage)
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }
    Ok(())
}

pub fn db_query(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    want_two_args(args, sp, "Usage: db_query(sql, params)")?;
    let sql = want_str(&args[0], "db_query sql", sp)?.to_string();
    let params = to_db_params(want_params_array(&args[1], sp, "db_query")?, sp, "db_query")?;
    let rows = goblin_db::query(&sql, &params).map_err(|e| db_error(e, sp))?;
    Ok(Value::Array(rows.into_iter().map(row_to_value).collect()))
}

pub fn db_query_one(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    want_two_args(args, sp, "Usage: db_query_one(sql, params)")?;
    let sql = want_str(&args[0], "db_query_one sql", sp)?.to_string();
    let params = to_db_params(want_params_array(&args[1], sp, "db_query_one")?, sp, "db_query_one")?;
    let row = goblin_db::query_one(&sql, &params).map_err(|e| db_error(e, sp))?;
    Ok(row.map(row_to_value).unwrap_or(Value::Nil))
}

pub fn db_exec(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    want_two_args(args, sp, "Usage: db_exec(sql, params)")?;
    let sql = want_str(&args[0], "db_exec sql", sp)?.to_string();
    let params = to_db_params(want_params_array(&args[1], sp, "db_exec")?, sp, "db_exec")?;
    let rows_affected = goblin_db::exec(&sql, &params).map_err(|e| db_error(e, sp))?;
    let mut out = IndexMap::new();
    out.insert("ok".to_string(), Value::Bool(true));
    out.insert("rows_affected".to_string(), Value::Int(rows_affected as i64));
    Ok(Value::MapOrd(out))
}
