use goblin_parser::Parser;
use goblin_lexer::{lex, Token, Diagnostic};

fn join_diags(diags: Vec<Diagnostic>) -> String {
    let mut out = String::new();
    for (i, d) in diags.into_iter().enumerate() {
        if i > 0 { out.push('\n'); }
        out.push_str(&format!("{}: {}", d.severity, d.message));
    }
    out
}

fn lex_ok(src: &str) -> Vec<Token> {
    match lex(src, "<test>") {
        Ok(toks) => toks,
        Err(diags) => panic!("{}", join_diags(diags)),
    }
}

#[test]
fn assignment_at_stmt_boundary_ok() {
    let toks = lex_ok("x = 5\ny = 2\nx + y\n");
    let p = Parser::new(&toks);
    let res = p.parse_module();
    assert!(res.is_ok(), "expected OK, got: {:#?}", res);
}

#[test]
fn assignment_inside_parens_is_forbidden() {
    let toks = lex_ok("a + (b = 3)\n");
    let p = Parser::new(&toks);
    let res = p.parse_module();
    assert!(res.is_err(), "expected error, got OK");
    let all = res.err().map(join_diags).unwrap();
    assert!(all.contains("assignment not allowed in expression"), "diagnostics:\n{}", all);
}

#[test]
fn assignment_in_call_args_is_forbidden() {
    let toks = lex_ok("foo(bar = 1)\n");
    let p = Parser::new(&toks);
    let res = p.parse_module();
    assert!(res.is_err(), "expected error, got OK");
    let all = res.err().map(join_diags).unwrap();
    assert!(all.contains("assignment not allowed in expression"), "diagnostics:\n{}", all);
}

#[test]
fn assignment_in_condition_is_forbidden() {
    let toks = lex_ok("if (x = 1) { 0 }\n");
    let p = Parser::new(&toks);
    let res = p.parse_module();
    assert!(res.is_err(), "expected error, got OK");
    let all = res.err().map(join_diags).unwrap();
    assert!(all.contains("assignment not allowed in expression"), "diagnostics:\n{}", all);
}
