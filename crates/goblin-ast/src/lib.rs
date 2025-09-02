//! Abstract Syntax Tree (AST) for Goblin.

use goblin_diagnostics::Span;

#[derive(Debug, Clone)]
pub struct Module {
    pub items: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    /// An identifier with its source span.
    Ident(String, Span),
    /// A numeric literal (int/float/money) with its source span.
    Number(String, Span),
}

