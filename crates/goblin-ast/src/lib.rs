//! Abstract Syntax Tree (AST) for Goblin.

use goblin_diagnostics::Span;

#[derive(Debug, Clone)]
pub struct Module {
    pub items: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    Class(ClassDecl),
    Action(ActionDecl),
}

#[derive(Debug, Clone)]
pub struct ClassDecl {
    pub name: String,
    pub fields: Vec<FieldDecl>,
    pub actions: Vec<ActionDecl>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FieldDecl {
    pub name: String,
    pub private: bool, // true if '#field'
    pub default: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ActionDecl {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Stmt>,  // a list of statements
    pub is_single: bool,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub type_name: Option<String>,   // e.g. "int", "money" (optional for now)
    pub default: Option<Expr>,       // support `param = expr`
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ActionBody {
    Expr(Expr),          // `act foo() = expr`
    Block(Vec<Stmt>),    // `act foo() ... end`  (we’ll fill later)
}

#[derive(Debug, Clone)]
pub enum Expr {
    // A literal or composite value forms
    Array(Vec<Expr>, Span),
    Bool(bool, Span),
    Float(String, Span),
    FloatWithUnit(String, String, Span),
    Ident(String, Span),
    Int(String, Span),
    IntWithUnit(String, String, Span),
    Nil(Span),
    Number(String, Span),                // <-- add this back
    Object(Vec<(String, Expr)>, Span),
    Str(String, Span),

    // Unary / binary / assignment
    Assign(Box<Expr>, Box<Expr>, Span),
    Binary(Box<Expr>, String, Box<Expr>, Span),
    Postfix(Box<Expr>, String, Span),
    Prefix(String, Box<Expr>, Span),

    // Indexing and member access
    Index(Box<Expr>, Box<Expr>, Span),
    Member(Box<Expr>, String, Span),
    OptMember(Box<Expr>, String, Span),

    // Calls
    Call(Box<Expr>, String, Vec<Expr>, Span),
    FreeCall(String, Vec<Expr>, Span),
    NsCall(String, String, Vec<Expr>, Span),
    OptCall(Box<Expr>, String, Vec<Expr>, Span),
}
