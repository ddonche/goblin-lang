//! Abstract Syntax Tree (AST) for Goblin — aligned to the current parser.
use goblin_diagnostics::Span;

pub type Ident = (String, Span);

#[derive(Debug, Clone)]
pub struct Module {
    pub items: Vec<Stmt>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindMode {
    Normal, // '='  (smart declare/mutate; shadowing with '=' is forbidden)
    Shadow, // '[=' (shadow operator: always birth a new local in this scope)
}

#[derive(Debug, Clone)]
pub struct BindStmt {
    pub name: Ident,
    pub expr: Expr,
    pub is_const: bool,   // true if preceded by 'imm'
    pub mode: BindMode,   // Normal or Shadow
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    Class(ClassDecl),
    Action(ActionDecl),
    Bind(BindStmt),
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
    pub private: bool,       // parser currently sets false; hook for '#field' later
    pub default: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub type_name: Option<String>,
    pub default: Option<Expr>, // AST Expr (not PExpr)
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ActionBody {
    Block(Vec<Stmt>),
}

#[derive(Debug, Clone)]
pub struct ActionDecl {
    pub name: String,
    pub params: Vec<Param>,    // <-- single field; no PExpr here
    pub body: ActionBody,
    pub span: Span,
    pub ret: Option<String>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    // Literals & identifiers
    Nil(Span),
    Bool(bool, Span),
    Number(String, Span),    // raw text as produced by lexer/parser (e.g., "10", "3.14")
    Str(String, Span),
    Char(char, Span),
    Ident(String, Span),
    Slice(Box<Expr>, Option<Box<Expr>>, Option<Box<Expr>>, Span),
    Slice3(Box<Expr>, Option<Box<Expr>>, Option<Box<Expr>>, Option<Box<Expr>>, Span),

    // Collections & objects
    Array(Vec<Expr>, Span),
    Object(Vec<(String, Expr)>, Span),

    // Property & indexing
    Member(Box<Expr>, String, Span),      // obj.name
    OptMember(Box<Expr>, String, Span),   // obj?.name
    Index(Box<Expr>, Box<Expr>, Span),    // obj[idx]

    // Calls
    Call(Box<Expr>, String, Vec<Expr>, Span),    // recv.name(args)
    OptCall(Box<Expr>, String, Vec<Expr>, Span), // recv?.name(args)
    FreeCall(String, Vec<Expr>, Span),           // name(args)
    NsCall(String, String, Vec<Expr>, Span),     // Ns::name(args)

    // Operators
    Prefix(String, Box<Expr>, Span),
    Postfix(Box<Expr>, String, Span),
    Binary(Box<Expr>, String, Box<Expr>, Span),
    Assign(Box<Expr>, Box<Expr>, Span),
}
