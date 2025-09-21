//! Abstract Syntax Tree (AST) for Goblin — aligned to the current parser.
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
    pub private: bool,       // parser currently sets false; hook for '#field' later
    pub default: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub type_name: Option<String>,   // e.g. `amount | Number`
    pub default: Option<Expr>,       // default arg value if provided
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ActionBody {
    Block(Vec<Stmt>),
    // Native(Vec<String>) // (not used by parser right now, placeholder if you add later)
}

#[derive(Debug, Clone)]
pub struct ActionDecl {
    pub name: String,
    pub params: Vec<Param>,
    pub body: ActionBody,
    pub span: Span,
    pub ret: Option<String>, // parser always sets None for now; wire a real Type later
}

#[derive(Debug, Clone)]
pub enum Expr {
    // Literals & identifiers
    Nil(Span),
    Bool(bool, Span),
    Number(String, Span),    // raw text as produced by lexer/parser (e.g., "10", "3.14")
    Str(String, Span),
    Ident(String, Span),

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
