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
    pub class_name: Option<String>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    Class(ClassDecl),
    Action(ActionDecl),
    Bind(BindStmt),
    Enum(EnumDecl),
    Import(ImportStmt),
    Judge(JudgeStmt),
    JudgeAll(JudgeAllStmt),
    Return(ReturnStmt),
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
    pub private: bool,
    pub nullable: bool,      // true if field has `?` suffix
    pub readonly: bool,      // true if field has `!` prefix
    pub relation: Option<RelationDef>,
    pub default: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum RelationDef {  // Changed from 'enum' to 'pub enum'
    Of { class_name: String, as_name: String },
    With { class_name: String },
    Re { class_name: String },
}

#[derive(Debug, Clone)]
pub struct ImportStmt {
    pub items: ImportItems,
    pub alias: Option<String>,  // Only used for single-path imports
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ImportItems {
    /// Single path: import game/hero
    Path(String),
    /// Multiple items from source: import { hero, Combat } from game
    Named {
        items: Vec<ImportItem>,
        source: String,
    },
}

#[derive(Debug, Clone)]
pub struct ImportItem {
    pub name: String,
    pub alias: Option<String>,  // For: import { hero as h } from game
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
    Expr(Expr),
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
pub struct ReturnStmt {
    pub values: Vec<Expr>, // expressions; empty = bare return
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum JudgeArmBody {
    Expr(Expr),
    Stmts(Vec<Stmt>),
}

/// One arm of a **statement-form** judge.
/// `condition = None` means this is the implicit `else` arm.
#[derive(Debug, Clone)]
pub struct JudgeArmStmt {
    pub condition: Option<Box<Expr>>, // None for `else`
    pub body: JudgeArmBody,
    pub span: Span,
}

/// Statement-form judge node. Independent from the expression form (`Expr::Judge`).
#[derive(Debug, Clone)]
pub struct JudgeStmt {
    pub arms: Vec<JudgeArmStmt>,          // one or more arms
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct JudgeAllStmt {
    pub arms: Vec<JudgeArmStmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumDecl {
    pub name: String,
    pub variants: Vec<EnumVariant>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: String,
    pub fields: Option<Vec<FieldDecl>>,  // Reuse your existing FieldDecl
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct JudgeArm {
    pub condition: Option<Box<Expr>>,  // None for else
    pub value: Box<Expr>,
    pub span: Span,
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
    TupleAssign(Vec<String>, Box<Expr>, Span),

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

    // Other
    EnumVariant {
        enum_name: String,
        variant_name: String,
        fields: Option<Vec<(String, Expr)>>,  // field name -> value
        span: Span,
    },
    Judge {
        using: Option<Box<Expr>>,
        arms: Vec<JudgeArm>,
        all: bool,
        span: Span,
    },
    Block {
        stmts: Vec<Stmt>,  
        span: Span,
    }
}

impl Expr {
    pub fn span(&self) -> &Span {
        match self {
            Expr::Nil(sp) => sp,
            Expr::Bool(_, sp) => sp,
            Expr::Number(_, sp) => sp,
            Expr::Str(_, sp) => sp,
            Expr::Char(_, sp) => sp,
            Expr::Ident(_, sp) => sp,
            Expr::Slice(_, _, _, sp) => sp,
            Expr::Slice3(_, _, _, _, sp) => sp,
            Expr::TupleAssign(_, _, sp) => sp,
            Expr::Array(_, sp) => sp,
            Expr::Object(_, sp) => sp,
            Expr::Member(_, _, sp) => sp,
            Expr::OptMember(_, _, sp) => sp,
            Expr::Index(_, _, sp) => sp,
            Expr::Call(_, _, _, sp) => sp,
            Expr::OptCall(_, _, _, sp) => sp,
            Expr::FreeCall(_, _, sp) => sp,
            Expr::NsCall(_, _, _, sp) => sp,
            Expr::Prefix(_, _, sp) => sp,
            Expr::Postfix(_, _, sp) => sp,  
            Expr::Binary(_, _, _, sp) => sp,
            Expr::Assign(_, _, sp) => sp,
            Expr::EnumVariant { span, .. } => span,
            Expr::Judge { span, .. } => span,
            Expr::Block { span, .. } => span,  
        }
    }
}