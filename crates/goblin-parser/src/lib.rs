use goblin_ast as ast;
use goblin_diagnostics::{Diagnostic, Span};
use goblin_lexer::{Token, TokenKind};

#[derive(Debug, Clone)]
enum PExpr {
    Array(Vec<PExpr>),
    Assign(Box<PExpr>, Box<PExpr>),
    Binary(Box<PExpr>, String, Box<PExpr>),
    Bool(bool),
    Call(Box<PExpr>, String, Vec<PExpr>),
    Float(String),
    FloatWithUnit(String, String),
    FreeCall(String, Vec<PExpr>),
    Ident(String),
    Index(Box<PExpr>, Box<PExpr>),
    Int(String),
    IntWithUnit(String, String),
    IsBound(Box<PExpr>),
    Member(Box<PExpr>, String),
    Nil,
    NsCall(String, String, Vec<PExpr>),
    Object(Vec<(String, PExpr)>),
    OptCall(Box<PExpr>, String, Vec<PExpr>),
    OptMember(Box<PExpr>, String),
    Postfix(Box<PExpr>, String),
    Prefix(String, Box<PExpr>),
    Slice(Box<PExpr>, Option<Box<PExpr>>, Option<Box<PExpr>>),
    Slice3(
        Box<PExpr>,
        Option<Box<PExpr>>,
        Option<Box<PExpr>>,
        Option<Box<PExpr>>,
    ),
    Str(String),  

    ClassDecl {
        name: String,
        fields: Vec<(String, PExpr)>,
        actions: Vec<PAction>,
    },
}

#[derive(Debug, Clone)]
struct PAction {
    name: String,
    params: Vec<String>,
    body: Vec<PExpr>,
    is_single: bool,
}


#[derive(Debug, Clone)]
enum PDecl {
    Expr(PExpr),
    Action(PAction),
    Class {
        name: String,
        fields: Vec<(String, PExpr)>,
        actions: Vec<PAction>,
    },
}

pub type ParseResult<T> = Result<T, Vec<Diagnostic>>;

pub struct Parser<'t> {
    toks: &'t [Token],
    i: usize,
    in_stmt: bool,
}

fn is_block_starter_name(name: &str) -> bool {
    matches!(name, "if" | "for" | "while" | "repeat" | "judge")
}

impl<'t> Parser<'t> {
    pub fn new(toks: &'t [Token]) -> Self {
        Self { toks, i: 0, in_stmt: false }
    }

    

    fn span_from_tokens(toks: &[goblin_lexer::Token], start_i: usize, end_i: usize) -> Span {
        if toks.is_empty() {
            return Span::new("<empty>", 0, 0, 0, 0, 0, 0);
        }
        let start_tok = toks.get(start_i.min(toks.len() - 1));
        let end_tok = toks.get(end_i.min(toks.len() - 1));
        
        match (start_tok, end_tok) {
            (Some(s), Some(e)) => Span::new(
                s.span.file.clone(),
                s.span.start,
                e.span.end,
                s.span.line_start,
                e.span.line_end,
                s.span.col_start,
                e.span.col_end,
            ),
            (Some(s), None) => s.span.clone(),
            _ => Span::new("<unknown>", 0, 0, 0, 0, 0, 0),
        }
    }

    fn lower_expr_preview(pe: PExpr, sp: Span) -> ast::Expr {
        match pe {

            // Basic literals and identifiers
            PExpr::Ident(name) => ast::Expr::Ident(name, sp),
            PExpr::Int(s)
            | PExpr::Float(s)
            | PExpr::IntWithUnit(s, _)
            | PExpr::FloatWithUnit(s, _) => ast::Expr::Number(s, sp),
            PExpr::Bool(b) => ast::Expr::Bool(b, sp),
            PExpr::Str(s) => ast::Expr::Str(s, sp),
            PExpr::Nil => ast::Expr::Nil(sp),

            // Collections
            PExpr::Array(items) => {
                let elems = items
                    .into_iter()
                    .map(|e| Self::lower_expr_preview(e, sp.clone()))
                    .collect();
                ast::Expr::Array(elems, sp)
            }
           PExpr::Object(fields) => {
                let fields = fields
                    .into_iter()
                    .map(|(k, v)| (k, Self::lower_expr_preview(v, sp.clone())))
                    .collect();
                ast::Expr::Object(fields, sp)
            }

            // Calls
            PExpr::Call(expr, name, args) => {
                let recv = Box::new(Self::lower_expr_preview(*expr, sp.clone()));
                let args = args
                    .into_iter()
                    .map(|a| Self::lower_expr_preview(a, sp.clone()))
                    .collect();
                ast::Expr::Call(recv, name, args, sp)
            }
            PExpr::FreeCall(name, args) => {
                let args = args
                    .into_iter()
                    .map(|a| Self::lower_expr_preview(a, sp.clone()))
                    .collect();
                ast::Expr::FreeCall(name, args, sp)
            }
            PExpr::NsCall(ns, name, args) => {
                let args = args
                    .into_iter()
                    .map(|a| Self::lower_expr_preview(a, sp.clone()))
                    .collect();
                ast::Expr::NsCall(ns, name, args, sp)
            }
            PExpr::OptCall(expr, name, args) => {
                let recv = Box::new(Self::lower_expr_preview(*expr, sp.clone()));
                let args = args
                    .into_iter()
                    .map(|a| Self::lower_expr_preview(a, sp.clone()))
                    .collect();
                ast::Expr::OptCall(recv, name, args, sp)
            }

            // Member and indexing
            PExpr::Member(expr, name) => {
                let obj = Box::new(Self::lower_expr_preview(*expr, sp.clone()));
                ast::Expr::Member(obj, name, sp)
            }
            PExpr::OptMember(expr, name) => {
                let obj = Box::new(Self::lower_expr_preview(*expr, sp.clone()));
                ast::Expr::OptMember(obj, name, sp)
            }
            PExpr::Index(expr, idx) => {
                let obj = Box::new(Self::lower_expr_preview(*expr, sp.clone()));
                let idx = Box::new(Self::lower_expr_preview(*idx, sp.clone()));
                ast::Expr::Index(obj, idx, sp)
            }

            // Unary / postfix / binary / assignment
            PExpr::Prefix(op, expr) => {
                let expr = Box::new(Self::lower_expr_preview(*expr, sp.clone()));
                ast::Expr::Prefix(op, expr, sp)
            }
            PExpr::Postfix(expr, op) => {
                let expr = Box::new(Self::lower_expr_preview(*expr, sp.clone()));
                ast::Expr::Postfix(expr, op, sp)
            }
            PExpr::Binary(lhs, op, rhs) => {
                let lhs = Box::new(Self::lower_expr_preview(*lhs, sp.clone()));
                let rhs = Box::new(Self::lower_expr_preview(*rhs, sp.clone()));
                ast::Expr::Binary(lhs, op, rhs, sp)
            }
            PExpr::Assign(lhs, rhs) => {
                let lhs = Box::new(Self::lower_expr_preview(*lhs, sp.clone()));
                let rhs = Box::new(Self::lower_expr_preview(*rhs, sp.clone()));
                ast::Expr::Assign(lhs, rhs, sp)
            }

            // Fallback for unimplemented constructs
            other => {
                let txt = format!("{:?}", other);
                ast::Expr::Ident(txt, sp)
            }
        }
    }

    fn lower_expr(&mut self, pe: PExpr) -> ast::Expr {
        let sp = Self::span_from_tokens(self.toks, self.i.saturating_sub(1), self.i);
        Self::lower_expr_preview(pe, sp)
    }

    fn try_parse_class_decl(&mut self) -> Option<Result<PExpr, String>> {
        if !self.peek_op("@") {
            return None;
        }
        
        let save = self.i;
        if self.eat_op("@") {
            if let Some(_) = self.peek_ident() {
                self.i = save;
                Some(self.parse_class_decl())
            } else {
                self.i = save;
                None
            }
        } else {
            None
        }
    }

    fn skip_action_block(&mut self) -> Result<(), String> {
        use goblin_lexer::TokenKind;

        let mut depth = 1;
        loop {
            if self.is_eof() {
                return Err("unexpected end of input while parsing action block".to_string());
            }
            let tok = self.peek().unwrap();

            // Close on 'end' or '}'.
            match &tok.kind {
                TokenKind::Ident if tok.value.as_deref() == Some("end") => {
                    self.i += 1;
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                    continue;
                }
                TokenKind::Op(op) if op == "}" => {
                    self.i += 1;
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                    continue;
                }
                _ => {}
            }

            // Nest when we see another block-starter keyword.
            if let Some(s) = self.peek_ident() {
                if is_block_starter_name(s) {
                    self.eat_ident();
                    depth += 1;
                    continue;
                }
            }

            // Otherwise, advance.
            self.i += 1;
        }
        Ok(())
    }

    fn parse_field_chain_line(&mut self) -> Result<Vec<(String, PExpr)>, String> {
        let mut out = Vec::new();

        loop {
            let Some(key) = self.eat_object_key() else {
                if out.is_empty() {
                    return Err("expected field name in class header".to_string());
                } else {
                    break;
                }
            };

            if !self.eat_op(":") {
                return Err("expected ':' after field name".to_string());
            }

            let val = self.parse_coalesce()?;
            out.push((key, val));

            if self.eat_op("::") {
                if self.peek_newline_or_eof() {
                    break;
                }
                continue;
            }

            break;
        }

        Ok(out)
    }

    fn parse_action_after_keyword(&mut self, kw: &str) -> Result<PAction, String> {
        use goblin_lexer::TokenKind;

        let Some(name) = self.eat_ident() else {
            return Err(format!("expected {} name", kw));
        };

        // optional params: (p1, p2, ...)
        let mut params = Vec::new();
        if self.eat_op("(") {
            if !self.peek_op(")") {
                loop {
                    let Some(p) = self.eat_ident() else {
                        return Err("expected parameter name".to_string());
                    };
                    params.push(p);
                    if self.eat_op(",") {
                        if self.peek_op(")") { break; }
                        continue;
                    }
                    break;
                }
            }
            if !self.eat_op(")") {
                return Err("expected ')' after parameter list".to_string());
            }
        }

        // single-line form: `act foo = expr`
        if self.eat_op("=") {
            let expr = self.parse_coalesce()?;
            return Ok(PAction {
                name,
                params,
                body: vec![expr],
                is_single: true,
            });
        }

        // IMPORTANT: do NOT parse a block body here.
        Ok(PAction {
            name,
            params,
            body: Vec::new(),
            is_single: false,
        })
    }

    fn parse_free_action(&mut self, kw: &str) -> Result<ast::Stmt, String> {
        let pa = self.parse_action_after_keyword(kw)?;

        // If header produced the single-line (= expr) body, just lower it.
        let body_stmts: Vec<ast::Stmt> = if !pa.body.is_empty() {
            pa.body.into_iter().map(|pe| ast::Stmt::Expr(self.lower_expr(pe))).collect()
        } else {
            // Parse a *statement block* right here.
            // Do NOT call expect_block_start(); parse_stmt_block() handles both:
            //   - same-line '{ ... }'
            //   - layout (newline + indent ... 'end' or '}')
            self.parse_stmt_block()?
        };

        let body = ast::ActionBody::Block(body_stmts);

        let params: Vec<ast::Param> = pa.params.into_iter().map(|pname| {
            let sp = Self::span_from_tokens(self.toks, self.i.saturating_sub(1), self.i);
            ast::Param { name: pname, type_name: None, default: None, span: sp }
        }).collect();

        let act = ast::ActionDecl {
            name: pa.name,
            params,
            body,
            span: Self::span_from_tokens(self.toks, self.i.saturating_sub(1), self.i),
            ret: None,
        };

        Ok(ast::Stmt::Action(act))
    }

    #[inline]
    fn peek(&self) -> Option<&Token> { self.toks.get(self.i) }

    #[inline]
    fn peek_op(&self, s: &str) -> bool {
        if let Some(t) = self.peek() {
            if let goblin_lexer::TokenKind::Op(ref op) = t.kind {
                return op == s;
            }
        }
        false
    }

    #[inline]
    fn eat_op(&mut self, s: &str) -> bool {
        if self.peek_op(s) {
            self.i += 1;
            true
        } else {
            false
        }
    }

    #[inline]
    fn peek_ident(&self) -> Option<&str> {
        self.peek().and_then(|t| {
            if let goblin_lexer::TokenKind::Ident = t.kind {
                t.value.as_deref()
            } else {
                None
            }
        })
    }

    #[inline]
    fn eat_ident(&mut self) -> Option<String> {
        let val = {
            if let Some(t) = self.peek() {
                if let goblin_lexer::TokenKind::Ident = t.kind {
                    t.value.clone()
                } else {
                    None
                }
            } else {
                None
            }
        };
        if val.is_some() {
            self.i += 1;
        }
        val
    }

    // Consume consecutive NEWLINE tokens.
    fn skip_newlines(&mut self) {
        use goblin_lexer::TokenKind;
        while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, TokenKind::Newline)) {
            self.i += 1;
        }
    }

    // Do we see a block closer at the current position?  (either '}' or the ident 'end')
    fn peek_block_close(&self) -> bool {
        use goblin_lexer::TokenKind;

        if let Some(tok) = self.toks.get(self.i) {
            match &tok.kind {
                TokenKind::Op(op) if op == "}" => true,
                // `Ident` is a unit variant; the text is in `tok.value`
                TokenKind::Ident => tok.value.as_deref() == Some("end"),
                _ => false,
            }
        } else {
            false
        }
    }

    // If present, consume a block closer. Returns true if one was consumed.
    fn eat_block_close(&mut self) -> bool {
        if self.eat_op("}") {
            return true;
        }
        if let Some("end") = self.peek_ident() {
            let _ = self.eat_ident();
            return true;
        }
        false
    }

    // Require a block closer right here (after optional newlines). Helpful error if missing.
    fn expect_block_close(&mut self, ctx: &'static str) -> Result<(), String> {
        self.skip_newlines();
        if self.eat_block_close() {
            Ok(())
        } else {
            Err(format!("expected '}}' or 'end' to close {}", ctx))
        }
    }

    // Optional: also treat semicolons as statement separators when scanning blocks.
    fn skip_stmt_separators(&mut self) {
        use goblin_lexer::TokenKind;
        loop {
            match self.toks.get(self.i) {
                Some(tok) if matches!(tok.kind, TokenKind::Newline) => { self.i += 1; }
                Some(tok) if matches!(tok.kind, TokenKind::Op(ref s) if s == ";") => { self.i += 1; }
                _ => break,
            }
        }
    }

    // Generic "read statements until '}' or 'end'". Use this for bodies of fn/if/while/class/etc.
    // It preserves your existing statement parser and newline tolerance.
    fn parse_stmt_block(&mut self) -> Result<Vec<ast::Stmt>, String> {
        use goblin_lexer::TokenKind;

        // BRACE MODE: `{ ... }`
        if self.eat_op("{") {
            let mut out = Vec::new();
            loop {
                self.skip_stmt_separators();
                if self.peek_block_close() {
                    let _ = self.eat_block_close(); // guaranteed true
                    break;
                }
                if self.is_eof() {
                    return Err("unexpected end of file: expected '}' or 'end' to close block".into());
                }
                out.push(self.parse_stmt()?);
            }
            return Ok(out);
        }

        // INDENT MODE: require newline, optionally followed by Indent
        let mut saw_nl = false;
        while let Some(tok) = self.toks.get(self.i) {
            if matches!(tok.kind, TokenKind::Newline) {
                self.i += 1;
                saw_nl = true;
            } else {
                break;
            }
        }

        // Fallback: accept physical line break if lexer didn’t emit Newline
        if !saw_nl {
            if let (Some(prev), Some(curr)) = (self.toks.get(self.i.saturating_sub(1)), self.toks.get(self.i)) {
                if curr.span.line_start > prev.span.line_end {
                    saw_nl = true;
                }
            }
        }

        // Optionally consume a single Indent
        let mut depth: usize = 0;
        if let Some(tok) = self.toks.get(self.i) {
            if matches!(tok.kind, TokenKind::Indent) {
                self.i += 1;
                depth = 1;
            }
        }

        if !saw_nl {
            return Err("expected newline to start block".to_string());
        }

        let mut out = Vec::new();
        loop {
            match self.toks.get(self.i) {
                Some(tok) if matches!(tok.kind, TokenKind::Indent) => {
                    self.i += 1;
                    depth += 1;
                    continue;
                }
                Some(tok) if matches!(tok.kind, TokenKind::Dedent) => {
                    self.i += 1;
                    if depth == 0 {
                        return Err("dedent below block start".into());
                    }
                    depth -= 1;
                    if depth == 0 {
                        // Require an explicit closer after layout ends
                        self.skip_newlines();
                        if self.peek_block_close() {
                            let _ = self.eat_block_close();
                            break;
                        }
                        return Err("missing 'end' (or '}') to close block".into());
                    }
                    continue;
                }
                Some(_) => { /* normal statement path */ }
                None => {
                    return Err("unexpected end of file: expected '}' or 'end' to close block".into());
                }
            }

            // Also allow explicit closers
            if depth == 0 && self.peek_block_close() {
                let _ = self.eat_block_close();
                break;
            }

            // Parse one statement in the block
            out.push(self.parse_stmt()?);
            self.skip_stmt_separators();
        }

        Ok(out)
    }

    fn peek_newline_or_eof(&self) -> bool {
        use goblin_lexer::TokenKind;
        match self.toks.get(self.i) {
            Some(tok) => matches!(tok.kind, TokenKind::Newline | TokenKind::Eof),
            None => true,
        }
    }

    fn peek_ident_at_col(&self, kw: &str, col: u32) -> bool {
        use goblin_lexer::TokenKind;
        if let Some(tok) = self.toks.get(self.i) {
            match tok.kind {
                TokenKind::Ident => tok.value.as_deref() == Some(kw) && tok.span.col_start == col,
                _ => false,
            }
        } else {
            false
        }
    }

    /// After a header (e.g., `if <expr>`), accept either `{` or a newline-led block.
    /// Be tolerant if a caller already consumed the `Newline` (and/or `Indent`) tokens.
    /// We also use spans to detect a line break when tokens were eaten upstream.
    fn expect_block_start(&mut self) -> Result<(), String> {
        use goblin_lexer::TokenKind;

        // Style 1: explicit braced body: `{`
        if self.eat_op("{") {
            return Ok(());
        }

        // Style 2: explicit newline, then (optionally) an Indent token
        if let Some(tok) = self.peek() {
            if matches!(tok.kind, TokenKind::Newline) {
                // consume the newline
                self.i += 1;
                // consume exactly one optional Indent token if present
                if let Some(t2) = self.peek() {
                    if matches!(t2.kind, TokenKind::Indent) {
                        self.i += 1;
                    }
                }
                return Ok(());
            }
        }

        // Style 3: physical line break (span-based), for cases where lexer didn’t emit Newline here
        let prev = self.toks.get(self.i.saturating_sub(1));
        let curr = self.toks.get(self.i);
        if let (Some(p), Some(c)) = (prev, curr) {
            if c.span.line_start > p.span.line_end {
                // accept (and consume) a single Indent token if it happens to be here
                if let Some(t2) = self.peek() {
                    if matches!(t2.kind, TokenKind::Indent) {
                        self.i += 1;
                    }
                }
                return Ok(());
            }
        }

        Err("expected newline to start block".to_string())
    }

    fn parse_kv_bind_list(&mut self) -> Result<Vec<(String, PExpr)>, String> {
        let mut fields = Vec::new();
        loop {
            self.skip_newlines();

            let Some(key) = self.eat_object_key() else {
                break;
            };

            if !self.eat_op(":") {
                return Err("expected ':' after field name in class header".to_string());
            }

            let value = self.parse_coalesce()?;
            fields.push((key, value));

            self.skip_newlines();
            if self.eat_op("::") {
                continue;
            }
            break;
        }
        Ok(fields)
    }

    fn parse_class_decl(&mut self) -> Result<PExpr, String> {
        use goblin_lexer::TokenKind;

        if !self.eat_op("@") {
            return Err("expected '@' to start class declaration".to_string());
        }

        let Some(name) = self.eat_ident() else {
            return Err("expected class name after '@'".to_string());
        };

        let name_tok_idx = self.i - 1;
        if !name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
            self.i = name_tok_idx;
            return Err(format!("class name '{}' must start with an uppercase letter", name));
        }

        if !self.eat_op("=") {
            return Err(format!("expected '=' after class name '{}'", name));
        }

        let fields = self.parse_field_chain_line()?;
        self.eat_semi_separators();

        let mut actions = Vec::new();

        loop {
            self.eat_semi_separators();

            // Close class body with either `end` or `}`
            if self.is_eof() {
                return Err(format!("unterminated class '{}': missing '}}' or 'end'", name));
            }
            let tok = self.peek().unwrap();
            let is_end = matches!(tok.kind, TokenKind::Ident) && tok.value.as_deref() == Some("end");
            let is_rbrace = matches!(tok.kind, TokenKind::Op(ref s) if s == "}");
            if is_end || is_rbrace {
                self.i += 1;
                break;
            }

            // Expect 'act' or 'action'
            let kw = match self.toks.get(self.i).map(|t| &t.kind) {
                Some(TokenKind::Act) => {
                    self.i += 1;
                    "act"
                }
                Some(TokenKind::Action) => {
                    self.i += 1;
                    "action"
                }
                _ => {
                    return Err("expected 'act' or 'action' or 'end' in class body".to_string());
                }
            };

            let action = self.parse_action_after_keyword(kw)?;
            actions.push(action);
        }

        Ok(PExpr::ClassDecl {
            name,
            fields,
            actions,
        })
    }

    fn parse_decl(&mut self) -> Result<PDecl, String> {
        if let Some(t) = self.peek() {
            let kind = t.kind.clone();
            let val = t.value.clone();
            match kind {
                TokenKind::Op(op) if op == "@" => {
                    let class = self.parse_class_decl()?;
                    if let PExpr::ClassDecl {
                        name,
                        fields,
                        actions,
                    } = class
                    {
                        return Ok(PDecl::Class {
                            name,
                            fields,
                            actions,
                        });
                    }
                }
                TokenKind::Ident => {
                    let kw = val.as_deref().unwrap_or("");
                    match kw {
                        "act" | "action" => {
                            let _ = self.eat_ident();
                            let act = self.parse_action_after_keyword(kw)?;
                            return Ok(PDecl::Action(act));
                        }
                        "class" => {
                            let _ = self.eat_ident();
                            let class = self.parse_class_decl_keyword()?;
                            if let PExpr::ClassDecl {
                                name,
                                fields,
                                actions,
                            } = class
                            {
                                return Ok(PDecl::Class {
                                    name,
                                    fields,
                                    actions,
                                });
                            }
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        }

        let expr = self.parse_assign()?;
        Ok(PDecl::Expr(expr))
    }

    fn parse_class_decl_keyword(&mut self) -> Result<PExpr, String> {
        use goblin_lexer::TokenKind;

        let Some(name) = self.eat_ident() else {
            return Err("expected class name after 'class'".to_string());
        };

        if !self.eat_op("=") {
            return Err(format!("expected '=' after class name '{}'", name));
        }

        let fields = self.parse_field_chain_line()?;
        self.eat_semi_separators();

        let mut actions = Vec::new();

        loop {
            self.eat_semi_separators();

            if self.is_eof() {
                return Err(format!(
                    "unterminated class '{}': missing '}}' or 'end'",
                    name
                ));
            }
            let tok = self.peek().unwrap();
            let is_end =
                matches!(tok.kind, TokenKind::Ident) && tok.value.as_deref() == Some("end");
            let is_rbrace = matches!(tok.kind, TokenKind::Op(ref s) if s == "}");
            if is_end || is_rbrace {
                self.i += 1;
                break;
            }

            let Some(kw) = self.eat_ident() else {
                return Err("expected 'act' or 'action' or 'end' in class body".to_string());
            };
            if kw != "act" && kw != "action" {
                return Err(format!(
                    "expected 'act' or 'action' or 'end', found '{}'",
                    kw
                ));
            }

            let action = self.parse_action_after_keyword(&kw)?;
            actions.push(action);
        }

        Ok(PExpr::ClassDecl {
            name,
            fields,
            actions,
        })
    }

    fn eat_object_key(&mut self) -> Option<String> {
        if let Some(t) = self.peek() {
            match t.kind {
                goblin_lexer::TokenKind::Ident | goblin_lexer::TokenKind::String => {
                    let val = t.value.clone().unwrap_or_default();
                    self.i += 1;
                    Some(val)
                }
                _ => None,
            }
        } else {
            None
        }
    }

    #[inline]
    fn eat_string_lit(&mut self) -> Option<String> {
        let val = {
            if let Some(t) = self.peek() {
                if let goblin_lexer::TokenKind::String = t.kind {
                    t.value.clone()
                } else {
                    None
                }
            } else {
                None
            }
        };
        if val.is_some() {
            self.i += 1;
        }
        val
    }

    #[inline]
    fn bump(&mut self) -> Option<&Token> {
        let idx = self.i;
        self.i += 1;
        self.toks.get(idx)
    }

    #[inline]
    fn is_eof(&self) -> bool {
        self.i >= self.toks.len() || matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Eof))
    }

    fn eat_layout(&mut self) {
        while let Some(t) = self.peek() {
            match t.kind {
                TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent => self.i += 1,
                _ => break,
            }
        }
    }

    #[inline]
    fn is_expr_start(kind: &TokenKind) -> bool {
        use goblin_lexer::TokenKind;

        match kind {
            // common expr/stmt starters
            TokenKind::Ident
            | TokenKind::Int
            | TokenKind::Float
            | TokenKind::Money
            | TokenKind::String => true,

            // grouping / literals that can begin expr-stmts
            TokenKind::Op(op) if op == "(" || op == "[" || op == "{" || op == "@" => true,

            // keyword tokens that begin statements
            TokenKind::Act | TokenKind::Action => true,

            _ => false,
        }
    }

    fn parse_if_stmt(&mut self) -> Result<ast::Stmt, String> {
        // We are at 'if <cond> <newline or {>'
        let if_tok_i = self.i; // index of 'if'
        let if_line  = self.toks[if_tok_i].span.line_start;
        let if_col   = self.toks[if_tok_i].span.col_start;

        let _ = self.eat_ident();        // 'if'
        let _cond = self.parse_assign()?; // parse condition

        // Friendly diagnostic: next-line '{' is object literal, not a block
        {
            let save = self.i;
            // Look ahead (don't consume anything that matters to block reader)
            while let Some(tok) = self.toks.get(self.i) {
                use goblin_lexer::TokenKind;
                match tok.kind {
                    TokenKind::Newline => { self.i += 1; continue; }
                    TokenKind::Op(ref s) if s == ";" => { self.i += 1; continue; }
                    _ => break,
                }
            }
            if let Some(t) = self.peek() {
                if let goblin_lexer::TokenKind::Op(op) = &t.kind {
                    if op == "{" && t.span.line_start > if_line {
                        self.i = save;
                        return Err("brace '{' on a new line starts an object literal.\n\
                                    For an 'if' block, either use layout-indent and close with 'end' or '}', \
                                    or put '{' on the same line as 'if <cond>'.".to_string());
                    }
                }
            }
            self.i = save;
        }

        // THEN body — hand control straight to the block reader
        let _then_body = self.parse_stmt_block_until(|p| {
            p.peek_ident_at_col("elif", if_col) || p.peek_ident_at_col("else", if_col)
        })?;

        // zero or more ELIF blocks
        let mut elif_count = 0usize;
        loop {
            self.eat_semi_separators();
            if self.peek_ident_at_col("elif", if_col) {
                let _ = self.eat_ident();       // 'elif'
                let _elif_cond = self.parse_assign()?;
                let _elif_body = self.parse_stmt_block_until(|p| {
                    p.peek_ident_at_col("elif", if_col) || p.peek_ident_at_col("else", if_col)
                })?;
                elif_count += 1;
                continue;
            }
            break;
        }

        // optional ELSE block
        let mut has_else = false;
        self.eat_semi_separators();
        if self.peek_ident_at_col("else", if_col) {
            let _ = self.eat_ident(); // 'else'
            let _else_body = self.parse_stmt_block()?; // to 'end' / '}' / dedent
            has_else = true;
        }

        // Placeholder node so parse completes without dedicated If AST (your existing pattern)
        let sp  = Self::span_from_tokens(self.toks, if_tok_i, self.i.saturating_sub(1));
        let tag = if has_else {
            format!("if(+{} elif, else)", elif_count)
        } else {
            format!("if(+{} elif)", elif_count)
        };
        Ok(ast::Stmt::Expr(ast::Expr::Ident(tag, sp)))
    }

    fn parse_stmt_block_until<F>(&mut self, mut stop: F) -> Result<Vec<ast::Stmt>, String>
    where
        F: FnMut(&Parser<'_>) -> bool,
    {
        use goblin_lexer::TokenKind;

        // BRACE MODE: `{ ... }`
        if self.eat_op("{") {
            let mut out = Vec::new();
            loop {
                self.skip_stmt_separators();
                if self.peek_block_close() {
                    let _ = self.eat_block_close(); // guaranteed true
                    break;
                }
                if self.is_eof() {
                    return Err("unexpected end of file: expected '}' or 'end' to close block".into());
                }
                out.push(self.parse_stmt()?);
            }
            return Ok(out);
        }

        // INDENT MODE: require newline, optionally followed by Indent
        let mut saw_nl = false;
        while let Some(tok) = self.toks.get(self.i) {
            if matches!(tok.kind, TokenKind::Newline) {
                self.i += 1;
                saw_nl = true;
            } else {
                break;
            }
        }

        // Fallback: accept physical line break if lexer didn’t emit Newline
        if !saw_nl {
            if let (Some(prev), Some(curr)) = (self.toks.get(self.i.saturating_sub(1)), self.toks.get(self.i)) {
                if curr.span.line_start > prev.span.line_end {
                    saw_nl = true;
                }
            }
        }

        // Optionally consume a single Indent
        let mut depth: usize = 0;
        if let Some(tok) = self.toks.get(self.i) {
            if matches!(tok.kind, TokenKind::Indent) {
                self.i += 1;
                depth = 1;
            }
        }

        if !saw_nl {
            return Err("expected newline to start block".to_string());
        }

        let mut out = Vec::new();
        loop {
            // Always consume statement separators at block top
            self.skip_stmt_separators();

            // *** Key guard: in an open block, EOF => missing closer, not "expected expression"
            if self.is_eof() {
                return Err("missing 'end' (or '}') to close block".into());
            }

            // Layout changes
            if let Some(tok) = self.toks.get(self.i) {
                match tok.kind {
                    TokenKind::Indent => {
                        self.i += 1;
                        depth += 1;
                        continue;
                    }
                    TokenKind::Dedent => {
                        // consume all pending Dedents
                        while let Some(t2) = self.toks.get(self.i) {
                            if matches!(t2.kind, TokenKind::Dedent) {
                                self.i += 1;
                            } else {
                                break;
                            }
                        }
                        if depth == 0 {
                            return Err("dedent below block start".to_string());
                        }
                        depth -= 1;

                        // We just closed the innermost indentation level of this block.
                        // At depth==0 we *require* an explicit closer (`end` or `}`).
                        if depth == 0 {
                            self.skip_newlines();
                            if self.peek_block_close() {
                                let _ = self.eat_block_close();
                                break;
                            }
                            return Err("missing 'end' (or '}') to close block".into());
                        }
                        continue;
                    }
                    _ => { /* normal statement path */ }
                }
            }

            // Stop before aligned elif/else (or any caller-defined stopper)
            if depth == 0 && stop(self) {
                break;
            }

            // Also allow explicit closers
            if depth == 0 && self.peek_block_close() {
                let _ = self.eat_block_close();
                break;
            }

            // Parse one statement in the block
            out.push(self.parse_stmt()?);

            // After a statement, if we’re still inside the block and hit EOF, it’s a missing closer.
            self.skip_stmt_separators();
            if depth > 0 && self.is_eof() {
                return Err("missing 'end' (or '}') to close block".into());
            }
        }

        Ok(out)
    }

    pub fn parse_module(mut self) -> ParseResult<ast::Module> {
        let mut items = Vec::new();
        self.eat_layout();

        while !self.is_eof() {
            loop {
                match self.peek().map(|t| &t.kind) {
                    Some(k) if Self::is_expr_start(k) => break,
                    Some(TokenKind::Eof) | None => break,
                    Some(TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent) => {
                        self.i += 1;
                    }
                    _ => {
                        self.i += 1;
                    }
                }
            }

            if self.is_eof() { break; }
            
            match self.parse_stmt() {
                Ok(stmt) => items.push(stmt),
                Err(msg) => {
                    let sp = if let Some(tok) = self.peek() {
                        tok.span.clone()
                    } else {
                        Span::new("<eof>", 0, 0, 0, 0, 0, 0)
                    };
                    return Err(vec![Diagnostic::error("ParseError", &msg, sp)]);
                }
            }
            self.eat_layout();
        }

        Ok(ast::Module { items })
    }

    fn parse_stmt(&mut self) -> Result<ast::Stmt, String> {
        use goblin_lexer::TokenKind;

        // First: dedicated keyword tokens (some lexers emit these)
        if let Some(t) = self.peek() {
            match &t.kind {
                TokenKind::Act => { self.i += 1; return self.parse_free_action("act"); }
                TokenKind::Action => { self.i += 1; return self.parse_free_action("action"); }
                _ => {}
            }
        }

        // IF / ELIF / ELSE (layout-aware; placeholder AST until full lowering exists)
        if self.peek_ident() == Some("if") {
            // remember where the 'if' token sits (for column alignment)
            let if_tok_i = self.i;
            let if_col = self.toks[if_tok_i].span.col_start;

            let _ = self.eat_ident(); // 'if'

            // parse condition (expr-only: no assignment)
            let _cond = self.parse_coalesce()?;

            // parse the 'then' block, stopping before an aligned 'elif' or 'else'
            let _then = self.parse_stmt_block_until(|p| {
                p.peek_ident_at_col("elif", if_col) || p.peek_ident_at_col("else", if_col)
            })?;

            // zero or more aligned 'elif' blocks
            loop {
                self.skip_stmt_separators();
                if self.peek_ident_at_col("elif", if_col) {
                    let _ = self.eat_ident();        // 'elif'
                    let _c = self.parse_coalesce()?; // condition
                    let _b = self.parse_stmt_block_until(|p| {
                        p.peek_ident_at_col("elif", if_col) || p.peek_ident_at_col("else", if_col)
                    })?;
                    continue;
                }
                break;
            }

            // optional aligned 'else' block
            self.skip_stmt_separators();
            if self.peek_ident_at_col("else", if_col) {
                let _ = self.eat_ident(); // 'else'
                let _else = self.parse_stmt_block_until(|_| false)?;
            }

            // placeholder stmt that spans the whole if-construct
            let sp = Self::span_from_tokens(self.toks, if_tok_i, self.i);
            let expr = Self::lower_expr_preview(PExpr::Ident("if_block".into()), sp);
            return Ok(ast::Stmt::Expr(expr));
        }

        // free-arrow action: a >> foo : a, b, c
        if self.peek_ident() == Some("a") && self.peek_op(">>") {
            let _ = self.eat_ident(); // 'a'
            let _ = self.eat_op(">>"); // >>
            // name
            let name = self.eat_ident().ok_or("expected action name after 'a >> '")?;
            // parse arguments after ':'
            let params = if self.eat_op(":") {
                self.parse_args_colon()?
            } else {
                Vec::new() // No arguments if ':' is absent
            };
            return Ok(ast::Stmt::Expr(self.lower_expr(PExpr::FreeCall(name, params))));
        }

        if let Some(res) = self.try_parse_class_decl() {
            let p = res?;
            return Ok(ast::Stmt::Expr(self.lower_expr(p)));
        }

        // Fallback for when lexer emits Ident("act"/"action")
        if let Some(kw) = self.peek_ident() {
            if kw == "act" || kw == "action" {
                let kw = self.eat_ident().unwrap();
                return self.parse_free_action(&kw);
            }
        }

        // Statement boundary: allow assignment here
        let prev = self.in_stmt;
        self.in_stmt = true;
        let expr_pe = match self.parse_assign() {
            Ok(e) => e,
            Err(e) => {
                self.in_stmt = prev;
                return Err(e);
            }
        };
        self.in_stmt = prev;

        let expr = self.lower_expr(expr_pe);
        Ok(ast::Stmt::Expr(expr))
    }

    fn parse_expr(&mut self) -> ParseResult<ast::Expr> {
        let t = self.bump().ok_or_else(|| vec![Diagnostic::error(
            "ParseError", "Unexpected end of input.", Span::new("<eof>", 0, 0, 0, 0, 0, 0)
        )])?;
        let sp = t.span.clone();
        match &t.kind {
            TokenKind::Ident => {
                let text = t.value.clone().unwrap_or_default();
                Ok(ast::Expr::Ident(text, sp))
            }
            TokenKind::Int | TokenKind::Float | TokenKind::Money => {
                let text = t.value.clone().unwrap_or_default();
                Ok(ast::Expr::Number(text, sp))
            }
            TokenKind::Act | TokenKind::Action => {
                let kw = if matches!(t.kind, TokenKind::Act) { "act" } else { "action" };
                Err(vec![Diagnostic::error(
                    "ParseError",
                    &format!("Unexpected keyword '{}'.", kw),
                    sp,
                )])
            }
            _ => Err(vec![Diagnostic::error("ParseError", "Expected expression.", sp)]),
        }
    }

    fn parse_primary(&mut self) -> Result<PExpr, String> {
        if self.peek_op("(") {
            let _ = self.eat_op("(");
            let expr = self.parse_coalesce()?;
            if !self.eat_op(")") {
                return Err("expected ')' to close parenthesized expression".to_string());
            }
            return Ok(expr);
        }

        if self.eat_op("[") {
            while matches!(self.toks.get(self.i), Some(tok)
                if matches!(tok.kind, goblin_lexer::TokenKind::Newline))
            { self.i += 1; }

            let mut elems = Vec::new();

            if !self.peek_op("]") {
                loop {
                    let elem = self.parse_coalesce()?;
                    elems.push(elem);

                    if self.eat_op(",") {
                        while matches!(self.toks.get(self.i), Some(tok)
                            if matches!(tok.kind, goblin_lexer::TokenKind::Newline))
                        { self.i += 1; }
                        if self.peek_op("]") { break; }
                        continue;
                    }
                    break;
                }
            }

            while matches!(self.toks.get(self.i), Some(tok)
                if matches!(tok.kind, goblin_lexer::TokenKind::Newline))
            { self.i += 1; }

            if !self.eat_op("]") {
                return Err("expected ']' to close array".to_string());
            }
            return Ok(PExpr::Array(elems));
        }

        if self.eat_op("{") {
            while matches!(self.toks.get(self.i), Some(tok)
                if matches!(tok.kind, goblin_lexer::TokenKind::Newline))
            { self.i += 1; }

            let mut props: Vec<(String, PExpr)> = Vec::new();

            if !self.peek_op("}") {
                loop {
                    let Some(key) = self.eat_object_key() else {
                        return Err("expected object key".to_string());
                    };

                    while matches!(self.toks.get(self.i), Some(tok)
                        if matches!(tok.kind, goblin_lexer::TokenKind::Newline))
                    { self.i += 1; }

                    if !self.eat_op(":") {
                        return Err("expected ':' after object key".to_string());
                    }

                    while matches!(self.toks.get(self.i), Some(tok)
                        if matches!(tok.kind, goblin_lexer::TokenKind::Newline))
                    { self.i += 1; }

                    let value = self.parse_assign()?;
                    props.push((key, value));

                    if self.eat_op(",") {
                        while matches!(self.toks.get(self.i), Some(tok)
                            if matches!(tok.kind, goblin_lexer::TokenKind::Newline))
                        { self.i += 1; }
                        if self.peek_op("}") { break; }
                        continue;
                    }
                    break;
                }
            }

            while matches!(self.toks.get(self.i), Some(tok)
                if matches!(tok.kind, goblin_lexer::TokenKind::Newline))
            { self.i += 1; }

            if !self.eat_op("}") {
                return Err("expected '}' to close object".to_string());
            }
            return Ok(PExpr::Object(props));
        }

        let (kind, val_opt) = match self.peek() {
            Some(t) => (t.kind.clone(), t.value.clone()),
            None => return Err("expected expression, found EOF".to_string()),
        };

        match kind {
            goblin_lexer::TokenKind::Ident => {
                let name = val_opt.unwrap_or_default();
                self.i += 1;

                match name.as_str() {
                    "true" => Ok(PExpr::Bool(true)),
                    "false" => Ok(PExpr::Bool(false)),
                    "nil" => Ok(PExpr::Nil),
                    _ => Ok(PExpr::Ident(name)),
                }
            }

            goblin_lexer::TokenKind::Int => {
                let lit = val_opt.unwrap_or_default();
                self.i += 1;

                if let Some(t) = self.peek() {
                    if t.kind == goblin_lexer::TokenKind::Op("unit".into()) {
                        let unit = t.value.clone().unwrap_or_default();
                        self.i += 1;
                        return Ok(PExpr::IntWithUnit(lit, unit));
                    }
                }

                Ok(PExpr::Int(lit))
            }

            goblin_lexer::TokenKind::Float => {
                let lit = val_opt.unwrap_or_default();
                self.i += 1;

                if let Some(t) = self.peek() {
                    if t.kind == goblin_lexer::TokenKind::Op("unit".into()) {
                        let unit = t.value.clone().unwrap_or_default();
                        self.i += 1;
                        return Ok(PExpr::FloatWithUnit(lit, unit));
                    }
                }

                Ok(PExpr::Float(lit))
            }

            goblin_lexer::TokenKind::String => {
                let lit = val_opt.unwrap_or_default();
                self.i += 1;
                Ok(PExpr::Str(lit))
            }

            _ => Err("expected expression".to_string()),
        }
    }

    fn parse_assign(&mut self) -> Result<PExpr, String> {
        // lowest precedence; parse LHS
        let lhs = self.parse_coalesce()?; // uses your existing precedence stack

        // detect assignment operators (longest-first)
        let op = if self.eat_op("//=") {
            Some("//=")
        } else if self.eat_op("+=") {
            Some("+=")
        } else if self.eat_op("-=") {
            Some("-=")
        } else if self.eat_op("*=") {
            Some("*=")
        } else if self.eat_op("/=") {
            Some("/=")
        } else if self.eat_op("%=") {
            Some("%=")
        } else if self.eat_op("=") {
            Some("=")
        } else {
            None
        };

        // NEW: Assignments are only legal at statement boundaries.
        if op.is_some() && !self.in_stmt {
            return Err("assignment not allowed in expression; use a statement at block level".to_string());
        }

        if let Some(op) = op {
            match lhs {
                PExpr::Ident(_) | PExpr::Member(_, _) => {
                    // multi-line assignment block:
                    //   name =
                    //       <exprâ€¦>
                    //   end / }
                    if self.peek_newline_or_eof() {
                        self.skip_newlines();
                        let rhs = if self.eat_op("{") {
                            // brace block: parse until '}'
                            let mut last = None;
                            loop {
                                self.skip_stmt_separators();
                                if self.peek_block_close() {
                                    let _ = self.eat_block_close();
                                    break;
                                }
                                if self.toks.get(self.i).is_none() {
                                    return Err("unexpected end of file: expected '}' or 'end' to close assignment".into());
                                }
                                // expression statement inside block; take last as value
                                last = Some(self.parse_assign()?);
                            }
                            last.ok_or_else(|| "empty assignment block".to_string())?
                        } else {
                            // single-line continuation after '='
                            self.parse_assign()?
                        };

                        if op == "=" {
                            Ok(PExpr::Assign(Box::new(lhs), Box::new(rhs)))
                        } else {
                            Ok(PExpr::Binary(Box::new(lhs), op.to_string(), Box::new(rhs)))
                        }
                    } else {
                        // single-line assignment
                        let rhs = self.parse_assign()?; // right-assoc (a = b = c)
                        if op == "=" {
                            Ok(PExpr::Assign(Box::new(lhs), Box::new(rhs)))
                        } else {
                            Ok(PExpr::Binary(Box::new(lhs), op.to_string(), Box::new(rhs)))
                        }
                    }
                }
                _ => Err("left side of assignment must be an identifier or member".to_string()),
            }
        } else {
            Ok(lhs)
        }
    }

    fn parse_coalesce(&mut self) -> Result<PExpr, String> {
        // Nullish coalescing layer (above OR)
        let mut lhs = self.parse_or()?;

        while self.eat_op("??") {
            let rhs = self.parse_or()?;
            lhs = PExpr::Binary(Box::new(lhs), "??".into(), Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_or(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.parse_and()?;

        loop {
            // alias for OR
            if self.eat_op("<>") {
                let rhs = self.parse_and()?;
                lhs = PExpr::Binary(Box::new(lhs), "or".into(), Box::new(rhs));
                continue;
            }
            // textual 'or'
            if self.peek_ident() == Some("or") {
                let _ = self.eat_ident();
                let rhs = self.parse_and()?;
                lhs = PExpr::Binary(Box::new(lhs), "or".into(), Box::new(rhs));
                continue;
            }
            break;
        }

        Ok(lhs)
    }
    
    fn parse_and(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.parse_compare()?;

        loop {
            // alias for AND
            if self.eat_op("&&") {
                let rhs = self.parse_compare()?;
                lhs = PExpr::Binary(Box::new(lhs), "and".into(), Box::new(rhs));
                continue;
            }
            // textual 'and'
            if self.peek_ident() == Some("and") {
                let _ = self.eat_ident();
                let rhs = self.parse_compare()?;
                lhs = PExpr::Binary(Box::new(lhs), "and".into(), Box::new(rhs));
                continue;
            }
            break;
        }

        Ok(lhs)
    }

    fn parse_compare(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.parse_additive()?; // or parse_range() if that's your next layer

        loop {
            // textual: is / is not
            if self.peek_ident() == Some("is") {
                let _ = self.eat_ident();
                let neg = self.peek_ident() == Some("not");
                if neg { let _ = self.eat_ident(); }
                let rhs = self.parse_additive()?;
                let op = if neg { "is not" } else { "is" };
                lhs = PExpr::Binary(Box::new(lhs), op.into(), Box::new(rhs));
                continue;
            }

            if self.eat_op("===") { let rhs = self.parse_additive()?; lhs = PExpr::Binary(Box::new(lhs), "===".into(), Box::new(rhs)); continue; }
            if self.eat_op("!==") { let rhs = self.parse_additive()?; lhs = PExpr::Binary(Box::new(lhs), "!==".into(), Box::new(rhs)); continue; }
            if self.eat_op("==")  { let rhs = self.parse_additive()?; lhs = PExpr::Binary(Box::new(lhs), "==".into(),  Box::new(rhs)); continue; }
            if self.eat_op("!=")  { let rhs = self.parse_additive()?; lhs = PExpr::Binary(Box::new(lhs), "!=".into(),  Box::new(rhs)); continue; }
            if self.eat_op("<=")  { let rhs = self.parse_additive()?; lhs = PExpr::Binary(Box::new(lhs), "<=".into(),  Box::new(rhs)); continue; }
            if self.eat_op(">=")  { let rhs = self.parse_additive()?; lhs = PExpr::Binary(Box::new(lhs), ">=".into(),  Box::new(rhs)); continue; }
            if self.eat_op("<")   { let rhs = self.parse_additive()?; lhs = PExpr::Binary(Box::new(lhs), "<".into(),   Box::new(rhs)); continue; }
            if self.eat_op(">")   { let rhs = self.parse_additive()?; lhs = PExpr::Binary(Box::new(lhs), ">".into(),   Box::new(rhs)); continue; }

            break;
        }

        Ok(lhs)
    }

    fn parse_range(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.parse_join()?;

        loop {
            let op = if self.eat_op("...") { "..." }
            else if self.eat_op("..") { ".." }
            else { break };

            let rhs = self.parse_join()?;
            lhs = PExpr::Binary(Box::new(lhs), op.to_string(), Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_join(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.parse_additive()?;

        loop {
            if self.eat_op("||") {
                let rhs = self.parse_additive()?;
                lhs = PExpr::Binary(Box::new(lhs), "||".to_string(), Box::new(rhs));
                continue;
            } else if self.eat_op("|") {
                let rhs = self.parse_additive()?;
                lhs = PExpr::Binary(Box::new(lhs), "|".to_string(), Box::new(rhs));
                continue;
            }
            break;
        }

        Ok(lhs)
    }

    fn parse_additive(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.parse_multiplicative()?;

        loop {
            if self.eat_op("+") {
                let rhs = self.parse_multiplicative()?;
                lhs = PExpr::Binary(Box::new(lhs), "+".to_string(), Box::new(rhs));
            } else if self.eat_op("-") {
                let rhs = self.parse_multiplicative()?;
                lhs = PExpr::Binary(Box::new(lhs), "-".to_string(), Box::new(rhs));
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    fn parse_multiplicative(&mut self) -> Result<PExpr, String> {
        // One level tighter than additive
        let mut lhs = self.parse_power()?;

        loop {
            let op = if self.eat_op("><") { "><" }      // divmod
                     else if self.eat_op("//") { "//" } // integer division
                     else if self.eat_op("*")  { "*"  }
                     else if self.eat_op("/")  { "/"  }
                     else if self.eat_op("%")  { "%"  }
                     else { break };

            let rhs = self.parse_power()?;
            lhs = PExpr::Binary(Box::new(lhs), op.to_string(), Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_power(&mut self) -> Result<PExpr, String> {
        let lhs = self.parse_unary()?;

        if self.eat_op("**") {
            let rhs = self.parse_power()?;
            return Ok(PExpr::Binary(Box::new(lhs), "**".to_string(), Box::new(rhs)));
        }

        if self.eat_op("^^") {
            let rhs = self.parse_power()?;
            return Ok(PExpr::Binary(Box::new(lhs), "^^".to_string(), Box::new(rhs)));
        }

        Ok(lhs)
    }

    fn parse_kv_bind_list_judge(&mut self) -> Result<Vec<(String, PExpr)>, String> {
        use goblin_lexer::TokenKind;

        let mut out: Vec<(String, PExpr)> = Vec::new();

        loop {
            // Stop when we see a block closer 'end' or '}' (caller will consume it)
            self.skip_newlines();
            if self.peek_block_close() {
                break;
            }

            // Capture everything from here up to the first ':' at nesting level 0
            let start_i = self.i;
            let mut depth_paren = 0i32;
            let mut depth_brack = 0i32;
            let mut depth_brace = 0i32;

            let mut end_i: Option<usize> = None;
            while let Some(tok) = self.toks.get(self.i) {
                match &tok.kind {
                    TokenKind::Op(op) if op == ":" => {
                        if depth_paren == 0 && depth_brack == 0 && depth_brace == 0 {
                            end_i = Some(self.i);
                            break;
                        }
                        // ':' inside nested ()/[]/{} – part of the expression
                        self.i += 1;
                    }

                    TokenKind::Op(op) if op == "(" => { depth_paren += 1; self.i += 1; }
                    TokenKind::Op(op) if op == ")" => { depth_paren -= 1; self.i += 1; }
                    TokenKind::Op(op) if op == "[" => { depth_brack += 1; self.i += 1; }
                    TokenKind::Op(op) if op == "]" => { depth_brack -= 1; self.i += 1; }
                    TokenKind::Op(op) if op == "{" => { depth_brace += 1; self.i += 1; }
                    TokenKind::Op(op) if op == "}" => { depth_brace -= 1; self.i += 1; }

                    // Treat layout as whitespace while searching for the first ':' at top level
                    TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent
                        if depth_paren == 0 && depth_brack == 0 && depth_brace == 0 =>
                    {
                        self.i += 1;
                    }

                    // Any other token – keep scanning
                    _ => { self.i += 1; }
                }
            }

            let Some(colon_i) = end_i else {
                return Err("expected ':' after condition in judge".to_string());
            };

            // Build a readable key string from tokens [start_i .. colon_i)
            // (We only need a stable textual key right now; evaluation will happen later.)
            let mut parts: Vec<String> = Vec::new();
            for t in &self.toks[start_i..colon_i] {
                match &t.kind {
                    TokenKind::Op(op) => parts.push(op.clone()),
                    _ => parts.push(t.value.clone().unwrap_or_default()),
                }
            }
            // Simple join with spaces; keeps operators recognizable
            let key_text = parts.join(" ").trim().to_string();

            // Consume the ':'
            self.i = colon_i;
            let _ = self.eat_op(":");

            // Optional layout after ':'
            while let Some(t) = self.peek() {
                match t.kind {
                    TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent => { self.i += 1; }
                    _ => break,
                }
            }

            // Parse value expression
            let val = self.parse_assign()?;
            out.push((key_text, val));

            // Allow trailing commas or line breaks between pairs
            self.eat_semi_separators();
            if self.peek_block_close() {
                break;
            }
        }

        Ok(out)
    }

    fn parse_unary(&mut self) -> Result<PExpr, String> {
        // expression-form: judge â€¦ (must close with 'end' or '}')
        if let Some("judge") = self.peek_ident() {
            let _ = self.eat_ident(); // consume 'judge'
            self.skip_newlines();

            // key: expr pairs (your existing newline/colon tolerant routine)
            let pairs = self.parse_kv_bind_list_judge()?;

            self.skip_newlines();
            self.expect_block_close("judge")?;

            return Ok(PExpr::Object(pairs));
        }

        // prefix &  (definedness) â€' parse only an lvalue chain after '&'
        if self.eat_op("&") {
            let target = self.parse_lvalue_after_amp()?;
            return Ok(PExpr::IsBound(Box::new(target)));
        }

        // logical-not alias
        if self.eat_op("!") {
            let rhs = self.parse_unary()?;
            return Ok(PExpr::Prefix("!".into(), Box::new(rhs)));
        }

        // unary plus/minus
        if self.eat_op("+") {
            let rhs = self.parse_unary()?;
            return Ok(PExpr::Prefix("+".into(), Box::new(rhs)));
        }
        if self.eat_op("-") {
            let rhs = self.parse_unary()?;
            return Ok(PExpr::Prefix("-".into(), Box::new(rhs)));
        }

        // hand off to your postfix/member pipeline
        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.parse_member()?;

        loop {
            // indexing / slicing
            if self.eat_op("[") {
                while matches!(self.toks.get(self.i), Some(tok)
                    if matches!(tok.kind, goblin_lexer::TokenKind::Newline))
                { self.i += 1; }

                if self.eat_op("]") {
                    return Err("expected expression inside '[]'".to_string());
                }

                let mut start: Option<PExpr> = None;
                if !self.peek_op(":") {
                    // NOTE: must produce PExpr + String error
                    start = Some(self.parse_coalesce()?);
                    while matches!(self.toks.get(self.i), Some(tok)
                        if matches!(tok.kind, goblin_lexer::TokenKind::Newline))
                    { self.i += 1; }
                }

                // slice?
                if self.eat_op(":") {
                    while matches!(self.toks.get(self.i), Some(tok)
                        if matches!(tok.kind, goblin_lexer::TokenKind::Newline))
                    { self.i += 1; }

                    let mut end: Option<PExpr> = None;
                    if !self.peek_op("]") && !self.peek_op(":") {
                        end = Some(self.parse_coalesce()?);
                        while matches!(self.toks.get(self.i), Some(tok)
                            if matches!(tok.kind, goblin_lexer::TokenKind::Newline))
                        { self.i += 1; }
                    }

                    let mut step: Option<PExpr> = None;
                    if self.eat_op(":") {
                        while matches!(self.toks.get(self.i), Some(tok)
                            if matches!(tok.kind, goblin_lexer::TokenKind::Newline))
                        { self.i += 1; }

                        if !self.peek_op("]") {
                            step = Some(self.parse_assign()?);
                            while matches!(self.toks.get(self.i), Some(tok)
                                if matches!(tok.kind, goblin_lexer::TokenKind::Newline))
                            { self.i += 1; }
                        }
                    }

                    if !self.eat_op("]") {
                        return Err("expected ']' after slice".to_string());
                    }

                    lhs = if step.is_some() {
                        PExpr::Slice3(
                            Box::new(lhs),
                            start.map(Box::new),
                            end.map(Box::new),
                            step.map(Box::new),
                        )
                    } else {
                        PExpr::Slice(
                            Box::new(lhs),
                            start.map(Box::new),
                            end.map(Box::new),
                        )
                    };
                    continue;
                }

                while matches!(self.toks.get(self.i), Some(tok)
                    if matches!(tok.kind, goblin_lexer::TokenKind::Newline))
                { self.i += 1; }

                if !self.eat_op("]") {
                    return Err("expected ']' after index expression".to_string());
                }
                let idx = start.expect("index expression parsed");
                lhs = PExpr::Index(Box::new(lhs), Box::new(idx));
                continue;
            }

            // postfix operators
            if self.eat_op("++") { lhs = PExpr::Postfix(Box::new(lhs), "++".to_string()); continue; }
            if self.eat_op("--") { lhs = PExpr::Postfix(Box::new(lhs), "--".to_string()); continue; }
            if self.eat_op("**") { lhs = PExpr::Postfix(Box::new(lhs), "**".to_string()); continue; } // square
            if self.eat_op("//") { lhs = PExpr::Postfix(Box::new(lhs), "//".to_string()); continue; } // sqrt (postfix form)
            if self.eat_op("?")  { lhs = PExpr::IsBound(Box::new(lhs)); continue; }
            if self.eat_op("!")  { lhs = PExpr::Postfix(Box::new(lhs), "!".to_string());  continue; }
            if self.eat_op("^")  { lhs = PExpr::Postfix(Box::new(lhs), "^".to_string());  continue; }
            if self.eat_op("_")  { lhs = PExpr::Postfix(Box::new(lhs), "_".to_string());  continue; }

            break;
        }

        Ok(lhs)
    }

    fn parse_member(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.parse_primary()?;

        loop {
            if self.eat_op(">>") {
                if let Some(name) = self.eat_ident() {
                    lhs = PExpr::Member(Box::new(lhs), name);
                    continue;
                } else if let Some(key) = self.eat_string_lit() {
                    lhs = PExpr::Member(Box::new(lhs), key);
                    continue;
                } else {
                    return Err("expected identifier or string after '>>'".to_string());
                }
            }

            if self.eat_op("?>>") {
                if self.eat_op("(") {
                    let mut args = Vec::new();
                    while matches!(self.toks.get(self.i), Some(tok)
                        if matches!(tok.kind, goblin_lexer::TokenKind::Newline))
                    { self.i += 1; }

                    if !self.peek_op(")") {
                        loop {
                            let arg = self.parse_coalesce()?;
                            args.push(arg);
                            if self.eat_op(",") {
                                while matches!(self.toks.get(self.i), Some(tok)
                                    if matches!(tok.kind, goblin_lexer::TokenKind::Newline))
                                { self.i += 1; }
                                if self.peek_op(")") { break; }
                                continue;
                            }
                            break;
                        }
                    }
                    if !self.eat_op(")") {
                        return Err("expected ')' to close optional call".to_string());
                    }
                    lhs = match lhs {
                        PExpr::OptMember(obj, name) => PExpr::OptCall(obj, name, args),
                        other => PExpr::OptCall(Box::new(other), String::new(), args),
                    };
                    continue;
                } else {
                    let Some(name) = self.eat_ident() else {
                        return Err("expected identifier after '?>>'".to_string());
                    };
                    lhs = PExpr::OptMember(Box::new(lhs), name);
                    continue;
                }
            }

            if self.eat_op(":") {
                let mut args = Vec::new();
                while matches!(self.toks.get(self.i), Some(tok)
                    if matches!(tok.kind, goblin_lexer::TokenKind::Newline))
                { self.i += 1; }

                if self.peek_newline_or_eof() {
                    return Err("expected argument after ':'".to_string());
                }

                loop {
                    let arg = self.parse_coalesce()?;
                    args.push(arg);
                    if self.eat_op(",") {
                        while matches!(self.toks.get(self.i), Some(tok)
                            if matches!(tok.kind, goblin_lexer::TokenKind::Newline))
                        { self.i += 1; }
                        if self.peek_newline_or_eof() { break; }
                        continue;
                    }
                    break;
                }

                lhs = match lhs {
                    PExpr::Member(obj, name) => PExpr::Call(obj, name, args),
                    PExpr::OptMember(obj, name) => PExpr::OptCall(obj, name, args),
                    PExpr::Ident(name) => PExpr::FreeCall(name, args),
                    other => {
                        return Err(format!("invalid colon-call target: {:?}", other));
                    }
                };
                continue;
            }

            if self.eat_op("(") {
                let mut args = Vec::new();
                while matches!(self.toks.get(self.i), Some(tok)
                    if matches!(tok.kind, goblin_lexer::TokenKind::Newline))
                { self.i += 1; }

                if !self.peek_op(")") {
                    loop {
                        let arg = self.parse_coalesce()?;
                        args.push(arg);
                        if self.eat_op(",") {
                            while matches!(self.toks.get(self.i), Some(tok)
                                if matches!(tok.kind, goblin_lexer::TokenKind::Newline))
                            { self.i += 1; }
                            if self.peek_op(")") { break; }
                            continue;
                        }
                        break;
                    }
                }
                if !self.eat_op(")") {
                    return Err("expected ')' after argument list".to_string());
                }

                lhs = match lhs {
                    PExpr::Member(obj, name) => PExpr::Call(obj, name, args),
                    PExpr::OptMember(obj, name) => PExpr::OptCall(obj, name, args),
                    PExpr::Ident(name) => PExpr::FreeCall(name, args),
                    other => PExpr::Call(Box::new(other), String::new(), args),
                };
                continue;
            }

            if matches!(&lhs, PExpr::Ident(_)) && self.peek_op("::") {
                let ns = if let PExpr::Ident(ref s) = lhs { s.clone() } else { unreachable!() };
                let _ = self.eat_op("::");

                let Some(opname) = self.eat_ident() else {
                    return Err("expected identifier after '::'".to_string());
                };

                if self.eat_op("(") {
                    let args = if self.eat_op(")") {
                        vec![]
                    } else {
                        self.parse_args_paren()?
                    };
                    lhs = PExpr::NsCall(ns, opname, args);
                } else if self.eat_op(":") {
                    let args = self.parse_args_colon()?;
                    lhs = PExpr::NsCall(ns, opname, args);
                } else {
                    lhs = PExpr::NsCall(ns, opname, vec![]);
                }
                continue;
            }

            break;
        }

        Ok(lhs)
    }

    fn parse_lvalue_after_amp(&mut self) -> Result<PExpr, String> {
        let Some(name) = self.eat_ident() else {
            return Err("SyntaxError: '&' requires a variable/field/index".into());
        };
        let mut expr = PExpr::Ident(name);

        loop {
            if self.eat_op("?>>") {
                let Some(field) = self.eat_ident() else {
                    return Err("SyntaxError: expected field name after '?>>'".into());
                };
                expr = PExpr::OptMember(Box::new(expr), field);
                continue;
            }
            if self.eat_op(">>") {
                let Some(field) = self.eat_ident() else {
                    return Err("SyntaxError: expected field name after '>>'".into());
                };
                expr = PExpr::Member(Box::new(expr), field);
                continue;
            }
            if self.eat_op("[") {
                // NOTE: must produce PExpr + String error
                let key = self.parse_coalesce()?;
                if !self.eat_op("]") { return Err("SyntaxError: missing ']' in index".into()); }
                expr = PExpr::Index(Box::new(expr), Box::new(key));
                continue;
            }
            break;
        }

        Ok(expr)
    }

    fn parse_args_paren(&mut self) -> Result<Vec<PExpr>, String> {
        let mut args = Vec::new();

        loop {
            let expr = self.parse_coalesce()?;
            args.push(expr);

            if self.eat_op(",") {
                continue;
            } else if self.eat_op(")") {
                break;
            } else {
                return Err("expected ',' or ')' in argument list".to_string());
            }
        }

        Ok(args)
    }

    fn parse_args_colon(&mut self) -> Result<Vec<PExpr>, String> {
        let mut args = Vec::new();
        args.push(self.parse_coalesce()?);
        while self.eat_op(",") {
            args.push(self.parse_coalesce()?);
        }
        Ok(args)
    }

    fn eat_semi_separators(&mut self) {
        loop {
            if self.eat_op(";") {
                continue;
            }
            match self.peek() {
                Some(t) if matches!(t.kind, goblin_lexer::TokenKind::Newline) => {
                    self.i += 1;
                    continue;
                }
                _ => {}
            }
            break;
        }
    }
}

pub(crate) fn parse_expr_preview(tokens: &[goblin_lexer::Token]) -> Result<PExpr, String> {
    let mut p = Parser::new(tokens);
    p.parse_coalesce()
}

pub(crate) fn parse_program(tokens: &[goblin_lexer::Token]) -> Result<Vec<PDecl>, String> {
    let mut p = Parser::new(tokens);
    let mut out = Vec::new();

    while p.i < p.toks.len() {
        p.eat_semi_separators();
        p.skip_newlines();
        if p.i >= p.toks.len() {
            break;
        }

        let decl = p.parse_decl()?;
        out.push(decl);

        p.eat_semi_separators();
        p.skip_newlines();
    }

    Ok(out)
}

pub(crate) fn parse_program_preview(tokens: &[goblin_lexer::Token]) -> Result<Vec<PExpr>, String> {
    let mut p = Parser::new(tokens);
    let mut out = Vec::new();

    while p.i < p.toks.len() {
        p.eat_semi_separators();
        p.skip_newlines();
        if p.i >= p.toks.len() { break; }

        if p.peek_op("@") {
            if let Some(res) = p.try_parse_class_decl() {
                let node = res?;
                out.push(node);
                p.eat_semi_separators();
                p.skip_newlines();
                continue;
            }
        }

        let expr = p.parse_coalesce()?;
        out.push(expr);

        p.eat_semi_separators();
        p.skip_newlines();
    }

    Ok(out)
}
