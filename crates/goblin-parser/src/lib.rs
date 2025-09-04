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
    Slice3(Box<PExpr>, Option<Box<PExpr>>, Option<Box<PExpr>>, Option<Box<PExpr>>),
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

pub type ParseResult<T> = Result<T, Vec<Diagnostic>>;

pub struct Parser<'t> {
    toks: &'t [Token],
    i: usize,
}

fn is_block_starter_name(name: &str) -> bool {
    matches!(name, "if" | "for" | "while" | "repeat" | "judge")
}

impl<'t> Parser<'t> {
    pub fn new(toks: &'t [Token]) -> Self { Self { toks, i: 0 } }

    fn span_from_tokens(toks: &[goblin_lexer::Token], start_i: usize, end_i: usize) -> Span {
        if toks.is_empty() {
            return Span::new("<empty>", 0, 0, 0, 0, 0, 0);
        }
        let start_tok = toks.get(start_i.min(toks.len()-1));
        let end_tok = toks.get(end_i.min(toks.len()-1));
        
        match (start_tok, end_tok) {
            (Some(s), Some(e)) => {
                Span::new(
                    s.span.file.clone(),
                    s.span.start,
                    e.span.end,
                    s.span.line_start,
                    e.span.line_end,
                    s.span.col_start,
                    e.span.col_end,
                )
            }
            (Some(s), None) => s.span.clone(),
            _ => Span::new("<unknown>", 0, 0, 0, 0, 0, 0),
        }
    }

    fn lower_expr_preview(pe: PExpr, sp: Span) -> ast::Expr {
        match pe {
            PExpr::Ident(name) => ast::Expr::Ident(name, sp),
            PExpr::Int(s) | PExpr::Float(s) | PExpr::IntWithUnit(s, _) | PExpr::FloatWithUnit(s, _) => {
                ast::Expr::Number(s, sp)
            }
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
            let Some(tok) = self.toks.get(self.i) else {
                return Err("unexpected end of input while parsing action block".to_string());
            };

            // Close on 'end' or '}'.
            match &tok.kind {
                TokenKind::Ident if tok.value.as_deref() == Some("end") => {
                    self.i += 1;
                    depth -= 1;
                    if depth == 0 { break; }
                    continue;
                }
                TokenKind::Op(op) if op == "}" => {
                    self.i += 1;
                    depth -= 1;
                    if depth == 0 { break; }
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

        // Single-line action: `act foo = expr`
        if self.eat_op("=") {
            let expr = self.parse_coalesce()?;
            return Ok(PAction { name, params, body: vec![expr], is_single: true });
        }

        // Multi-line action body: ends with either `end` or `}`
        let mut body = Vec::new();
        self.eat_semi_separators();

        loop {
            if let Some(tok) = self.toks.get(self.i) {
                let is_end = matches!(tok.kind, TokenKind::Ident) && tok.value.as_deref() == Some("end");
                let is_rbrace = matches!(tok.kind, TokenKind::Op(ref s) if s == "}");
                if is_end || is_rbrace {
                    self.i += 1;
                    break;
                }
            } else {
                return Err(format!("unterminated {} '{}': missing '}}' or 'end'", kw, name));
            }

            let expr = self.parse_coalesce()?;
            body.push(expr);
            self.eat_semi_separators();
        }

        Ok(PAction { name, params, body, is_single: false })
    }

    fn parse_free_action(&mut self, kw: &str) -> Result<ast::Stmt, String> {
        let pa = self.parse_action_after_keyword(kw)?;
        let body = pa
            .body
            .into_iter()
            .map(|pe| ast::Stmt::Expr(self.lower_expr(pe)))
            .collect();
        let act = ast::ActionDecl {
            name: pa.name,
            params: pa.params,
            body,
            is_single: pa.is_single,
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
        let mut out = Vec::new();
        loop {
            self.skip_stmt_separators();
            if self.peek_block_close() {
                let _ = self.eat_block_close(); // guaranteed true
                break;
            }
            // EOF before closer = hard error
            if self.toks.get(self.i).is_none() {
                return Err("unexpected end of file: expected '}' or 'end' to close block".into());
            }
            out.push(self.parse_stmt()?);
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
            if let Some(tok) = self.toks.get(self.i) {
                let is_end = matches!(tok.kind, TokenKind::Ident) && tok.value.as_deref() == Some("end");
                let is_rbrace = matches!(tok.kind, TokenKind::Op(ref s) if s == "}");
                if is_end || is_rbrace {
                    self.i += 1;
                    break;
                }
            } else {
                return Err(format!("unterminated class '{}': missing '}}' or 'end'", name));
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

        Ok(PExpr::ClassDecl { name, fields, actions })
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
        matches!(kind, TokenKind::Ident | TokenKind::Int | TokenKind::Float | TokenKind::Money)
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
        if let Some(res) = self.try_parse_class_decl() {
            let p = res?;
            return Ok(ast::Stmt::Expr(self.lower_expr(p)));
        }

        if let Some(kw) = self.peek_ident() {
            if kw == "act" || kw == "action" {
                let kw = self.eat_ident().unwrap();
                return self.parse_free_action(&kw);
            }
        }

        let expr_pe = self.parse_assign()?;
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

        if let Some(op) = op {
            match lhs {
                PExpr::Ident(_) | PExpr::Member(_, _) => {
                    // multi-line assignment block:
                    //   name =
                    //       <expr…>
                    //   end / }
                    if self.peek_newline_or_eof() {
                        self.skip_newlines();
                        let rhs = self.parse_assign()?; // allow full precedence on RHS
                        self.expect_block_close("assignment")?;
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
            if self.eat_op("=")   { return Err("assignments are only allowed as statements".to_string()); }
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

    fn parse_unary(&mut self) -> Result<PExpr, String> {
        // expression-form: judge … (must close with 'end' or '}')
        if let Some("judge") = self.peek_ident() {
            let _ = self.eat_ident(); // consume 'judge'
            self.skip_newlines();

            // key: expr pairs (your existing newline/colon tolerant routine)
            let pairs = self.parse_kv_bind_list()?;

            self.skip_newlines();
            self.expect_block_close("judge")?;

            return Ok(PExpr::Object(pairs));
        }

        // prefix &  (definedness) — parse only an lvalue chain after '&'
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