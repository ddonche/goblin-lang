use goblin_ast as ast;
use goblin_diagnostics::{Diagnostic, Span};
use goblin_lexer::{Token, TokenKind};

#[derive(Debug, Clone)]
enum PExpr {
    Ident(String),
    Int(String),
    Float(String),
    Str(String),
    Postfix(Box<PExpr>, String),
    Binary(Box<PExpr>, String, Box<PExpr>),
    Member(Box<PExpr>, String),
    Call(Box<PExpr>, String, Vec<PExpr>),
    OptCall(Box<PExpr>, String, Vec<PExpr>),
    Index(Box<PExpr>, Box<PExpr>),
    Array(Vec<PExpr>),
    Object(Vec<(String, PExpr)>),
    FreeCall(String, Vec<PExpr>),
    NsCall(String, String, Vec<PExpr>),
    Prefix(String, Box<PExpr>),
    Assign(Box<PExpr>, Box<PExpr>),
    IsBound(Box<PExpr>),
    Bool(bool),
    Nil,
}

pub type ParseResult<T> = Result<T, Vec<Diagnostic>>;

pub struct Parser<'t> {
    toks: &'t [Token],
    i: usize,
}

impl<'t> Parser<'t> {
    pub fn new(toks: &'t [Token]) -> Self { Self { toks, i: 0 } }

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
        // Clone the value while only immutably borrowing self,
        // then advance the cursor after the borrow ends.
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

    fn is_lvalue(expr: &PExpr) -> bool {
        match expr {
            PExpr::Ident(_)        => true,
            PExpr::Member(_, _)    => true,
            // If you have indexing in the AST, uncomment the next line:
            // PExpr::Index(_, _)  => true,
            _ => false,
        }    
    }

    #[inline]
    fn peek_string_lit(&self) -> Option<&str> {
        self.peek().and_then(|t| {
            if let goblin_lexer::TokenKind::String = t.kind {
                t.value.as_deref()
            } else {
                None
            }
        })
    }

    #[inline]
    fn eat_string_lit(&mut self) -> Option<String> {
        // Borrow-safe: capture value first, then advance.
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

    // Borrow-safe bump: compute index first, then get by index
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
            // Skip anything that cannot start an expression (temporary, while the parser is tiny)
            loop {
                match self.peek().map(|t| &t.kind) {
                    Some(k) if Self::is_expr_start(k) => break,
                    Some(TokenKind::Eof) | None => break,
                    // also swallow layout that might slip through
                    Some(TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent) => {
                        self.i += 1;
                    }
                    // punctuation/operators/etc. — skip for now
                    _ => {
                        self.i += 1;
                    }
                }
            }

            if self.is_eof() { break; }
            items.push(self.parse_stmt()?);
            self.eat_layout();
        }

        Ok(ast::Module { items })
    }

    fn parse_stmt(&mut self) -> ParseResult<ast::Stmt> {
        self.eat_layout(); // be tolerant
        let e = self.parse_expr()?;
        Ok(ast::Stmt::Expr(e))
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
            _ => Err(vec![Diagnostic::error("ParseError", "Expected expression.", sp)]),
        }
    }

    fn parse_primary(&mut self) -> Result<PExpr, String> {
        // Parenthesized grouping: (expr)
        if self.peek_op("(") {
            let _ = self.eat_op("("); // consume '('
            let expr = self.parse_assign()?; // full precedence inside parens
            if !self.eat_op(")") {
                return Err("expected ')' to close parenthesized expression".to_string());
            }
            return Ok(expr);
        }

        // Array literal: [expr, expr, ...] with optional trailing comma
        if self.eat_op("[") {
            // empty []
            if self.eat_op("]") {
                return Ok(PExpr::Array(vec![]));
            }

            let mut items = Vec::new();
            loop {
                items.push(self.parse_assign()?);
                if self.eat_op("]") {
                    break;
                }
                if !self.eat_op(",") {
                    return Err("expected ',' or ']' in array literal".to_string());
                }
                // allow trailing comma
                if self.eat_op("]") {
                    break;
                }
            }

            return Ok(PExpr::Array(items));
        }

        // Object literal: { key: value, ... } with optional trailing comma
        if self.eat_op("{") {
            // empty {}
            if self.eat_op("}") {
                return Ok(PExpr::Object(vec![]));
            }

            let mut pairs: Vec<(String, PExpr)> = Vec::new();
            loop {
                let Some(key) = self.eat_object_key() else {
                    return Err("expected identifier or string key in object literal".to_string());
                };
                if !self.eat_op(":") {
                    return Err("expected ':' after object key".to_string());
                }
                let value = self.parse_assign()?;
                pairs.push((key, value));

                if self.eat_op("}") {
                    break;
                }
                if !self.eat_op(",") {
                    return Err("expected ',' or '}' in object literal".to_string());
                }
                // allow trailing comma
                if self.eat_op("}") {
                    break;
                }
            }

            return Ok(PExpr::Object(pairs));
        }

        // Capture the token first (avoid borrow issues), then bump `self.i`.
        let (kind, val_opt) = match self.peek() {
            Some(t) => (t.kind.clone(), t.value.clone()),
            None => return Err("expected expression, found EOF".to_string()),
        };

        match kind {
            goblin_lexer::TokenKind::Ident => {
                let name = val_opt.unwrap_or_default();
                self.i += 1;

                // Namespaced call: Name::op(...)
                if self.eat_op("::") {
                    let Some(op) = self.eat_ident() else {
                        return Err("expected identifier after '::'".to_string());
                    };

                    // ( ... ) form
                    if self.eat_op("(") {
                        if self.eat_op(")") {
                            return Ok(PExpr::NsCall(name, op, vec![]));
                        } else {
                            let args = self.parse_args_paren()?;
                            return Ok(PExpr::NsCall(name, op, args));
                        }
                    }

                    // Paren-less single-line: Name::op: a, b
                    if self.eat_op(":") {
                        let args = self.parse_args_colon()?;
                        return Ok(PExpr::NsCall(name, op, args));
                    }

                    // Zero-arg without parens
                    return Ok(PExpr::NsCall(name, op, vec![]));
                }

                // Free call: name(...)
                if self.eat_op("(") {
                    if self.eat_op(")") {
                        return Ok(PExpr::FreeCall(name, vec![]));
                    } else {
                        let args = self.parse_args_paren()?;
                        return Ok(PExpr::FreeCall(name, args));
                    }
                }

                // Free call: name: a, b   (paren-less)
                if self.eat_op(":") {
                    let args = self.parse_args_colon()?;
                    return Ok(PExpr::FreeCall(name, args));
                }

                // Not a call → map literals to real nodes
                match name.as_str() {
                    "true"  => Ok(PExpr::Bool(true)),
                    "false" => Ok(PExpr::Bool(false)),
                    "nil"   => Ok(PExpr::Nil),
                    _ => Ok(PExpr::Ident(name)),
                }
            }
            goblin_lexer::TokenKind::Int => {
                let lit = val_opt.unwrap_or_default();
                self.i += 1;
                Ok(PExpr::Int(lit))
            }
            goblin_lexer::TokenKind::Float => {
                let lit = val_opt.unwrap_or_default();
                self.i += 1;
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

    fn parse_postfix(&mut self) -> Result<PExpr, String> {
        // Start at prefix level (handles unary prefix if you add it later)
        let mut lhs = self.parse_prefix()?;

        loop {
            // --- indexing: value[expr] ---
            if self.eat_op("[") {
                if self.eat_op("]") {
                    return Err("expected expression inside '[]'".to_string());
                }
                let idx = self.parse_assign()?; // full expression as index
                if !self.eat_op("]") {
                    return Err("expected ']' after index expression".to_string());
                }
                lhs = PExpr::Index(Box::new(lhs), Box::new(idx));
                continue;
            }

            // --- member access: value >> field ---
            if self.eat_op(">>") {
                let Some(name) = self.eat_ident() else {
                    return Err("expected identifier after '>>'".to_string());
                };
                lhs = PExpr::Member(Box::new(lhs), name);
                continue;
            }

            // --- calls: .foo(...) or ?.foo(...) ---
            if self.peek_op(".") || self.peek_op("?.") {
                lhs = self.parse_call_from(lhs)?;
                continue;
            }

            // --- postfix operators (factorial/ceil/floor/etc.) ---
            if self.eat_op("!") {
                lhs = PExpr::Postfix(Box::new(lhs), "!".to_string());
                continue;
            }
            if self.eat_op("^") {
                lhs = PExpr::Postfix(Box::new(lhs), "^".to_string());
                continue;
            }
            if self.eat_op("_") {
                lhs = PExpr::Postfix(Box::new(lhs), "_".to_string());
                continue;
            }

            // Done with postfix chain
            break;
        }

        Ok(lhs)
    }

    fn parse_assign(&mut self) -> Result<PExpr, String> {
        // Right-assoc assignment, lowest precedence
        let lhs = self.parse_coalesce()?;  // <— was parse_or()

        // Detect any assignment op (longest-first). Note: no >>= support.
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
                    let rhs = self.parse_assign()?; // right-associative (a = b = c)
                    if op == "=" {
                        Ok(PExpr::Assign(Box::new(lhs), Box::new(rhs)))
                    } else {
                        Ok(PExpr::Binary(Box::new(lhs), op.to_string(), Box::new(rhs)))
                    }
                }
                _ => Err("left side of assignment must be an identifier or member".to_string()),
            }
        } else {
            Ok(lhs)
        }
    }

    fn parse_and(&mut self) -> Result<PExpr, String> {
        // AND binds tighter than OR, but looser than joins/additive/etc.
        let mut lhs = self.parse_join()?; // was parse_additive() before we added joins

        while self.eat_op("&&") {
            let rhs = self.parse_join()?;
            lhs = PExpr::Binary(Box::new(lhs), "&&".to_string(), Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_or(&mut self) -> Result<PExpr, String> {
        // OR binds looser than AND, tighter than ?? (handled by parse_nullish)
        let mut lhs = self.parse_and()?; // was parse_join() or parse_compare()

        while self.eat_op("<>") {
            let rhs = self.parse_and()?;
            lhs = PExpr::Binary(Box::new(lhs), "<>".to_string(), Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_power(&mut self) -> Result<PExpr, String> {
        // right-assoc: a ** b ** c == a ** (b ** c)
        let mut lhs = self.parse_prefix()?;

        // accept either ** or ^^
        let op = if self.eat_op("**") {
            Some("**")
        } else if self.eat_op("^^") {
            Some("^^")
        } else {
            None
        };

        if let Some(op) = op {
            let rhs = self.parse_power()?; // recurse for right-assoc
            lhs = PExpr::Binary(Box::new(lhs), op.to_string(), Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_divmod(&mut self) -> Result<PExpr, String> {
        // PREVIOUSLY: let mut lhs = self.parse_member()? / self.parse_postfix()? / self.parse_prefix()?;
        let mut lhs = self.parse_power()?;  // ← start from power now

        loop {
            if self.eat_op("*") {
                let rhs = self.parse_power()?;
                lhs = PExpr::Binary(Box::new(lhs), "*".to_string(), Box::new(rhs));
            } else if self.eat_op("/") {
                let rhs = self.parse_power()?;
                lhs = PExpr::Binary(Box::new(lhs), "/".to_string(), Box::new(rhs));
            } else if self.eat_op("//") {
                let rhs = self.parse_power()?;
                lhs = PExpr::Binary(Box::new(lhs), "//".to_string(), Box::new(rhs));
            } else if self.eat_op("%") {
                let rhs = self.parse_power()?;
                lhs = PExpr::Binary(Box::new(lhs), "%".to_string(), Box::new(rhs));
            } else if self.eat_op("><") {
                let rhs = self.parse_power()?;
                lhs = PExpr::Binary(Box::new(lhs), "><".to_string(), Box::new(rhs));
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    fn parse_member(&mut self) -> Result<PExpr, String> {
        // dot-calls bind tighter than member, so build on parse_call()
        let mut lhs = self.parse_call()?;

        loop {
            if self.eat_op(">>") {
                if let Some(name) = self.eat_ident() {
                    lhs = PExpr::Member(Box::new(lhs), name);
                    // NEW: allow chaining calls right after member
                    lhs = self.parse_call_from(lhs)?;
                    continue;
                }
                if let Some(key) = self.eat_string_lit() {
                    lhs = PExpr::Member(Box::new(lhs), key);
                    // NEW: allow chaining calls right after member
                    lhs = self.parse_call_from(lhs)?;
                    continue;
                }
                return Err("expected identifier or string after '>>'".to_string());
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    #[inline]
    fn parse_expr_tiny(&mut self) -> Result<PExpr, String> {
        // or ( <>)  >  divmod (><)  >  member (>>)  >  postfix (! ^ _ )  >  primary
        self.parse_or()
    }

    fn parse_call(&mut self) -> Result<PExpr, String> {
        // Tighter than member (>>), built on postfix
        let mut lhs = self.parse_postfix()?;

        loop {
            // Namespaced: H::op(...), H::op: ...
            if matches!(&lhs, PExpr::Ident(_)) && self.peek_op("::") {
                let ns = if let PExpr::Ident(ref s) = lhs { s.clone() } else { unreachable!() };
                let _ = self.eat_op("::");

                let Some(opname) = self.eat_ident() else {
                    return Err("expected identifier after '::'".to_string());
                };

                if self.eat_op("(") {
                    if self.eat_op(")") {
                        lhs = PExpr::NsCall(ns, opname, vec![]);
                    } else {
                        let args = self.parse_args_paren()?;
                        lhs = PExpr::NsCall(ns, opname, args);
                    }
                } else if self.eat_op(":") {
                    let args = self.parse_args_colon()?;
                    lhs = PExpr::NsCall(ns, opname, args);
                } else {
                    lhs = PExpr::NsCall(ns, opname, vec![]);
                }
                continue;
            }

            // Free call: name(...), name: ...
            if matches!(&lhs, PExpr::Ident(_)) && (self.peek_op("(") || self.peek_op(":")) {
                let name = if let PExpr::Ident(ref s) = lhs { s.clone() } else { unreachable!() };

                if self.eat_op("(") {
                    if self.eat_op(")") {
                        lhs = PExpr::FreeCall(name, vec![]);
                    } else {
                        let args = self.parse_args_paren()?;
                        lhs = PExpr::FreeCall(name, args);
                    }
                } else if self.eat_op(":") {
                    let args = self.parse_args_colon()?;
                    lhs = PExpr::FreeCall(name, args);
                }
                continue;
            }

            // Dot-calls (now delegated)
            if self.peek_op(".") {
                lhs = self.parse_call_from(lhs)?;
                continue;
            }

            break;
        }

        Ok(lhs)
    }

    fn parse_args_paren(&mut self) -> Result<Vec<PExpr>, String> {
        let mut args = Vec::new();

        // Empty: already confirmed caller saw '(' and next isn't ')'
        loop {
            // parse one expression (use full precedence stack)
            let expr = self.parse_or()?; 
            args.push(expr);

            // comma or close
            if self.eat_op(",") {
                // continue to next arg
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
        // Parse one or more arguments separated by commas, no closing delimiter.
        let mut args = Vec::new();
        // at least one expr
        args.push(self.parse_assign()?);
        while self.eat_op(",") {
            args.push(self.parse_assign()?);
        }
        Ok(args)
    }

    // Strict lvalue parser used ONLY after '&' to avoid recursive loops.
    // Accepts: ident ( '.' field | '?.' field | '[' expr ']' )*
    // Accepts: ident ( '.' field | '?.' field | '[' expr ']' )*
    fn parse_lvalue_after_amp(&mut self) -> Result<PExpr, String> {
        let Some(name) = self.eat_ident() else {
            return Err("SyntaxError: '&' requires a variable/field/index".into());
        };
        let mut expr = PExpr::Ident(name);

        loop {
            if self.eat_op("?.") {
                let Some(field) = self.eat_ident() else {
                    return Err("SyntaxError: expected field name after '?.'".into());
                };
                expr = PExpr::Member(Box::new(expr), field);
            } else if self.eat_op(".") {
                let Some(field) = self.eat_ident() else {
                    return Err("SyntaxError: expected field name after '.'".into());
                };
                expr = PExpr::Member(Box::new(expr), field);
            } else if self.eat_op("[") {
                let key = self.parse_expr_tiny()?;  // you already have this
                if !self.eat_op("]") {
                    return Err("SyntaxError: missing ']' in index".into());
                }
                expr = PExpr::Index(Box::new(expr), Box::new(key)); // if you don’t have Index, omit this case
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<PExpr, String> {
        // NEW: & definedness (prefix). Parse ONLY an lvalue chain to avoid recursion loops.
        if self.eat_op("&") {
            let target = self.parse_lvalue_after_amp()?;
            return Ok(PExpr::IsBound(Box::new(target)));
        }

        // existing logical NOT so !&expr works naturally
        if self.eat_op("!") {
            let rhs = self.parse_unary()?;
            return Ok(PExpr::Prefix("!".into(), Box::new(rhs)));
        }

        // unary plus
        if self.eat_op("+") {
            let rhs = self.parse_unary()?;
            return Ok(PExpr::Prefix("+".into(), Box::new(rhs)));
        }

        // unary minus
        if self.eat_op("-") {
            let rhs = self.parse_unary()?;
            return Ok(PExpr::Prefix("-".into(), Box::new(rhs)));
        }

        // … any other prefix cases you already support …

        // fall through to your normal postfix/primary pipeline
        self.parse_postfix()
    }
    
    fn parse_prefix(&mut self) -> Result<PExpr, String> {
        // NEW: & definedness
        if self.eat_op("&") {
            let target = self.parse_lvalue_after_amp()?;
            return Ok(PExpr::IsBound(Box::new(target)));
        }

        if self.eat_op("-") {
            let rhs = self.parse_prefix()?;
            return Ok(PExpr::Prefix("-".to_string(), Box::new(rhs)));
        }
        if self.eat_op("+") {
            let rhs = self.parse_prefix()?;
            return Ok(PExpr::Prefix("+".to_string(), Box::new(rhs)));
        }
        if self.eat_op("!") {
            let rhs = self.parse_prefix()?;
            return Ok(PExpr::Prefix("!".to_string(), Box::new(rhs)));
        }

        // fall through to the next tighter layer
        self.parse_member()
    }

    fn parse_call_from(&mut self, mut lhs: PExpr) -> Result<PExpr, String> {
        loop {
            // method-style: .name(...) / .name: ... / zero-arg .name
            if self.eat_op(".") {
                let Some(name) = self.eat_ident() else {
                    return Err("expected identifier after '.'".to_string());
                };
                if self.eat_op("(") {
                    if self.eat_op(")") {
                        lhs = PExpr::Call(Box::new(lhs), name, vec![]);
                    } else {
                        let args = self.parse_args_paren()?;
                        lhs = PExpr::Call(Box::new(lhs), name, args);
                    }
                } else if self.eat_op(":") {
                    let args = self.parse_args_colon()?;
                    lhs = PExpr::Call(Box::new(lhs), name, args);
                } else {
                    lhs = PExpr::Call(Box::new(lhs), name, vec![]);
                }
                continue;
            }

            // OPTIONAL method-style: ?.name(...) / ?.name: ... / zero-arg ?.name
            if self.eat_op("?.") {
                let Some(name) = self.eat_ident() else {
                    return Err("expected identifier after '?.'".to_string());
                };
                if self.eat_op("(") {
                    if self.eat_op(")") {
                        lhs = PExpr::OptCall(Box::new(lhs), name, vec![]);
                    } else {
                        let args = self.parse_args_paren()?;
                        lhs = PExpr::OptCall(Box::new(lhs), name, args);
                    }
                } else if self.eat_op(":") {
                    let args = self.parse_args_colon()?;
                    lhs = PExpr::OptCall(Box::new(lhs), name, args);
                } else {
                    lhs = PExpr::OptCall(Box::new(lhs), name, vec![]);
                }
                continue;
            }

            // member access (fields only): receiver >> field
            if self.eat_op(">>") {
                let Some(field) = self.eat_ident() else {
                    return Err("expected identifier after '>>'".to_string());
                };
                lhs = PExpr::Member(Box::new(lhs), field);
                continue;
            }

            break;
        }
        Ok(lhs)
    }

    fn parse_additive(&mut self) -> Result<PExpr, String> {
        // Additive family (left-assoc): +, -
        // Precedence so far:
        // postfix → .call → >> member → prefix → mult(parse_divmod) → (here) additive → or
        let mut lhs = self.parse_divmod()?;

        loop {
            if self.eat_op("+") {
                let rhs = self.parse_divmod()?;
                lhs = PExpr::Binary(Box::new(lhs), "+".to_string(), Box::new(rhs));
            } else if self.eat_op("-") {
                let rhs = self.parse_divmod()?;
                lhs = PExpr::Binary(Box::new(lhs), "-".to_string(), Box::new(rhs));
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    fn parse_join(&mut self) -> Result<PExpr, String> {
        // Joins bind looser than arithmetic, tighter than comparisons.
        // We’ll wire it into the chain on the next step.
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

    fn parse_nullish(&mut self) -> Result<PExpr, String> {
        // Looser than logical (<> / &&), tighter than assignment
        let mut lhs = self.parse_or()?;  // you already have parse_or() for <> / &&

        while self.eat_op("??") {
            let rhs = self.parse_or()?;
            lhs = PExpr::Binary(Box::new(lhs), "??".to_string(), Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_coalesce(&mut self) -> Result<PExpr, String> {
        // coalesce is looser than logical OR
        let mut lhs = self.parse_or()?;

        while self.eat_op("??") {
            let rhs = self.parse_or()?;
            lhs = PExpr::Binary(Box::new(lhs), "??".to_string(), Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_compare(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.parse_range()?; // was parse_additive()

        loop {
            if self.eat_op("===") {
                let rhs = self.parse_range()?; // and all RHS in this function
                lhs = PExpr::Binary(Box::new(lhs), "===".to_string(), Box::new(rhs));
            } else if self.eat_op("!==") {
                let rhs = self.parse_range()?;
                lhs = PExpr::Binary(Box::new(lhs), "!==".to_string(), Box::new(rhs));
            } else if self.eat_op("==") {
                let rhs = self.parse_range()?;
                lhs = PExpr::Binary(Box::new(lhs), "==".to_string(), Box::new(rhs));
            } else if self.eat_op("!=") {
                let rhs = self.parse_range()?;
                lhs = PExpr::Binary(Box::new(lhs), "!=".to_string(), Box::new(rhs));
            } else if self.eat_op("<=") {
                let rhs = self.parse_range()?;
                lhs = PExpr::Binary(Box::new(lhs), "<=".to_string(), Box::new(rhs));
            } else if self.eat_op(">=") {
                let rhs = self.parse_range()?;
                lhs = PExpr::Binary(Box::new(lhs), ">=".to_string(), Box::new(rhs));
            } else if self.eat_op("<") {
                let rhs = self.parse_range()?;
                lhs = PExpr::Binary(Box::new(lhs), "<".to_string(), Box::new(rhs));
            } else if self.eat_op(">") {
                let rhs = self.parse_range()?;
                lhs = PExpr::Binary(Box::new(lhs), ">".to_string(), Box::new(rhs));
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    fn eat_semi_separators(&mut self) {
        loop {
            if self.eat_op(";") {
                continue;
            }
            // Also swallow NEWLINE tokens from the lexer
            match self.peek() {
                Some(t) if matches!(t.kind, goblin_lexer::TokenKind::Newline) => {
                    self.i += 1; // consume newline
                    continue;
                }
                _ => {}
            }
            break;
        }
    }

    fn parse_range(&mut self) -> Result<PExpr, String> {
        // Ranges sit above joins; joins sit above additive.
        let mut lhs = self.parse_join()?;

        loop {
            if self.eat_op("...") {
                let rhs = self.parse_join()?;
                lhs = PExpr::Binary(Box::new(lhs), "...".to_string(), Box::new(rhs));
            } else if self.eat_op("..") {
                let rhs = self.parse_join()?;
                lhs = PExpr::Binary(Box::new(lhs), "..".to_string(), Box::new(rhs));
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    fn is_assign_target(e: &PExpr) -> bool {
        matches!(e, PExpr::Ident(_) | PExpr::Member(_, _))
    }

    fn eat_any_assign(&mut self) -> Option<&'static str> {
        // Longest-first where relevant; tokens are already disambiguated by the lexer.
        for op in ["<<=", ">>=", "//=", "+=", "-=", "*=", "/=", "%=", "="] {
            if self.eat_op(op) { return Some(op); }
        }
        None
    }



}

// --- temporary preview entry for expressions (keeps current parser untouched) ---
pub(crate) fn parse_expr_preview(tokens: &[goblin_lexer::Token]) -> Result<PExpr, String> {
    let mut p = Parser::new(tokens);
    let expr = p.parse_assign()?; // was parse_or()
    Ok(expr)
}    

pub(crate) fn parse_program_preview(tokens: &[goblin_lexer::Token]) -> Result<Vec<PExpr>, String> {
    let mut p = Parser::new(tokens);
    let mut exprs = Vec::new();

    while p.i < p.toks.len() {
        p.eat_semi_separators();
        if p.i >= p.toks.len() {
            break;
        }

        let expr = p.parse_assign()?;

        // Must be followed by a statement terminator we consume,
        // or by a closing delimiter. Anything else is an error.
        let before = p.i;
        p.eat_semi_separators();
        if p.i == before {
            if let Some(t) = p.peek() {
                let end_block = match &t.kind {
                    goblin_lexer::TokenKind::Op(s) => s == ")" || s == "]" || s == "}",
                    _ => false,
                };
                if !end_block {
                    return Err("expected newline or ';' between expressions".to_string());
                }
            }
        }

        exprs.push(expr);
    }

    Ok(exprs)
}