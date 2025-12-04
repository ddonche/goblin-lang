use indexmap::IndexMap;

use crate::error::YallError;
use crate::lexer::{Lexer, Token, TokenKind};
use crate::value::YallValue;

/// Represents one logical line already stripped of comments.
#[derive(Debug)]
struct Line<'a> {
    indent: usize,
    key: &'a str,
    rest: &'a str, // after "key:" or after "- "
    raw: &'a str,
    line_no: usize,
}

pub struct Parser<'a> {
    lines: Vec<Line<'a>>,
    label: &'a str,
}

impl<'a> Parser<'a> {
    pub fn new(cleaned_text: &'a str, label: &'a str) -> Self {
        let mut lines = Vec::new();

        for (i, raw) in cleaned_text.lines().enumerate() {
            let line_no = i + 1;
            let trimmed = raw.trim();

            if trimmed.is_empty() {
                continue;
            }

            // count indent
            let indent = raw.chars().take_while(|c| *c == ' ').count();

            // BLOCK LIST?
            if trimmed.starts_with("- ") {
                let rest = &trimmed[2..];
                lines.push(Line {
                    indent,
                    key: "-",
                    rest,
                    raw,
                    line_no,
                });
                continue;
            }

            // BLOCK KEY: VALUE?
            if let Some(pos) = trimmed.find(':') {
                let key = &trimmed[..pos];
                let after = &trimmed[pos + 1..];

                // allow pure "key:" meaning nested block starts
                let rest = after.trim_start();

                lines.push(Line {
                    indent,
                    key,
                    rest,
                    raw,
                    line_no,
                });
                continue;
            }

            // SCALAR TOP-LEVEL (error)
            lines.push(Line {
                indent,
                key: trimmed,
                rest: "",
                raw,
                line_no,
            });
        }

        Self { lines, label }
    }

    fn error<T>(&self, line: usize, msg: &str) -> Result<T, YallError> {
        Err(YallError::new(self.label, line, msg))
    }

    /// Entry point: parse the whole file → Map
    pub fn parse(&self) -> Result<YallValue, YallError> {
        let mut idx = 0;
        let root = self.parse_map(0, &mut idx)?;
        Ok(YallValue::Map(root))
    }

    fn parse_map(
        &self,
        expected_indent: usize,
        idx: &mut usize,
    ) -> Result<IndexMap<String, YallValue>, YallError> {
        let mut map = IndexMap::new();

        while *idx < self.lines.len() {
            let line = &self.lines[*idx];

            // If indentation decreases → parent map ends
            if line.indent < expected_indent {
                break;
            }

            // If indentation increases → malformed
            if line.indent > expected_indent {
                return self.error(line.line_no, "unexpected indentation");
            }

            // If we hit a list item here, that means this map key's value
            // is actually a LIST. Delegate to parse_value_block.
            if line.key == "-" {
                // A block like:
                //   key:
                //     - item
                //
                // is handled by parse_value_block at the parent's level.
                //
                // So we break here and let parse_value_block handle it.
                break;
            }

            let key = line.key.to_string();
            let line_no = line.line_no;

            // KEY: (no value) → nested block
            if line.rest.is_empty() {
                *idx += 1;
                let child = self.parse_value_block(expected_indent + 2, idx)?;
                map.insert(key, child);
                continue;
            }

            // KEY: inline scalar / inline structure
            let val = self.parse_inline_or_scalar(line.rest, line_no)?;
            *idx += 1;
            map.insert(key, val);
        }

        Ok(map)
    }

    /// Parse a single list item starting at `-`.
    /// Supports:
    ///   - "- scalar"
    ///   - "- { inline: map }"
    ///   - "- [inline, list]"
    ///   - "- key: val" with additional "sibling" lines:
    ///         - slug: docs
    ///           href: /default/docs.html
    ///   - "- trailboss::build_routes_once"  (namespaced scalar)
    fn parse_list_item(
        &self,
        expected_indent: usize,
        idx: &mut usize,
    ) -> Result<YallValue, YallError> {
        let line    = &self.lines[*idx];
        let line_no = line.line_no;

        // "- " with nothing after it → nested block
        if line.rest.is_empty() {
            *idx += 1;
            return self.parse_value_block(expected_indent + 2, idx);
        }

        let trimmed = line.rest.trim();

        // SPECIAL CASE: namespaced identifiers like "mod::action"
        // We NEVER want to treat these as "key: value" maps.
        if trimmed.contains("::") {
            let v = self.parse_scalar(trimmed, line_no)?;
            *idx += 1;
            return Ok(v);
        }

        // "- { ... }" or "- [ ... ]" → inline structures
        if trimmed.starts_with('{') || trimmed.starts_with('[') {
            let val = self.parse_inline_or_scalar(line.rest, line_no)?;
            *idx += 1;
            return Ok(val);
        }

        // Check if this looks like "key: value" → map-style list item
        if let Some(pos) = trimmed.find(':') {
            let key_raw  = &trimmed[..pos];
            let val_text = trimmed[pos + 1..].trim();

            let key = key_raw.trim().to_string();
            let mut map = IndexMap::new();

            let first_val = if val_text.is_empty() {
                // "key:" with no inline value – treat as null for now
                YallValue::Null
            } else {
                self.parse_inline_or_scalar(val_text, line_no)?
            };
            map.insert(key, first_val);

            // Consume the "-" line
            *idx += 1;

            // Now consume any sibling "key: value" lines at indent = expected_indent + 2
            while *idx < self.lines.len() {
                let next = &self.lines[*idx];

                if next.indent < expected_indent + 2 {
                    break; // parent will handle
                }
                if next.indent > expected_indent + 2 {
                    return self.error(
                        next.line_no,
                        "unexpected indentation under list item",
                    );
                }
                if next.key == "-" {
                    break; // start of next list item
                }

                let key = next.key.trim().to_string();
                let next_line_no = next.line_no;

                if next.rest.is_empty() {
                    // "key:" with nested block underneath
                    *idx += 1;
                    let child = self.parse_value_block(expected_indent + 4, idx)?;
                    map.insert(key, child);
                } else {
                    let v = self.parse_inline_or_scalar(next.rest, next_line_no)?;
                    map.insert(key, v);
                    *idx += 1;
                }
            }

            return Ok(YallValue::Map(map));
        }

        // Fallback: "- scalar"
        let v = self.parse_scalar(trimmed, line_no)?;
        *idx += 1;
        Ok(v)
    }

    // ---------------------------
    // Block LIST
    // ---------------------------
    fn parse_list(
        &self,
        expected_indent: usize,
        idx: &mut usize,
    ) -> Result<Vec<YallValue>, YallError> {
        let mut out = Vec::new();

        while *idx < self.lines.len() {
            let line = &self.lines[*idx];

            // list item must start exactly at expected_indent
            if line.indent < expected_indent {
                break;
            }
            if line.indent > expected_indent {
                return self.error(line.line_no, "unexpected indentation in list");
            }

            // no more list items at this level
            if line.key != "-" {
                break;
            }

            let item = self.parse_list_item(expected_indent, idx)?;
            out.push(item);
        }

        Ok(out)
    }

    // ---------------------------
    // Parse a nested block: either MAP or LIST
    // ---------------------------
    fn parse_value_block(
        &self,
        expected_indent: usize,
        idx: &mut usize,
    ) -> Result<YallValue, YallError> {
        if *idx >= self.lines.len() {
            return Ok(YallValue::Map(IndexMap::new()));
        }

        let line = &self.lines[*idx];

        if line.indent < expected_indent {
            // empty block
            return Ok(YallValue::Map(IndexMap::new()));
        }

        if line.key == "-" {
            let list = self.parse_list(expected_indent, idx)?;
            return Ok(YallValue::Array(list));
        }

        // otherwise MAP
        let map = self.parse_map(expected_indent, idx)?;
        Ok(YallValue::Map(map))
    }

    // ---------------------------
    // Inline or scalar
    // ---------------------------
    fn parse_inline_or_scalar(
        &self,
        text: &str,
        line_no: usize,
    ) -> Result<YallValue, YallError> {
        let trimmed = text.trim();

        // INLINE MAP OR ARRAY?
        if trimmed.starts_with('{') || trimmed.starts_with('[') {
            return self.parse_inline(trimmed, line_no);
        }

        // otherwise SCALAR
        self.parse_scalar(trimmed, line_no)
    }

    // ---------------------------
    // Inline structures using lexer
    // ---------------------------
    fn parse_inline(
        &self,
        text: &str,
        line_no: usize,
    ) -> Result<YallValue, YallError> {
        let mut lex = Lexer::new(text, self.label);
        let tokens = lex.tokenize()?;

        let mut idx = 0;
        self.parse_tokens(&tokens, &mut idx, line_no)
    }

    fn parse_tokens(
        &self,
        toks: &[Token],
        idx: &mut usize,
        line_no: usize,
    ) -> Result<YallValue, YallError> {
        if *idx >= toks.len() {
            return self.error(line_no, "unexpected end of inline structure");
        }

        match &toks[*idx].kind {
            TokenKind::LBrace => self.parse_inline_map(toks, idx, line_no),
            TokenKind::LBracket => self.parse_inline_array(toks, idx, line_no),
            _ => self.error(line_no, "inline value must start with '{' or '['"),
        }
    }

    fn parse_inline_map(
        &self,
        toks: &[Token],
        idx: &mut usize,
        line_no: usize,
    ) -> Result<YallValue, YallError> {
        let mut map = IndexMap::new();
        *idx += 1; // consume {

        loop {
            if *idx >= toks.len() {
                return self.error(line_no, "unterminated inline map");
            }

            match &toks[*idx].kind {
                TokenKind::RBrace => {
                    *idx += 1;
                    break;
                }
                TokenKind::Comma => {
                    *idx += 1;
                    continue;
                }
                TokenKind::Bare(key) | TokenKind::Str(key) => {
                    let key = key.clone();
                    *idx += 1;

                    // expect colon
                    if *idx >= toks.len() || toks[*idx].kind != TokenKind::Colon {
                        return self.error(line_no, "missing colon in inline map");
                    }
                    *idx += 1;

                    let val = self.parse_inline_value(toks, idx, line_no)?;
                    map.insert(key, val);
                }
                _ => return self.error(line_no, "unexpected token in inline map"),
            }
        }

        Ok(YallValue::Map(map))
    }

    fn parse_inline_array(
        &self,
        toks: &[Token],
        idx: &mut usize,
        line_no: usize,
    ) -> Result<YallValue, YallError> {
        let mut arr = Vec::new();
        *idx += 1; // consume [

        loop {
            if *idx >= toks.len() {
                return self.error(line_no, "unterminated inline array");
            }

            match &toks[*idx].kind {
                TokenKind::RBracket => {
                    *idx += 1;
                    break;
                }
                TokenKind::Comma => {
                    *idx += 1;
                    continue;
                }
                _ => {
                    let val = self.parse_inline_value(toks, idx, line_no)?;
                    arr.push(val);
                }
            }
        }

        Ok(YallValue::Array(arr))
    }

    fn parse_inline_value(
        &self,
        toks: &[Token],
        idx: &mut usize,
        line_no: usize,
    ) -> Result<YallValue, YallError> {
        if *idx >= toks.len() {
            return self.error(line_no, "unexpected end");
        }

        match &toks[*idx].kind {
            TokenKind::LBrace => self.parse_inline_map(toks, idx, line_no),
            TokenKind::LBracket => self.parse_inline_array(toks, idx, line_no),

            TokenKind::Str(s) => {
                let v = YallValue::Str(s.clone());
                *idx += 1;
                Ok(v)
            }

            TokenKind::Bare(s) => {
                let val = self.parse_scalar(s, line_no)?;
                *idx += 1;
                Ok(val)
            }

            other => self.error(line_no, &format!("unexpected token: {:?}", other)),
        }
    }

    // ---------------------------
    // SCALARS
    // ---------------------------
    fn parse_scalar(&self, text: &str, line_no: usize) -> Result<YallValue, YallError> {
        let trimmed = text.trim();

        // ---------- DOUBLE-QUOTED STRING ----------
        // Handles things like: "trailboss::build_routes_once"
        if trimmed.starts_with('"') && trimmed.ends_with('"') && trimmed.len() >= 2 {
            let inner = &trimmed[1..trimmed.len() - 1];
            let mut out = String::new();
            let mut chars = inner.chars();

            while let Some(c) = chars.next() {
                if c == '\\' {
                    // escape sequence
                    let next = match chars.next() {
                        Some(n) => n,
                        None => {
                            return self.error(line_no, "unterminated escape in string");
                        }
                    };

                    match next {
                        '"' => out.push('"'),
                        '\\' => out.push('\\'),
                        'n' => out.push('\n'),
                        'r' => out.push('\r'),
                        't' => out.push('\t'),
                        other => out.push(other),
                    }
                } else {
                    out.push(c);
                }
            }

            return Ok(YallValue::Str(out));
        }

        // ---------- KEYWORDS ----------
        match trimmed {
            "null" => return Ok(YallValue::Null),
            "true" => return Ok(YallValue::Bool(true)),
            "false" => return Ok(YallValue::Bool(false)),
            _ => {}
        }

        // INT?
        if trimmed.chars().all(|c| c.is_ascii_digit() || c == '-' || c == '+') {
            if let Ok(i) = trimmed.parse::<i64>() {
                return Ok(YallValue::Int(i));
            }
        }

        // FLOAT?
        let mut has_decimal = false;
        let mut has_digit = false;
        for c in trimmed.chars() {
            if c.is_ascii_digit() { has_digit = true; continue; }
            if c == '.' && !has_decimal { has_decimal = true; continue; }
            // anything else breaks float
            has_digit = false;
            break;
        }
        if has_digit {
            if let Ok(f) = trimmed.parse::<f64>() {
                return Ok(YallValue::Float(f));
            }
        }

        // ---------- FALLBACK: BARE STRING ----------
        Ok(YallValue::Str(trimmed.to_string()))
    }
}
