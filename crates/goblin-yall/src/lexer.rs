use crate::error::YallError;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    LBrace,     // {
    RBrace,     // }
    LBracket,   // [
    RBracket,   // ]
    Colon,      // :
    Comma,      // ,
    Bare(String),
    Str(String),
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub col: usize,
}

pub struct Lexer<'a> {
    text: &'a str,
    chars: Vec<char>,
    index: usize,
    line: usize,
    col: usize,
    label: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str, label: &'a str) -> Self {
        Self {
            text,
            chars: text.chars().collect(),
            index: 0,
            line: 1,
            col: 1,
            label,
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.index).copied()
    }

    fn next(&mut self) -> Option<char> {
        let ch = self.chars.get(self.index).copied()?;
        self.index += 1;

        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(ch)
    }

    fn error<T>(&self, msg: &str) -> Result<T, YallError> {
        Err(YallError::new(self.label, self.line, msg))
    }

    fn skip_ws(&mut self) {
        while let Some(ch) = self.peek() {
            if ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' {
                self.next();
            } else {
                break;
            }
        }
    }

    fn skip_comment(&mut self) {
        // Only treat as comment if preceded by whitespace ON THIS LINE.
        if self.index == 0 { return; }

        let prev = self.chars.get(self.index - 1).copied().unwrap_or('x');

        if prev.is_whitespace() {
            while let Some(ch) = self.peek() {
                self.next();
                if ch == '\n' { break; }
            }
        }
    }

    fn read_string(&mut self) -> Result<Token, YallError> {
        let start_line = self.line;
        let start_col = self.col;

        // consume opening "
        self.next();

        let mut buf = String::new();
        let mut escape = false;

        while let Some(ch) = self.next() {
            if escape {
                let real = match ch {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\\' => '\\',
                    '"' => '"',
                    other => other,
                };
                buf.push(real);
                escape = false;
                continue;
            }

            match ch {
                '\\' => escape = true,
                '"'  => {
                    return Ok(Token {
                        kind: TokenKind::Str(buf),
                        line: start_line,
                        col: start_col,
                    });
                }
                _ => buf.push(ch),
            }
        }

        self.error("unterminated string")
    }

    fn read_bare(&mut self) -> Result<Token, YallError> {
        let start_line = self.line;
        let start_col = self.col;

        let mut buf = String::new();

        while let Some(ch) = self.peek() {
            if ch.is_whitespace()
                || ch == ':'
                || ch == ','
                || ch == '{'
                || ch == '}'
                || ch == '['
                || ch == ']'
            {
                break;
            }
            buf.push(ch);
            self.next();
        }

        // validate bare string according to Y’ALL v1.0 rules
        if buf.contains('"')
            || buf.contains(' ')
            || buf.contains('\t')
            || buf.contains('{')
            || buf.contains('}')
            || buf.contains('[')
            || buf.contains(']')
            || buf.contains(':')
        {
            return self.error("invalid bare string");
        }

        Ok(Token {
            kind: TokenKind::Bare(buf),
            line: start_line,
            col: start_col,
        })
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, YallError> {
        let mut tokens = Vec::new();

        while let Some(ch) = self.peek() {
            // whitespace
            if ch.is_whitespace() {
                self.skip_ws();
                continue;
            }

            // comment?
            if ch == '#' {
                self.skip_comment();
                continue;
            }

            let line = self.line;
            let col = self.col;

            match ch {
                '{' => {
                    self.next();
                    tokens.push(Token { kind: TokenKind::LBrace, line, col });
                }
                '}' => {
                    self.next();
                    tokens.push(Token { kind: TokenKind::RBrace, line, col });
                }
                '[' => {
                    self.next();
                    tokens.push(Token { kind: TokenKind::LBracket, line, col });
                }
                ']' => {
                    self.next();
                    tokens.push(Token { kind: TokenKind::RBracket, line, col });
                }
                ':' => {
                    self.next();
                    tokens.push(Token { kind: TokenKind::Colon, line, col });
                }
                ',' => {
                    self.next();
                    tokens.push(Token { kind: TokenKind::Comma, line, col });
                }
                '"' => {
                    tokens.push(self.read_string()?);
                }
                _ => {
                    if ch.is_alphanumeric() || ch == '_' || ch == '-' || ch == '#' {
                        tokens.push(self.read_bare()?);
                    } else {
                        return self.error(&format!("unexpected character '{}'", ch));
                    }
                }
            }
        }

        Ok(tokens)
    }
}
