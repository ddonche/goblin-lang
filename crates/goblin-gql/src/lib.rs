use std::fmt;

// ====== Public AST ======

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action { Grab, Fetch }

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Scope {
    Latest(u64),
    Oldest(u64),
    All,
    Ident(String),                // reserved
    IdentWithId(String, String),  // e.g., "user 123"
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Query {
    pub version: Option<u32>,
    pub action: Action,
    pub scope: Option<Scope>,
    pub entity: String,
    pub lines: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node {
    pub depth: usize,         // number of '-' (>=1); may be "virtual depth" for where bodies
    pub kind: LineKind,
    pub children: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LineKind {
    FieldSpec { entity: String, fields: FieldSel }, // `- entity [a, b]` / `- entity []` / `- entity`
    Relation  { name: String,   fields: FieldSel }, // `-- relation [x]` / `-- relation`
    Command   { name: String, args: Option<String>, is_block: bool }, // `- :command ...` or block opener (no braces)
    BlockCloseMarker, // unused for indent-style blocks; kept for compatibility
    Comment(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FieldSel {
    Default,        // no brackets => default fields
    All,            // []
    Some(Vec<String>), // [a, b, c]
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DslError {
    pub code: &'static str,
    pub message: String,
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for DslError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {} (line {}, col {})", self.code, self.message, self.line, self.col)
    }
}
impl std::error::Error for DslError {}

// ====== S-tier diagnostics helper ======

#[inline]
fn s_help(line: usize, col: usize, code: &'static str, msg: &str, help: &str) -> DslError {
    let message = format!("{msg}\n\nhelp: {help}");
    DslError { code, message, line, col }
}

// ====== Entry point ======

pub fn parse_query(src: &str) -> Result<Query, DslError> {
    let mut it = LineIter::new(src);

    // Optional version header
    let mut version: Option<u32> = None;
    if let Some((ln, raw)) = it.peek_nonempty() {
        let t = raw.trim_start();
        if let Some(rest) = t.strip_prefix("version ") {
            let (num, _) = take_number(rest.trim_start());
            if let Some(n) = num {
                version = Some(n as u32);
                let _ = it.next_nonempty(); // consume
            } else {
                return Err(s_help(
                    ln, 1, "P0111",
                    "Invalid version header. Expected `version <number>`.",
                    "Write:\n  version 1",
                ));
            }
        }
    }

    // Required header
    let (hdr_line, header) = it
        .next_nonempty()
        .ok_or_else(|| s_help(1, 1, "P0110",
            "Expected a header line starting with `grab` or `fetch`.",
            "Begin with:\n  grab\nor\n  grab latest 10\nor\n  fetch all"))?;
    let header = header.trim();

    let (action, rest) = if let Some(rest) = header.strip_prefix("grab ") {
        (Action::Grab, rest.trim())
    } else if let Some(rest) = header.strip_prefix("fetch ") {
        (Action::Fetch, rest.trim())
    } else if header == "grab" {
        (Action::Grab, "")
    } else if header == "fetch" {
        (Action::Fetch, "")
    } else {
        return Err(s_help(
            hdr_line, 1, "P0110",
            "Expected a header line starting with `grab` or `fetch`.",
            "Begin with:\n  grab\nor\n  grab latest 10\nor\n  fetch all",
        ));
    };

    // Scope only (entity inferred from first level-1 FieldSpec)
    let scope = parse_scope(rest, hdr_line)?;
    let entity = String::new();

    // Collect body lines until `xx` (skip blank, comments, and `::` separators)
    let mut raw_lines: Vec<RawLine> = Vec::new();
    while let Some((ln, raw)) = it.next_any() {
        let t = raw.trim();
        if t.is_empty() || t.starts_with('#') { continue; }
        if t == "xx" { break; }
        if t == "::" { continue; }
        raw_lines.push(RawLine { line_no: ln, content: raw.to_string() });
    }

    // Scan → flat nodes
    let mut flat_nodes = Vec::new();

    // Per-dash-depth "indent block" stacks for :where/:and_where/:or_where
    #[derive(Clone, Debug)]
    struct BlockEntry { base_indent: usize, had_body: bool, line: usize, col: usize }
    let mut indent_blocks: Vec<Vec<BlockEntry>> = Vec::new();

    // Guard against skipping depth
    let mut last_virtual_depth: Option<usize> = None;

    for rl in raw_lines {
        // Parse dash depth and count of spaces after dashes (indent within this depth)
        let (depth, indent, content, col) = parse_dash_depth(&rl.content, rl.line_no)?;
        // A single space after '-' is just a separator, not a where-body indent.
        let eff_indent = indent.saturating_sub(1);

        // Ensure stacks sized
        if indent_blocks.len() <= depth { indent_blocks.resize(depth + 1, Vec::new()); }
        let stack = &mut indent_blocks[depth];

        // Lone '}' is invalid in this spec
        if content.trim() == "}" {
            return Err(s_help(
                rl.line_no, col, "P0202",
                "Unexpected `}`. Blocks are indentation-based; braces aren’t allowed.",
                "For multi-line where, write:\n  - :where\n  -   status == \"published\"\n  -   :or_where category >> name == \"tech\"",
            ));
        }

        // If we see indent but no open where-block at this depth → error
        if eff_indent > 0 && stack.is_empty() {
            return Err(s_help(
                rl.line_no, col, "P0208",
                "Indented predicate/command without a `:where`/`:and_where`/`:or_where` opener at this depth.",
                "Open a where-block, then indent its body:\n  - :where\n  -   featured == true\n  -   :or_where category >> name == \"tech\"",
            ));
        }

        // Dedent: close any blocks whose base_indent >= current indent
        while let Some(top) = stack.last() {
            if eff_indent <= top.base_indent {
                // If a block closes with no body lines, it's an error
                if !top.had_body {
                    return Err(s_help(
                        top.line, top.col, "P0209",
                        "Empty where-block: expected at least one indented condition or command.",
                        "Add a condition (or remove the block):\n  - :where\n  -   status == \"published\"",
                    ));
                }
                stack.pop();
            } else {
                break;
            }
        }

        // Inside a where-block body if indent > current top.base_indent
        let in_where_body = stack.last().map(|b| eff_indent > b.base_indent).unwrap_or(false);
        if in_where_body {
            // mark the nearest open block as having a body
            if let Some(top) = stack.last_mut() { top.had_body = true; }
        }

        // Compute virtual depth for AST building
        let vdepth = if in_where_body { depth + 1 } else { depth };

        // Depth skipping guard (on virtual depth)
        if let Some(prev) = last_virtual_depth {
            if vdepth > prev + 1 {
                return Err(s_help(
                    rl.line_no, col, "P1002",
                    "Invalid nesting: you can’t skip a level.",
                    "Add one dash per hop:\n  - posts\n  -- comments\n  --- author",
                ));
            }
        }
        last_virtual_depth = Some(vdepth);

        // Comments (after dashes) — allowed anywhere
        if content.trim_start().starts_with('#') {
            flat_nodes.push(Node {
                depth: vdepth,
                kind: LineKind::Comment(content.trim_start().to_string()),
                children: Vec::new(),
            });
            continue;
        }

        // COMMAND or PREDICATE?
        if let Some(rest) = content.trim_start().strip_prefix(':') {
            // Command line
            let mut parts = rest.trim().splitn(2, char::is_whitespace);
            let name = parts.next().unwrap().to_string();
            let tail = parts.next().map(|s| s.trim()).unwrap_or("");

            // Braces are not allowed in this spec
            if tail.starts_with('{') {
                return Err(s_help(
                    rl.line_no, col, "P0213",
                    "Braces are not allowed for where-blocks.",
                    "Use indentation instead:\n  - :where\n  -   status == \"published\"",
                ));
            }

            // Indent-style block opener => name in {where,and_where,or_where} and no args
            let is_block = tail.is_empty()
                && matches!(name.as_str(), "where" | "and_where" | "or_where");

            if is_block {
                // Push block opener with this indent baseline
                stack.push(BlockEntry { base_indent: eff_indent, had_body: false, line: rl.line_no, col });
            }

            let args = if tail.is_empty() { None } else { Some(tail.to_string()) };

            flat_nodes.push(Node {
                depth: vdepth,
                kind: LineKind::Command { name, args, is_block },
                children: Vec::new(),
            });
            continue;
        }

        // If we're in a where body and the line does not start with ':', it's a predicate expression
        if in_where_body {
            let expr = content.trim();
            if expr.is_empty() {
                // ignore empty padding lines
                continue;
            }
            flat_nodes.push(Node {
                depth: vdepth,
                kind: LineKind::Command { name: "pred".to_string(), args: Some(expr.to_string()), is_block: false },
                children: Vec::new(),
            });
            continue;
        }

        // Otherwise it's a selection (entity/relation). Disallow `>>` here; only valid inside :where.
        if content.contains(">>") {
            return Err(s_help(
                rl.line_no, col, "P1505",
                "Invalid selection syntax: `>>` is only allowed inside `:where` paths.",
                "Use dash depth for joins in selections, and `>>` inside `:where`:\n  -- author [username]\n  - :where author >> id == 123",
            ));
        }

        let (ident, fields) = parse_entity_with_fields(content.trim(), rl.line_no, col)?;
        let kind = if ident == "PARENT" || depth >= 2 {
            LineKind::Relation { name: ident, fields }
        } else {
            LineKind::FieldSpec { entity: ident, fields }
        };
        flat_nodes.push(Node { depth: vdepth, kind, children: Vec::new() });
    }

    // Any unclosed where-blocks at EOF → P0206 (report the first one we find)
    for stack in &indent_blocks {
        if let Some(b) = stack.last() {
            return Err(s_help(
                b.line, b.col, "P0206",
                "Unclosed condition block before `xx`.",
                "Close the block by dedenting back to the opener’s depth, or remove the stray indented lines.",
            ));
        }
    }
    // Defensive: enforce that any blocks we closed had bodies
    for stack in &indent_blocks {
        for b in stack {
            if !b.had_body {
                return Err(s_help(
                    b.line, b.col, "P0209",
                    "Empty where-block: expected at least one indented condition or command.",
                    "Add a condition (or remove the block):\n  - :where\n  -   status == \"published\"",
                ));
            }
        }
    }

    // Build tree from virtual depths
    let lines = build_tree(flat_nodes)?;

    // infer entity from first level-1 FieldSpec
    let mut entity = entity;
    for n in &lines {
        if n.depth == 1 {
            if let LineKind::FieldSpec { entity: e, .. } = &n.kind {
                entity = e.clone();
                break;
            }
        }
    }
    if entity.is_empty() {
        return Err(s_help(
            hdr_line, 1, "P0113",
            "Missing top-level entity line.",
            "Start the body with the entity:\n  - posts [title, date]\n  xx",
        ));
    }

    Ok(Query { version, action, scope, entity, lines })
}

// ====== Pretty-printer (smoke output) ======

pub fn pretty(q: &Query) -> String {
    let mut s = String::new();
    use std::fmt::Write as _;
    let _ = writeln!(
        s,
        "Query(version={:?}, action={:?}, scope={:?}, entity={})",
        q.version, q.action, q.scope, q.entity
    );
    for n in &q.lines { dump_node(n, &mut s, 0); }
    s
}

fn dump_node(n: &Node, out: &mut String, indent: usize) {
    use std::fmt::Write as _;
    let pad = "  ".repeat(indent);
    match &n.kind {
        LineKind::FieldSpec { entity, fields } => {
            let _ = writeln!(out, "{}- FieldSpec: {} {}", pad, entity, fmt_fields(fields));
        }
        LineKind::Relation { name, fields } => {
            let _ = writeln!(out, "{}- Relation: {} {}", pad, name, fmt_fields(fields));
        }
        LineKind::Command { name, args, is_block } => {
            let block = if *is_block { " (block)" } else { "" };
            let argstr = if let Some(a) = args { format!(" {}", a) } else { "".to_string() };
            let _ = writeln!(out, "{}- Command: :{}{}{}", pad, name, argstr, block);
        }
        LineKind::Comment(text) => {
            let _ = writeln!(out, "{}- Comment: {}", pad, text);
        }
        LineKind::BlockCloseMarker => { /* not printed */ }
    }
    for c in &n.children { dump_node(c, out, indent + 1); }
}

fn fmt_fields(f: &FieldSel) -> String {
    match f {
        FieldSel::Default => "(default)".to_string(),
        FieldSel::All => "[]".to_string(),
        FieldSel::Some(v) => format!("[{}]", v.join(", ")),
    }
}

// ====== Internal parsing utilities ======

struct LineIter<'a> {
    iter: std::str::Lines<'a>,
    idx: usize, // 1-based line number
    buf: Option<(usize, &'a str)>,
}
impl<'a> LineIter<'a> {
    fn new(src: &'a str) -> Self { Self { iter: src.lines(), idx: 0, buf: None } }
    fn next_any(&mut self) -> Option<(usize, &'a str)> {
        if let Some(x) = self.buf.take() { return Some(x); }
        self.iter.next().map(|line| { self.idx += 1; (self.idx, line) })
    }
    fn peek_nonempty(&mut self) -> Option<(usize, &'a str)> {
        loop {
            if let Some((ln, s)) = self.next_any() {
                if s.trim().is_empty() { continue; }
                self.buf = Some((ln, s));
                return self.buf;
            } else { return None; }
        }
    }
    fn next_nonempty(&mut self) -> Option<(usize, &'a str)> { let x = self.peek_nonempty()?; self.buf = None; Some(x) }
}

#[derive(Debug)]
struct RawLine { line_no: usize, content: String }

fn parse_scope(rest: &str, line_no: usize) -> Result<Option<Scope>, DslError> {
    let toks = split_ws_preserving_quotes(rest);
    if toks.is_empty() {
        return Ok(None); // bare `grab` / `fetch` — scope inferred from body
    }

    match toks[0].as_str() {
        "latest" => {
            if toks.len() != 2 {
                return Err(s_help(
                    line_no, 1, "P0115",
                    "Malformed scope: `latest` requires exactly one number.",
                    "Write:\n  grab latest 10\n  - posts [title]\n  xx",
                ));
            }
            let n = toks[1].parse::<u64>().map_err(|_| s_help(
                line_no, 1, "P0115",
                "Malformed scope: expected a number after `latest`.",
                "Write:\n  grab latest 10\n  - posts [title]\n  xx",
            ))?;
            Ok(Some(Scope::Latest(n)))
        }
        "oldest" => {
            if toks.len() != 2 {
                return Err(s_help(
                    line_no, 1, "P0115",
                    "Malformed scope: `oldest` requires exactly one number.",
                    "Write:\n  grab oldest 5\n  - posts [title]\n  xx",
                ));
            }
            let n = toks[1].parse::<u64>().map_err(|_| s_help(
                line_no, 1, "P0115",
                "Malformed scope: expected a number after `oldest`.",
                "Write:\n  grab oldest 5\n  - posts [title]\n  xx",
            ))?;
            Ok(Some(Scope::Oldest(n)))
        }
        "all" => {
            if toks.len() != 1 {
                return Err(s_help(
                    line_no, 1, "P0112",
                    "Unexpected text after `grab`. The entity goes on the first body line.",
                    "Write:\n  grab all\n  - posts [title, date]\n  xx",
                ));
            }
            Ok(Some(Scope::All))
        }
        _ => Err(s_help(
            line_no, 1, "P0112",
            "Unexpected text after `grab`. The entity goes on the first body line.",
            "Write:\n  grab all\n  - posts [title, date]\n  xx",
        )),
    }
}

// Return (dash_depth, indent_after_dashes, content_without_that_indent, col_after_content_start)
fn parse_dash_depth(s: &str, line_no: usize) -> Result<(usize, usize, &str, usize), DslError> {
    let trimmed = s.trim_start();
    let col = s.len() - trimmed.len() + 1; // 1-based
    let mut depth = 0;
    for ch in trimmed.chars() {
        if ch == '-' { depth += 1; } else { break; }
    }
    if depth == 0 {
        return Err(s_help(
            line_no, col, "P0114",
            "Expected `-` to start a body line.",
            "Prefix each body line with dashes:\n  - posts [title]\n  -- comments\n  xx",
        ));
    }
    let after_dashes = &trimmed[depth..];
    let mut indent = 0usize;
    for ch in after_dashes.chars() {
        if ch.is_whitespace() { indent += ch.len_utf8(); } else { break; }
    }
    let content = &after_dashes[indent..];
    Ok((depth, indent, content, col + depth + indent))
}

fn parse_entity_with_fields(s: &str, line_no: usize, col: usize) -> Result<(String, FieldSel), DslError> {
    // Optional explicit-join sugar: `<alias> from <table> [fields]`
    let (head, tail_after_head) = if let Some(idx) = s.find(" from ") {
        (&s[..idx], &s[idx + " from ".len()..])
    } else {
        (s, "")
    };

    let (ident, rest0) = take_ident(head).ok_or_else(|| s_help(
        line_no, col, "P0116",
        "Expected an identifier.",
        "Write the entity or relation name:\n  - posts [title]\n  -- author [username]"
    ))?;
    let rest = rest0.trim_start();

    let mut after_from = tail_after_head.trim_start();
    if !after_from.is_empty() {
        let (_tbl, rem) = take_ident(after_from).ok_or_else(|| s_help(
            line_no, col, "P0116",
            "Expected a target identifier after `from`.",
            "Write:\n  -- author from users [username]"
        ))?;
        after_from = rem.trim_start();
    }

    let tail = if !after_from.is_empty() { after_from } else { rest };
    if tail.is_empty() { return Ok((ident, FieldSel::Default)); }
    if tail.starts_with('[') {
        let close = tail.find(']').ok_or_else(|| s_help(
            line_no, col, "P0711",
            "Unclosed `[` field list.",
            "Close the bracket or remove it:\n  - posts [title, date]\n  - posts               # default fields\n  - posts []            # all fields"
        ))?;
        let inner = &tail[1..close].trim();
        let tail_after = tail[close + 1..].trim();
        if !tail_after.is_empty() {
            return Err(s_help(
                line_no, col, "P0712",
                "Unexpected tokens after `]` in field list.",
                "End the line after the field list:\n  - posts [title, date]",
            ));
        }
        if inner.is_empty() { return Ok((ident, FieldSel::All)); }
        let mut fields = Vec::new();
        for item in inner.split(',') {
            let f = item.trim();
            if f.is_empty() { continue; }
            let name = if f.starts_with('"') && f.ends_with('"') && f.len() >= 2 {
                f[1..f.len()-1].to_string()
            } else { f.to_string() };
            fields.push(name);
        }
        return Ok((ident, FieldSel::Some(fields)));
    }

    return Err(s_help(
        line_no, col, "P0712",
        "Unexpected tokens after identifier.",
        "End the line or add a field list:\n  - posts\n  - posts [title, date]",
    ));
}

fn build_tree(mut flat: Vec<Node>) -> Result<Vec<Node>, DslError> {
    let mut stack: Vec<Node> = Vec::new();
    let mut root: Vec<Node> = Vec::new();

    for n in flat.drain(..) {
        if matches!(n.kind, LineKind::BlockCloseMarker) { continue; }
        while let Some(top) = stack.last() {
            if top.depth < n.depth { break; }
            let popped = stack.pop().unwrap();
            if let Some(parent) = stack.last_mut() {
                parent.children.push(popped);
            } else {
                root.push(popped);
            }
        }
        stack.push(n);
    }

    while let Some(n) = stack.pop() {
        if let Some(parent) = stack.last_mut() {
            parent.children.push(n);
        } else {
            root.push(n);
        }
    }

    Ok(root)
}

fn take_ident(s: &str) -> Option<(String, &str)> {
    let st = s.trim_start();
    if let Some(rest) = st.strip_prefix('"') {
        let end = rest.find('"')?;
        let ident = &rest[..end];
        let rem = &rest[end+1..];
        Some((ident.to_string(), rem))
    } else {
        let mut end = 0usize;
        for ch in st.chars() {
            if ch.is_ascii_alphanumeric() || ch == '_' { end += ch.len_utf8(); } else { break; }
        }
        if end == 0 { return None; }
        let ident = &st[..end];
        let rem = &st[end..];
        Some((ident.to_string(), rem))
    }
}

fn take_number(s: &str) -> (Option<u64>, &str) {
    let st = s.trim_start();
    let mut end = 0usize;
    for ch in st.chars() { if ch.is_ascii_digit() { end += 1; } else { break; } }
    if end == 0 { return (None, s); }
    let n = st[..end].parse::<u64>().ok();
    (n, &st[end..])
}

fn split_ws_preserving_quotes(s: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut buf = String::new();
    let mut in_q = false;
    for ch in s.chars() {
        match ch {
            '"' => { in_q = !in_q; buf.push(ch); }
            c if c.is_whitespace() && !in_q => {
                if !buf.trim().is_empty() { out.push(buf.trim().to_string()); }
                buf.clear();
            }
            _ => buf.push(ch),
        }
    }
    if !buf.trim().is_empty() { out.push(buf.trim().to_string()); }
    out
}