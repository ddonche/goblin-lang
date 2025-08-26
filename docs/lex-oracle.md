# Files to add for Step 1

Below are **copy‑paste ready** files to add to your repo.

---

## `docs/lex-oracle.md`

```markdown
# Lex Oracle Format v1

**Goal:** Make `tests/lex/**/*.expect.txt` machine-checkable and easy to read.

- UTF-8 text; `#` starts a comment; blank lines ignored.
- First non-comment line is the **mode**: `OK` (token stream) or `ERR` (error list).
- Optional **directives** (one per line) start with `@`:
  - `@check=kind` (default)
  - `@check=kind+value`
  - `@check=kind+value+span`
- Then **entries** (one per line).

## Token entries (OK mode)

Three forms:

1) **Kind only**
```

IDENT
INT
STRING
NEWLINE
INDENT
DEDENT
EOF

```

2) **Kind with value**
```

IDENT(user)
INT(123)
FLOAT(3.14e10)
STRING("hello\nworld")     # decoded string value
DURATION(5h)
MONEY("\$12.34")
DICE(2d6+1)
PICK(6\_4)
DATE(2025-08-25)
TIME(14:03:00)
DATETIME(2025-08-25T14:03:00Z)
BLOB(len=100)              # simple canonical summary is fine

```

3) **Fixed operators/punct (backticked lexeme)**
```

`=`
`**`
`//`
`|`
`||`
`|>`
`?.`
`??`
`.`
`,`
`(`
`)`
`{`
`}`
`[`
`]`

```

**Optional span suffix** for any entry:
```

IDENT(user) @1:1-1:4
`=`         @1:5-1:5
INT(42)     @1:7-1:8

```

**Canonicalization rules (only if you use `+value`):**
- `INT`/`FLOAT`: no underscores; lowercase `e`.
- `STRING("…")`: decoded content (escapes resolved).
- Other literal kinds: use a simple, consistent textual form (examples above).

## Error entries (ERR mode)

One per line:
```

!unterminated-string @3:1-3:10 : reached EOF while scanning string
!unterminated-block-comment @5:1-7:4
!bad-escape @2:8-2:9 : \q
!bad-unicode-escape @4:12-4:17 : \u12XZ
!tab-indentation @1:1-1:1
!mixed-indentation @6:1-6:5
!unexpected-dedent @9:1-9:1
!numeric-separator-misuse @2:5-2:8 : 1\_\_2
!unknown-byte @10:3-10:3 : \x00

```

- Category names are the contract; the trailing message (after `:`) is optional and matched as a substring.
- Multiple errors appear in the order you expect to report them.

## Examples

**Kinds only (default):**
```

OK
IDENT
`=`
INT
NEWLINE
EOF

```

**Kinds + values + spans:**
```

OK
@check=kind+value+span
IDENT(user)   @1:1-1:4
`=`           @1:5-1:5
STRING("hi")  @1:7-1:10
NEWLINE       @1:10-1:10
EOF           @1:10-1:10

```

**Operators & pipeline carry:**
```

OK
IDENT(x)
`|>`
IDENT(proc)
`(`
`)`
NEWLINE
EOF

```

**Unterminated string:**
```

ERR
!unterminated-string @2:5-2:999 : reached EOF

```
```

---

## `tests/lex/ok/_TEMPLATE.expect.txt`

```text
# Copy this, then fill in token lines.
OK
# Optional: @check=kind+value or +span
# IDENT(name)
# `=`
# INT(123)
# NEWLINE
# EOF
```

---

## `tests/lex/err/_TEMPLATE.expect.txt`

```text
# Copy this, then add one error per line.
ERR
# !category @L:C-L:C : optional message substring
# !unterminated-string @2:5-2:999 : reached EOF
```
