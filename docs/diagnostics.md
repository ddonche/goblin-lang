# Goblin Diagnostics — Style Guide (v1)

**Purpose**: Keep diagnostics consistent across the toolchain (lexer → parser → interpreter → CLI). This guide defines tone, structure, required fields, and span rules.

---

## 1) Anatomy of a Diagnostic

A diagnostic is a structured message with:

* **severity**: `error` | `warning` | `note` | `help`
* **category** (slug): short, stable id (e.g., `bad-escape`, `unterminated-string`, `unexpected-dedent`).
* **message**: one‑line summary (no trailing period).
* **primary\_span**: the main location of the issue.
* **secondary\_spans**: zero or more labeled spans that add context.
* **notes**: optional extra context (each becomes its own line).
* **help**: optional actionable suggestions (each becomes its own line).

All of these fields are represented by the `goblin-diagnostics` crate and should be filled by producers (lexer/parser/etc.).

---

## 2) Tone & Wording

* **Direct and specific.** Prefer concrete language over vague phrasing.
* **Neutral voice.** No blame, no jokes, no passive‑aggressive tone.
* **Actionable.** When possible, suggest how to fix (`help`) or why it happened (`note`).
* **One idea per line.** Keep the primary message short; push detail into notes/help.
* **No trailing period** on the main message. Sub-items may have periods if they’re sentences.

**Good**

* `error: bad-escape: invalid escape sequence \q`
* `error: unterminated-string: reached EOF while scanning string`

**Avoid**

* `error: you messed up your string` (blamey)
* `error: invalid` (vague)

---

## 3) Spans & Positions

* **1-based lines and columns.**
* **Half‑open byte ranges** `[start, end)` for internal use; the CLI prints human positions.
* **Primary span** should cover the smallest slice that expresses the issue.
* **Secondary spans** (with optional labels) point to related locations.
* When the exact location is unknown (e.g., unexpected EOF), use the closest stable location and mention uncertainty in a note.

Example primary span use:

* `bad-escape` → span only the bad escape `\q`.
* `unexpected-dedent` → span the first column of the offending line.

---

## 4) Required Output Shape (first lines)

Renderers should start with:

```
<severity>: <category>: <message>
  ┌─ <path>:<line>:<col>
```

Then, if available, show source context and annotations (the CLI may add this later). Finally, print `help` and `note` items:

```
  = help: try "\\n", "\\t", or remove the backslash
  = note: escapes are not processed inside raw strings
```

Formatting details:

* Severity is lowercase (`error`, `warning`, `note`, `help`).
* Category is the stable slug; **do not** include spaces.
* Message is a single line; no trailing period.
* Each `help` or `note` line starts with `= help:` or `= note:`.

---

## 5) Category Slugs

* Short, lowercase, kebab‑case.
* Stable over time (treat as public identifiers).
* Examples used in v1 fixtures:

  * `bad-escape`
  * `bad-unicode-escape`
  * `unterminated-string`
  * `unterminated-block-comment`
  * `tab-indentation`
  * `mixed-indentation`
  * `unexpected-dedent`
  * `numeric-separator-misuse`
  * `unknown-byte`

---

## 6) Multi-Diagnostic Policies

* Emit diagnostics in **source order** by primary span, then by category.
* Avoid cascading noise: prefer one precise diagnostic over many redundant ones.
* If later phases depend on earlier success, they may short‑circuit after `error`s and add a `note` explaining why.

---

## 7) Examples

### Unterminated string

```
error: unterminated-string: reached EOF while scanning string
  ┌─ tests/lex/err/err_unterminated_string_basic.gbln:2:5
  = help: add a closing quote to terminate the string
```

### Bad escape

```
error: bad-escape: invalid escape sequence \q
  ┌─ tests/lex/err/err_bad_escape_sequence.gbln:3:8
  = help: use \\n, \\t, \\"", \\' or a unicode escape like \\u{1F600}
  = note: escapes are inert in raw strings
```

### Unexpected dedent

```
error: unexpected-dedent: dedent does not match any open block
  ┌─ tests/lex/err/err_unexpected_dedent.gbln:9:1
  = note: previous indent levels: 0, 4
  = help: align to a previous indent level (e.g., 4 spaces)
```

---

## 8) Do & Don’t (quick checklist)

**Do**

* Fill **severity**, **category**, **message**, **primary\_span**.
* Add **secondary spans** for cross‑references, with labels.
* Add at least one **help** or **note** when a fix is obvious.

**Don’t**

* Don’t end the main message with a period.
* Don’t put file paths into the message; use spans and let the renderer show paths.
* Don’t change category slugs once published.
