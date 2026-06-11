from pathlib import Path
import re
from collections import defaultdict

ROOT = Path(__file__).resolve().parents[2]
CRATES = ROOT / "crates"
OUT = CRATES / "outputs/DOCUMENTATION_CHECKLIST.md"


def rel(path: Path) -> str:
    return path.relative_to(ROOT).as_posix()


def source_lines(path: Path):
    return path.read_text(encoding="utf-8").splitlines()


def ref(path: Path, line: int) -> str:
    return f"{rel(path)}:{line}"


def validate_ref(value: str):
    match = re.fullmatch(r"(.+):(\d+)", value)
    if not match:
        raise ValueError(f"Malformed reference: {value}")
    path = ROOT / match.group(1)
    line = int(match.group(2))
    if not path.is_file():
        raise ValueError(f"Missing referenced file: {value}")
    if line < 1 or line > len(source_lines(path)):
        raise ValueError(f"Out-of-range reference: {value}")


def parse_parity_entries():
    path = CRATES / "outputs/VM_PARITY_TODO.md"
    entries = []
    current = None
    for line in source_lines(path):
        if line.startswith("* name: "):
            if current:
                entries.append(current)
            current = {"name": line[8:]}
        elif current and line.startswith("  interpreter: "):
            current["interpreter"] = line[15:]
        elif current and line.startswith("  vm: "):
            current["vm"] = [] if line[6:] == "untraced" else [x.strip() for x in line[6:].split(",")]
        elif current and line.startswith("  status: "):
            current["status"] = line[10:]
    if current:
        entries.append(current)
    return entries


def parse_inventory_kinds():
    path = CRATES / "outputs/BUILTINS_FOUND.md"
    result = {}
    current = None
    for line in source_lines(path):
        if line.startswith("* name: "):
            current = line[8:]
        elif current and line.startswith("  kind: "):
            result[current] = line[8:]
    return result


def enum_variants(path: Path, enum_name: str):
    lines = source_lines(path)
    start = next(i for i, line in enumerate(lines) if re.search(rf"\b(?:pub\s+)?enum\s+{re.escape(enum_name)}\b", line))
    depth = 0
    opened = False
    variants = []
    for i in range(start, len(lines)):
        clean = re.sub(r"//.*", "", lines[i])
        if "{" in clean:
            opened = True
        depth += clean.count("{") - clean.count("}")
        if opened:
            match = re.match(r"\s*([A-Z][A-Za-z0-9_]*)\b", clean)
            if match and match.group(1) not in {enum_name}:
                variants.append((match.group(1), ref(path, i + 1)))
        if opened and depth <= 0:
            break
    return variants


def declared_types(path: Path):
    out = []
    for no, line in enumerate(source_lines(path), 1):
        match = re.match(r"\s*pub\s+(enum|struct|trait|type)\s+([A-Za-z_][A-Za-z0-9_]*)", line)
        if match:
            out.append((match.group(2), match.group(1), ref(path, no)))
    return out


def public_functions(path: Path):
    out = []
    for no, line in enumerate(source_lines(path), 1):
        match = re.match(r"\s*pub(?:\([^)]*\))?\s+(?:async\s+)?fn\s+([A-Za-z_][A-Za-z0-9_]*)", line)
        if match:
            out.append((match.group(1), ref(path, no)))
    return out


def named_functions(path: Path, predicate):
    out = []
    for no, line in enumerate(source_lines(path), 1):
        match = re.match(r"\s*(?:pub\s+)?(?:async\s+)?fn\s+([A-Za-z_][A-Za-z0-9_]*)", line)
        if match and predicate(match.group(1)):
            out.append((match.group(1), ref(path, no)))
    return out


def headings(path: Path):
    out = []
    for no, line in enumerate(source_lines(path), 1):
        if re.match(r"^#{1,4}\s+\S", line):
            out.append((line.lstrip("#").strip(), ref(path, no)))
    return out


def item(name, area, audience, refs, scope, status=None):
    refs = list(dict.fromkeys(refs))
    for value in refs:
        validate_ref(value)
    return {
        "name": name,
        "area": area,
        "audience": audience,
        "refs": refs,
        "scope": scope,
        "status": status,
    }


def runtime_items():
    kinds = parse_inventory_kinds()
    out = []
    for entry in parse_parity_entries():
        refs = [entry["interpreter"], *entry.get("vm", [])]
        kind = kinds.get(entry["name"], "runtime behavior")
        out.append(item(
            entry["name"],
            "Language Runtime Reference",
            "language users",
            refs,
            f"Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: {kind}.",
            entry.get("status"),
        ))
    return out


def lexer_items():
    path = CRATES / "goblin-lexer/src/lib.rs"
    out = []
    token_ref = next(r for n, k, r in declared_types(path) if n == "TokenKind")
    for name, vref in enum_variants(path, "TokenKind"):
        out.append(item(f"token kind: {name}", "Lexical Structure", "language users and tooling authors", [vref, token_ref], "Document spelling, lexical boundaries, precedence implications, and invalid forms."))
    for name, fref in named_functions(path, lambda n: n.startswith("lex_") or n in {"lex", "push_token", "skip_ws_and_comments"}):
        label = name.removeprefix("lex_").replace("_", " ")
        out.append(item(f"lexical form: {label}", "Lexical Structure", "language users and tooling authors", [fref], "Document accepted source forms, escapes/separators, edge cases, and diagnostics."))
    return out


def parser_items():
    path = CRATES / "goblin-parser/src/lib.rs"
    keywords = (
        "literal", "bind", "primary", "assign", "coalesce", "interpolation", "return", "action",
        "matrix", "class", "enum", "decl", "if", "unless", "for", "while", "repeat", "collect",
        "attempt", "module", "stmt", "overlay", "link", "unit", "transition", "import", "use",
        "expr", "compare", "range", "additive", "multiplicative", "power", "provoke", "judge",
        "sweep", "unary", "postfix", "member", "args", "definedness", "format",
    )
    funcs = named_functions(path, lambda n: n.startswith("parse_") and any(k in n for k in keywords))
    return [item(
        f"grammar production: {name.removeprefix('parse_').replace('_', ' ')}",
        "Grammar and Syntax",
        "language users and tooling authors",
        [fref],
        "Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.",
    ) for name, fref in funcs]


def type_model_items():
    out = []
    files = [CRATES / "goblin-ast/src/lib.rs", CRATES / "goblin-vm/src/value.rs"]
    for path in files:
        for name, kind, tref in declared_types(path):
            if name == "BuiltinId":
                continue
            area = "AST and Language Data Model" if "goblin-ast" in rel(path) else "Values, Memory, and Collection Backends"
            audience = "language and tooling authors" if "goblin-ast" in rel(path) else "embedders and VM contributors"
            out.append(item(f"{kind}: {name}", area, audience, [tref], "Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences."))
            if kind == "enum":
                for variant, vref in enum_variants(path, name):
                    out.append(item(f"{name}::{variant}", area, audience, [vref, tref], "Document meaning, payload, construction path, and observable behavior."))
    return out


def vm_items():
    out = []
    files = [
        "opcode.rs", "compiler.rs", "vm.rs", "exec.rs", "debug.rs", "error.rs", "session.rs",
        "worker.rs", "grid.rs", "tick.rs", "lib.rs",
    ]
    for filename in files:
        path = CRATES / "goblin-vm/src" / filename
        area = {
            "opcode.rs": "Bytecode and Opcodes", "compiler.rs": "Compiler and Lowering",
            "vm.rs": "VM Execution Model", "exec.rs": "VM Embedding API", "debug.rs": "Debugging and Introspection",
            "error.rs": "VM Errors", "session.rs": "VM Session and State", "worker.rs": "Workers and Swarm Execution",
            "grid.rs": "Grid Runtime", "tick.rs": "DES Tick Runtime", "lib.rs": "VM Crate API",
        }[filename]
        for name, kind, tref in declared_types(path):
            if filename == "grid.rs" and name in {"CellState", "NeighborMode"}:
                pass
            out.append(item(f"{kind}: {name}", area, "embedders and VM contributors", [tref], "Document API contract, invariants, ownership/threading model, failure modes, and examples."))
            if kind == "enum":
                for variant, vref in enum_variants(path, name):
                    out.append(item(f"{name}::{variant}", area, "embedders and VM contributors", [vref, tref], "Document semantics, operands/payload, state transition, and error behavior."))
        for name, fref in public_functions(path):
            if name in {"call_builtin", "builtin_by_name"}:
                continue
            out.append(item(f"API: {name}", area, "embedders and VM contributors", [fref], "Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example."))
    return out


def des_items():
    out = []
    for path in sorted((CRATES / "goblin-des/src").glob("*.rs")):
        for name, kind, tref in declared_types(path):
            out.append(item(f"{kind}: {name}", "DES Architecture", "language users, simulation authors, and contributors", [tref], "Document simulation role, identity/lifecycle, indexing, transition/tick behavior, and runtime integration."))
            if kind == "enum":
                for variant, vref in enum_variants(path, name):
                    out.append(item(f"{name}::{variant}", "DES Architecture", "language users, simulation authors, and contributors", [vref, tref], "Document semantic meaning and effect during DES processing."))
        for name, fref in public_functions(path):
            out.append(item(f"API: {name}", "DES Architecture", "embedders and contributors", [fref], "Document operation, state mutation, complexity/ordering concerns, and failure behavior."))
    return out


def subsystem_items():
    out = []
    configs = [
        ("goblin-diagnostics/src", "Diagnostics and Error Codes", "language users and tooling authors"),
        ("goblin-source/src", "Source Files and Spans", "tooling authors"),
        ("goblin-yall/src", "YALL Data Format", "language users and embedders"),
        ("goblin-gql/src", "GQL Query Language", "language users and embedders"),
        ("goblin-host/src", "Host Embedding", "embedders"),
        ("goblin-devserver/src", "Development Server", "application developers"),
        ("goblin-cli/src", "CLI and Configuration", "CLI users"),
    ]
    for directory, area, audience in configs:
        for path in sorted((CRATES / directory).glob("*.rs")):
            for name, kind, tref in declared_types(path):
                out.append(item(f"{kind}: {name}", area, audience, [tref], "Document purpose, configuration/data model, supported operations, errors, and examples."))
                if kind == "enum":
                    for variant, vref in enum_variants(path, name):
                        out.append(item(f"{name}::{variant}", area, audience, [vref, tref], "Document meaning, accepted inputs, and observable behavior."))
            for name, fref in public_functions(path):
                out.append(item(f"API: {name}", area, audience, [fref], "Document invocation, inputs, outputs, side effects, errors, and example usage."))
            if area == "CLI and Configuration":
                for name, fref in named_functions(path, lambda n: n == "main" or n.startswith(("run_", "cmd_", "print_", "load_"))):
                    out.append(item(f"command/workflow: {name.replace('_', ' ')}", area, audience, [fref], "Document command syntax, flags/environment, files used, exit status, and examples."))
    return out


def explicit_feature_items():
    parser = "crates/goblin-parser/src/lib.rs"
    interp = "crates/goblin-interpreter/src/lib.rs"
    compiler = "crates/goblin-vm/src/compiler.rs"
    lexer = "crates/goblin-lexer/src/lib.rs"
    cli = "crates/goblin-cli/src/main.rs"
    out = [
        item("guarded destructuring / bind guard (`?? =>`)", "Grammar and Syntax", "language users", [f"{parser}:2875", f"{parser}:2963", f"{parser}:3018", f"{interp}:5656", f"{compiler}:450"], "Document syntax, nil/nix trigger semantics, parser lowering, bound-name visibility, guard statement execution, examples, and interactions with coalescing."),
        item("tuple destructuring bindings", "Grammar and Syntax", "language users", [f"{parser}:2633", f"{parser}:2824", f"{interp}:3444", f"{compiler}:544"], "Document tuple targets, arity, tether/retether/shadow modes, errors, object restrictions, and examples."),
        item("repeat map destructuring (`as key, value`)", "Grammar and Syntax", "language users", [f"{parser}:5197", f"{interp}:19576", f"{interp}:19605"], "Document map iteration binding forms, ordering, key/value behavior, legacy form, and examples."),
        item("repeat object-field destructuring", "Grammar and Syntax", "language users", [f"{interp}:19849", f"{parser}:5159"], "Document field-name destructuring during repeat, missing fields, binding scope, and examples."),
        item("block terminators: `xx` and `end`", "Grammar and Syntax", "language users", [f"{lexer}:1205", f"{parser}:1928", f"{parser}:1935", f"{parser}:1941"], "Document that every block form accepts both terminators, placement/layout rules, nested blocks, and missing-terminator diagnostics."),
        item("VM engine selection (`--vm` / `GOBLIN_ENGINE=vm`)", "CLI and Configuration", "CLI users", [f"{cli}:209", f"{cli}:210", f"{cli}:211"], "Document precedence, supported commands, behavioral differences, exit status, and examples."),
    ]
    commands = {
        "goblin new": 153,
        "goblin start": 163,
        "goblin repl": 221,
        "goblin lex --check": 227,
        "goblin parse": 237,
        "goblin gql-parse": 242,
        "goblin run": 332,
        "goblin --help": 136,
        "goblin --version": 130,
    }
    for name, line in commands.items():
        out.append(item(name, "CLI and Configuration", "CLI users", [f"{cli}:{line}"], "Document invocation, arguments/options, environment and files, output, exit codes, and examples."))
    return out


def language_api_items():
    out = []
    configs = [
        (CRATES / "goblin-interpreter/src/lib.rs", "Interpreter Embedding API", "embedders and contributors"),
        (CRATES / "goblin-interpreter/src/modules.rs", "Interpreter Embedding API", "embedders and contributors"),
        (CRATES / "goblin-interpreter/src/diagnostics.rs", "Interpreter Embedding API", "embedders and tooling authors"),
        (CRATES / "goblin-lexer/src/lib.rs", "Lexer and Parser APIs", "tooling authors"),
        (CRATES / "goblin-parser/src/lib.rs", "Lexer and Parser APIs", "tooling authors"),
        (CRATES / "goblin-parser/src/diagnostics_ext.rs", "Lexer and Parser APIs", "tooling authors"),
    ]
    for path, area, audience in configs:
        for name, kind, tref in declared_types(path):
            out.append(item(f"{kind}: {name}", area, audience, [tref], "Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples."))
            if kind == "enum":
                for variant, vref in enum_variants(path, name):
                    out.append(item(f"{name}::{variant}", area, audience, [vref, tref], "Document semantic meaning, payload, and API behavior."))
        for name, fref in public_functions(path):
            out.append(item(f"API: {name}", area, audience, [fref], "Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example."))
    return out


def docs_index():
    out = []
    for path in sorted((ROOT / "docs").glob("*.md")):
        hs = headings(path)
        if hs:
            out.append((rel(path), hs))
    return out


def dedupe(items):
    seen = set()
    out = []
    for entry in items:
        key = (entry["area"], entry["name"], tuple(entry["refs"]))
        if key not in seen:
            seen.add(key)
            out.append(entry)
    return out


def render(items):
    grouped = defaultdict(list)
    for entry in items:
        grouped[entry["area"]].append(entry)
    runtime_count = len(grouped["Language Runtime Reference"])
    areas = sorted(grouped, key=lambda a: (a != "Language Runtime Reference", a))
    lines = [
        "# Goblin Documentation Checklist",
        "",
        "Source-derived checklist for the language, interpreter, VM, DES, tooling, embedding APIs, and repository-specific data formats.",
        "",
        f"Total checklist entries: **{len(items)}**. Runtime parity-derived entries: **{runtime_count}**.",
        "",
        "Every item includes exact repository source anchors. Existing prose documentation is indexed at the end, but source remains authoritative when prose and code disagree.",
        "",
        "## Completion Standard",
        "",
        "An item is complete only when its documentation covers the applicable parts of: purpose, syntax/API, inputs, outputs, aliases, errors, side effects, state/lifecycle, examples, edge cases, and cross-links. User-facing blocks must show both `xx` and `end` terminator forms.",
        "",
        "## Coverage Summary",
        "",
    ]
    for area in areas:
        lines.append(f"- {area}: {len(grouped[area])}")
    for area in areas:
        lines.extend(["", f"## {area}", ""])
        for entry in sorted(grouped[area], key=lambda e: e["name"].lower()):
            refs = ", ".join(f"`{r}`" for r in entry["refs"])
            status = f" Parity status: `{entry['status']}`." if entry.get("status") else ""
            lines.extend([
                f"- [ ] **{entry['name']}**",
                f"  Audience: {entry['audience']}.{status}",
                f"  Source: {refs}",
                f"  Coverage: {entry['scope']}",
            ])
    lines.extend(["", "## Existing Documentation Index", "", "Use this index to reconcile, reuse, or correct existing prose while completing the checklist.", ""])
    for path, hs in docs_index():
        lines.append(f"### `{path}`")
        lines.append("")
        for title, href in hs:
            lines.append(f"- {title}: `{href}`")
        lines.append("")
    return "\n".join(lines).rstrip() + "\n"


def main():
    items = dedupe(runtime_items() + lexer_items() + parser_items() + type_model_items() + vm_items() + des_items() + subsystem_items() + explicit_feature_items() + language_api_items())
    OUT.parent.mkdir(parents=True, exist_ok=True)
    OUT.write_text(render(items), encoding="utf-8")
    counts = defaultdict(int)
    for entry in items:
        counts[entry["area"]] += 1
    print(f"wrote {OUT} with {len(items)} entries")
    for area in sorted(counts):
        print(f"{counts[area]:4}  {area}")


if __name__ == "__main__":
    main()
