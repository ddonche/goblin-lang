from pathlib import Path
import json
import re

ROOT = Path(__file__).resolve().parents[2]
CRATES = ROOT / "crates"
INV = json.loads((CRATES / "work/inventory.json").read_text(encoding="utf-8"))


def read(rel):
    return (ROOT / rel).read_text(encoding="utf-8").splitlines()


VM_FILES = [p for p in (CRATES / "goblin-vm/src").rglob("*.rs")]
VM_LINES = {p.relative_to(ROOT).as_posix(): p.read_text(encoding="utf-8").splitlines() for p in VM_FILES}


def find_vm(pattern):
    rx = re.compile(pattern)
    refs = []
    for path, lines in VM_LINES.items():
        for no, line in enumerate(lines, 1):
            if rx.search(line):
                refs.append(f"{path}:{no}")
    return refs


def enum_ref(table, name):
    return table.get(name)


def camel(name):
    aliases = {
        "str": "ToStr", "to_str": "ToStr", "to_string": "ToStr", "int": "ToInt", "i": "ToInt",
        "float": "ToFloat", "f": "ToFloat", "bool": "ToBool", "big": "ToBig", "b": "ToBig",
        "type_of": "TypeOf", "typeof": "TypeOf", "valtype": "ValType", "vt": "ValType",
        "ask": "AskInput", "input": "AskInput", "repeat_str": "Repeat", "reap": "ReapSample",
    }
    return aliases.get(name, "".join(part.capitalize() for part in name.strip(":").rstrip("!").split("_")))


def builtin_trace(name):
    mapping = INV["builtin_names"].get(name) or INV["builtin_names"].get(name.lstrip(":"))
    if not mapping:
        return None, []
    bid = mapping["id"]
    refs = [mapping["ref"]]
    if bid in INV["builtin_ids"]:
        refs.append(INV["builtin_ids"][bid])
    refs.extend(find_vm(rf"BuiltinId::{re.escape(bid)}\b")[:4])
    return bid, list(dict.fromkeys(refs))


def stub_reason(bid, refs):
    if not bid:
        return "no compiler::builtin_by_name mapping"
    intercepted = {"Invoke", "Summon", "Provoke", "Tick", "QueryByIdent", "Objects", "Overlays"}
    if bid in intercepted:
        return None
    path = "crates/goblin-vm/src/builtins.rs"
    src = VM_LINES[path]
    for i, line in enumerate(src):
        if "NotImplemented" not in line:
            continue
        context = "\n".join(src[max(0, i - 5):i + 1])
        ids = set(re.findall(r"BuiltinId::([A-Za-z0-9_]+)", context))
        if bid in ids:
            return f"VM route is an explicit unsupported stub at {path}:{i + 1}"
    return None


def ast_missing_reason(kind, name, refs):
    if kind == "Stmt" and name == "BoxBind":
        return "compiler explicitly rejects box bind statements at crates/goblin-vm/src/compiler.rs:645"
    if kind == "Expr" and name == "BoxVar":
        return "VM explicitly reports that box_store is unavailable at crates/goblin-vm/src/builtins.rs:3908"
    return None


OP_LOWER = {
    "+": "Add", "++": "Concat", "-": "Sub", "*": "Mul", "/": "Div", "%": "Rem",
    "==": "Eq", "===": "Eq", "!=": "Ne", "/=": "Ne", "!==": "Ne", "<": "Lt",
    "<=": "Le", ">": "Gt", ">=": "Ge", "//": "Div", "**": "Pow", "of": "Mul", "%o": "Mul",
}


def operator_trace(op):
    opcode = OP_LOWER.get(op)
    refs = find_vm(rf'\"{re.escape(op)}\"')[:2]
    if opcode:
        refs.extend(find_vm(rf"Opcode::{opcode}\b")[:6])
    return opcode, list(dict.fromkeys(refs))


def ast_trace(kind, name):
    compiler = find_vm(rf"(?:Stmt|Expr)::{re.escape(name)}\b")
    if not compiler:
        return []
    refs = compiler[:4]
    opnames = []
    for ref in compiler[:2]:
        path, line = ref.rsplit(":", 1)
        src = read(path)
        lo = max(0, int(line) - 2)
        hi = min(len(src), int(line) + 18)
        opnames.extend(re.findall(r"Opcode::([A-Za-z0-9_]+)", "\n".join(src[lo:hi])))
    for op in dict.fromkeys(opnames):
        refs.extend(find_vm(rf"Opcode::{re.escape(op)}\b")[:4])
    return list(dict.fromkeys(refs))


def status_for(refs, missing_reason=None):
    if INV["tests"]["parity"]:
        return "PASS", "genuine interpreter-vs-VM parity test traced"
    if missing_reason:
        return "MISSING", missing_reason
    if refs:
        return "PARTIAL", "VM implementation is traceable, but no cross-engine parity test exists"
    return "MISSING", "no VM implementation could be traced"


def entry(name, kind, interp_ref, vm_refs, status, note):
    return {
        "name": name,
        "kind": kind,
        "interpreter": interp_ref,
        "vm": vm_refs,
        "status": status,
        "note": note,
    }


def build_entries():
    entries = []
    skipped = {"money", "db_query", "db_exec", "db_query_one"}
    for name, iref in INV["free_calls"].items():
        if name in skipped:
            continue
        bid, refs = builtin_trace(name)
        reason = stub_reason(bid, refs)
        status, note = status_for(refs, reason)
        entries.append(entry(name, "builtin/free call", iref, refs, status, note))
    for name, iref in INV["statements"].items():
        refs = ast_trace("Stmt", name)
        status, note = status_for(refs, ast_missing_reason("Stmt", name, refs))
        entries.append(entry(f"Stmt::{name}", "statement", iref, refs, status, note))
    for name, iref in INV["expressions"].items():
        refs = ast_trace("Expr", name)
        status, note = status_for(refs, ast_missing_reason("Expr", name, refs))
        entries.append(entry(f"Expr::{name}", "expression", iref, refs, status, note))
    for op, iref in INV["operators"].items():
        opcode, refs = operator_trace(op)
        status, note = status_for(refs)
        entries.append(entry(f"operator {op}", "operator/runtime", iref, refs, status, note))
    specials = [
        ("object method dispatch", "member/object runtime", INV["special"]["object method dispatch"], r"CallMethod|call_method|member_dispatch"),
        ("object method dispatch with AST args", "member/object runtime", INV["special"]["object method dispatch with AST args"], r"CallMethod|call_method|member_dispatch"),
        ("object instantiation", "member/object runtime", INV["special"]["object instantiation"], r"Instantiate|ClassInstance|Object"),
        ("member access", "member/object runtime", INV["special"]["member access"], r"GetMember|member_dispatch"),
        ("optional member access", "member/object runtime", INV["special"]["optional member access"], r"OptMember|GetMember"),
    ]
    for name, kind, iref, pat in specials:
        refs = find_vm(pat)[:10]
        status, note = status_for(refs)
        entries.append(entry(name, kind, iref, refs, status, note))
    seen = set()
    unique = []
    for item in entries:
        key = (item["kind"], item["name"])
        if key not in seen:
            seen.add(key)
            unique.append(item)
    return sorted(unique, key=lambda x: (x["kind"], x["name"]))


def render_inventory(entries):
    fp = INV["interpreter_fingerprint"]
    out = [
        "# Interpreter Behavior Inventory",
        "",
        "Generated from every Rust source file under `crates/goblin-interpreter/src/**` on branch `claude/quirky-mendel-4df8rm`.",
        f"Interpreter source fingerprint: `{fp['sha256']}` ({len(fp['files'])} files, {fp['lines']} lines).",
        "",
        "The inventory excludes `money`, `db_query`, `db_exec`, and `db_query_one` from parity work by explicit user instruction.",
        "",
        "## Source Manifest",
        "",
    ]
    for item in fp["files"]:
        out.append(f"- `{item['path']}:{item['lines']}` sha256 `{item['sha256']}`")
    out.extend(["", "## Behaviors", ""])
    for item in entries:
        out.extend([
            f"* name: {item['name']}",
            f"  kind: {item['kind']}",
            f"  source: {item['interpreter']}",
        ])
    return "\n".join(out) + "\n"


def render_parity(entries):
    counts = {s: sum(1 for e in entries if e["status"] == s) for s in ("PASS", "PARTIAL", "MISSING", "UNKNOWN")}
    fp = INV["vm_fingerprint"]
    out = [
        "# Interpreter / VM Parity Audit",
        "",
        "Branch: `claude/quirky-mendel-4df8rm`",
        f"VM source fingerprint: `{fp['sha256']}` ({len(fp['files'])} files, {fp['lines']} lines).",
        "",
        "Status policy:",
        "- `PASS`: a genuine test executes equivalent behavior through both interpreter and VM and compares results.",
        "- `PARTIAL`: VM implementation is architecturally traceable, but no genuine parity test exists.",
        "- `MISSING`: no VM implementation can be traced, or the traced VM path is only an explicit unsupported stub for implemented interpreter behavior.",
        "- `UNKNOWN`: source evidence is genuinely indeterminate.",
        "",
        f"Summary: PASS {counts['PASS']}, PARTIAL {counts['PARTIAL']}, MISSING {counts['MISSING']}, UNKNOWN {counts['UNKNOWN']}.",
        "",
        "No genuine cross-engine parity test was found. The VM tests are VM-only smoke/unit tests; representative locations include " + ", ".join(f"`{r}`" for r in INV["tests"]["vm_only"][:8]) + ".",
        "",
        "## Audit Entries",
        "",
    ]
    for item in entries:
        vm = ", ".join(item["vm"]) if item["vm"] else "untraced"
        out.extend([
            f"* name: {item['name']}",
            f"  interpreter: {item['interpreter']}",
            f"  vm: {vm}",
            f"  status: {item['status']}",
            f"  note: {item['note']}",
        ])
    return "\n".join(out) + "\n"


def validate(entries):
    allowed = {"PASS", "PARTIAL", "MISSING", "UNKNOWN"}
    for item in entries:
        if item["status"] not in allowed:
            raise ValueError(f"invalid status for {item['name']}: {item['status']}")
        refs = [item["interpreter"], *item["vm"]]
        for ref in refs:
            match = re.fullmatch(r"(.+\.rs):(\d+)", ref)
            if not match:
                raise ValueError(f"malformed source reference for {item['name']}: {ref}")
            path = ROOT / match.group(1)
            line = int(match.group(2))
            if not path.is_file():
                raise ValueError(f"missing source path for {item['name']}: {ref}")
            if line < 1 or line > len(path.read_text(encoding="utf-8").splitlines()):
                raise ValueError(f"out-of-range source line for {item['name']}: {ref}")


def main():
    entries = build_entries()
    validate(entries)
    outdir = CRATES / "outputs"
    outdir.mkdir(parents=True, exist_ok=True)
    (outdir / "BUILTINS_FOUND.md").write_text(render_inventory(entries), encoding="utf-8")
    (outdir / "VM_PARITY_TODO.md").write_text(render_parity(entries), encoding="utf-8")
    print(json.dumps({s: sum(1 for e in entries if e["status"] == s) for s in ("PASS", "PARTIAL", "MISSING", "UNKNOWN")}, indent=2))


if __name__ == "__main__":
    main()
