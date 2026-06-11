from pathlib import Path
import hashlib
import json
import re

ROOT = Path(__file__).resolve().parents[2]
INTERP = ROOT / "crates/goblin-interpreter/src"
VM = ROOT / "crates/goblin-vm/src"


def rel(path: Path) -> str:
    return path.relative_to(ROOT).as_posix()


def source_files(base: Path):
    return sorted(base.rglob("*.rs"))


def lines(path: Path):
    return path.read_text(encoding="utf-8").splitlines()


def refs_for(pattern: str, files, flags=0):
    rx = re.compile(pattern, flags)
    out = []
    for path in files:
        for no, line in enumerate(lines(path), 1):
            if rx.search(line):
                out.append(f"{rel(path)}:{no}")
    return out


def enum_variants(path: Path, enum_name: str):
    src = lines(path)
    start = next(i for i, line in enumerate(src) if re.search(rf"\benum\s+{enum_name}\b", line))
    depth = 0
    found_open = False
    out = {}
    for i in range(start, len(src)):
        line = re.sub(r"//.*", "", src[i])
        if "{" in line:
            found_open = True
        depth += line.count("{") - line.count("}")
        if found_open and depth <= 0:
            break
        if not found_open:
            continue
        for name in re.findall(r"\b([A-Z][A-Za-z0-9_]*)\b(?=\s*(?:\([^;]*\))?\s*,)", line):
            out.setdefault(name, f"{rel(path)}:{i + 1}")
    return out


def builtin_name_map(path: Path):
    src = lines(path)
    start = next(i for i, line in enumerate(src) if "pub fn builtin_by_name" in line)
    end = next(i for i in range(start + 1, len(src)) if src[i].startswith("// ") and "Convenience" in src[i])
    text = "\n".join(src[start:end])
    out = {}
    arm = re.compile(r"((?:\s*\"[^\"]+\"\s*\|?)+)\s*=>\s*BuiltinId::([A-Za-z0-9_]+)")
    for match in arm.finditer(text):
        names = re.findall(r'\"([^\"]+)\"', match.group(1))
        line_no = start + text[:match.start()].count("\n") + 1
        for name in names:
            out[name] = {"id": match.group(2), "ref": f"{rel(path)}:{line_no}"}
    return out


def interpreter_free_calls(path: Path):
    src = lines(path)
    start = next(i for i, line in enumerate(src) if i > 18000 and "ast::Expr::FreeCall(name, args" in line)
    end = next(i for i in range(start + 1, len(src)) if "Member/optional member calls" in src[i])
    out = {}
    pending = []
    for i in range(start, end):
        stripped = src[i].strip()
        if stripped.startswith('"') or (pending and stripped.startswith('|')):
            pending.extend(re.findall(r'\"([A-Za-z_:][A-Za-z0-9_:!]*)\"', stripped))
        if "=>" in stripped and pending:
            for name in pending:
                out.setdefault(name, f"{rel(path)}:{i + 1}")
            pending = []
        elif stripped and not stripped.startswith(("|", '"', "//")) and pending:
            pending = []
    return out


def function_match_arms(path: Path, fn_name: str, arm_indent: int = 8):
    src = lines(path)
    start = next(i for i, line in enumerate(src) if re.match(rf"\s*fn\s+{re.escape(fn_name)}\s*\(", line))
    end = next((i for i in range(start + 1, len(src)) if re.match(r"^fn\s+[a-z_]", src[i])), len(src))
    in_dispatch = False
    out = {}
    pending = []
    prefix = " " * arm_indent
    for i in range(start, end):
        line = src[i]
        if not in_dispatch and re.search(r"\blet\s+out(?:\s*:\s*Value)?\s*=\s*match\s+name\s*\{", line):
            in_dispatch = True
        direct_arm = in_dispatch
        if direct_arm and (line.startswith(prefix + '"') or (pending and line.startswith(prefix + "|"))):
            pending.extend(re.findall(r'\"([A-Za-z_:][A-Za-z0-9_:!]*)\"', line.split("=>", 1)[0]))
        if pending and "=>" in line:
            for name in pending:
                out.setdefault(name, f"{rel(path)}:{i + 1}")
            pending = []
        elif pending and line.strip() and not line.startswith(prefix + ("|", '"')):
            pending = []
    return out


def ast_dispatch(path: Path, enum_name: str, start_marker: str, after_line: int):
    src = lines(path)
    start = next(i for i, line in enumerate(src) if i >= after_line and start_marker in line)
    out = {}
    depth = 0
    opened = False
    for i in range(start, len(src)):
        line = src[i]
        if "{" in line:
            opened = True
        depth += line.count("{") - line.count("}")
        for name in re.findall(rf"ast::{enum_name}::([A-Za-z0-9_]+)", line):
            out.setdefault(name, f"{rel(path)}:{i + 1}")
        if opened and depth <= 0:
            break
    return out


def operator_dispatch(path: Path):
    src = lines(path)
    out = {}
    for i, line in enumerate(src, 1):
        if 20400 <= i <= 21824:
            for op in re.findall(r'\"(\+\+|\*\*|//|===|!==|/=|==|!=|<=|>=|&&|\|\||\?\?|\.\.=|\.\.|%o|of|[+\-*/%<>=!])\"', line):
                out.setdefault(op, f"{rel(path)}:{i}")
    return out


def tests():
    all_rs = source_files(ROOT / "crates")
    parity = []
    vm_tests = []
    for path in all_rs:
        text = path.read_text(encoding="utf-8")
        if "#[test]" not in text:
            continue
        refs = refs_for(r"#\[test\]", [path])
        if "goblin_interpreter" in text and ("goblin_vm" in text or "execute_source" in text):
            parity.extend(refs)
        if "goblin-vm" in rel(path) or "goblin_vm" in text or "execute_source" in text:
            vm_tests.extend(refs)
    return {"parity": sorted(set(parity)), "vm_only": sorted(set(vm_tests))}


def fingerprint(paths):
    h = hashlib.sha256()
    total = 0
    manifest = []
    for path in paths:
        data = path.read_bytes()
        h.update(rel(path).encode())
        h.update(b"\0")
        h.update(data)
        total += data.count(b"\n") + (0 if not data or data.endswith(b"\n") else 1)
        manifest.append({"path": rel(path), "lines": len(data.splitlines()), "sha256": hashlib.sha256(data).hexdigest()})
    return {"sha256": h.hexdigest(), "lines": total, "files": manifest}


def main():
    interp_files = source_files(INTERP)
    vm_files = source_files(VM)
    free = function_match_arms(INTERP / "lib.rs", "eval_builtin")
    free.update(function_match_arms(INTERP / "lib.rs", "call_action_by_name"))
    data = {
        "interpreter_fingerprint": fingerprint(interp_files),
        "vm_fingerprint": fingerprint(vm_files),
        "free_calls": dict(sorted(free.items())),
        "statements": dict(sorted(ast_dispatch(INTERP / "lib.rs", "Stmt", "fn eval_stmt(", 3400).items())),
        "expressions": dict(sorted(ast_dispatch(INTERP / "lib.rs", "Expr", "fn eval_expr(", 17700).items())),
        "operators": dict(sorted(operator_dispatch(INTERP / "lib.rs").items())),
        "builtin_names": builtin_name_map(VM / "compiler.rs"),
        "builtin_ids": enum_variants(VM / "value.rs", "BuiltinId"),
        "opcodes": enum_variants(VM / "opcode.rs", "Opcode"),
        "tests": tests(),
        "special": {
            "object method dispatch": "crates/goblin-interpreter/src/lib.rs:21825",
            "object method dispatch with AST args": "crates/goblin-interpreter/src/lib.rs:21967",
            "object instantiation": next(iter(refs_for(r"fn instantiate_object", [INTERP / "lib.rs"])), "UNKNOWN"),
            "member access": "crates/goblin-interpreter/src/lib.rs:18865",
            "optional member access": "crates/goblin-interpreter/src/lib.rs:19062",
        },
    }
    out = ROOT / "crates/work/inventory.json"
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(json.dumps(data, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    print(json.dumps({k: len(v) if isinstance(v, dict) else None for k, v in data.items()}, indent=2))


if __name__ == "__main__":
    main()
