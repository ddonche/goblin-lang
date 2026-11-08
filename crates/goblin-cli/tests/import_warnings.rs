//! What `import` says on stderr: a cycle is skipped with a warning naming
//! its files; a file imported twice without a cycle says nothing.

use std::process::Command;

fn run(name: &str, files: &[(&str, &str)]) -> (String, String) {
    let dir = std::env::temp_dir().join(format!("goblin-imports-{}-{name}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    for (f, src) in files { std::fs::write(dir.join(f), src).unwrap(); }
    let out = Command::new(env!("CARGO_BIN_EXE_goblin")).arg("run").arg("main.gbln").current_dir(&dir)
        .env_remove("GOBLIN_ENGINE").output().unwrap();
    assert!(out.status.success(), "{name}: failed: {}", String::from_utf8_lossy(&out.stderr));
    (String::from_utf8_lossy(&out.stdout).to_string(), String::from_utf8_lossy(&out.stderr).to_string())
}

#[test]
fn import_cycle_warns_with_its_files() {
    let (out, err) = run("cycle", &[
        ("main.gbln", "import a as a\nsay(a::f())\n"),
        ("a.gbln", "import b as b\nact f()\n    return \"a\"\nxx\n"),
        ("b.gbln", "import a as a\nact g()\n    return \"b\"\nxx\n"),
    ]);
    assert_eq!(out.lines().next(), Some("a"));
    assert!(err.contains("warning: import cycle: a.gbln -> b.gbln -> a.gbln"), "got:\n{err}");
}

#[test]
fn diamond_import_says_nothing() {
    let (_, err) = run("diamond", &[
        ("main.gbln", "import l as l\nimport r as r\nsay(l::f() + r::g())\n"),
        ("l.gbln", "import base as base\nact f()\n    return base::v()\nxx\n"),
        ("r.gbln", "import base as base\nact g()\n    return base::v()\nxx\n"),
        ("base.gbln", "act v()\n    return \"x\"\nxx\n"),
    ]);
    assert!(!err.contains("warning"), "got:\n{err}");
}
