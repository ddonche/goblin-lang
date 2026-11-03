//! `phase:` blocks and the `tick()` block: what a mistake in them says.

use std::process::Command;

fn error_of(name: &str, src: &str) -> String {
    let dir = std::env::temp_dir().join(format!("goblin-phase-{}-{name}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let file = dir.join("main.gbln");
    std::fs::write(&file, src).unwrap();
    let out = Command::new(env!("CARGO_BIN_EXE_goblin")).arg("run").arg(&file).current_dir(&dir)
        .env_remove("GOBLIN_ENGINE").output().unwrap();
    assert!(!out.status.success(), "{name}: it ran");
    String::from_utf8_lossy(&out.stderr).to_string()
}

#[test]
fn phase_mistakes_are_errors() {
    let cases: &[(&str, &str, &str)] = &[
        ("nested", "<>F |\n    hp: 1\n    phase: a\n        phase: b\n            every 1 tick\n                @hp |= 2\n            xx\n        xx\n    xx\nxx\n", "P1580"),
        ("field_in_phase", "<>F |\n    hp: 1\n    phase: a\n        act hit()\n            @hp |= 2\n        xx\n    xx\nxx\n", "P1581"),
        ("twice", "<>F |\n    hp: 1\n    phase: a\n        every 1 tick\n            @hp |= 2\n        xx\n    xx\n    phase: a\n        every 1 tick\n            @hp |= 3\n        xx\n    xx\nxx\n", "P1582"),
        ("overlay_line_in_phase", "<>F |\n    hp: 1\nxx\noverlay flu\n    hosts: F\n    phase: a\n        lasts: 3\n    xx\nxx\n", "P1581"),
        ("unknown_phase", "<>F |\n    hp: 1\n    phase: grow\n        every 1 tick\n            @hp |= 2\n        xx\n    xx\nxx\ntick()\n    rain\nxx\n", "R1250"),
        ("tick_args", "tick(1)\n", "arity"),
    ];
    for (name, src, code) in cases {
        let err = error_of(name, src);
        assert!(err.contains(code), "{name}: expected {code}, got:\n{err}");
    }
}
