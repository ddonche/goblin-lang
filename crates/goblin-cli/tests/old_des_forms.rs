//! The DES forms the 2026-11-01 syntax pass replaced are errors that name
//! the new form.

use std::process::Command;

fn error_of(name: &str, src: &str) -> String {
    let dir = std::env::temp_dir().join(format!("goblin-old-des-{}-{name}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let file = dir.join("main.gbln");
    std::fs::write(&file, src).unwrap();
    let out = Command::new(env!("CARGO_BIN_EXE_goblin")).arg("run").arg(&file).current_dir(&dir)
        .env_remove("GOBLIN_ENGINE").output().unwrap();
    assert!(!out.status.success(), "{name}: the old form ran");
    String::from_utf8_lossy(&out.stderr).to_string()
}

#[test]
fn old_des_forms_are_errors_that_name_the_new_form() {
    let cases: &[(&str, &str, &str, &str)] = &[
        ("decision", "<>F |\n    hp: 1\n    score | decision against F by [ 1 ]\nxx\n", "P1490", "decision prey"),
        ("one_of", "<>F |\n    hp: 1\n    one of a, b\nxx\n", "P1593", "judge"),
        ("order_by", "<>F |\n    hp: 1\n    order by [ @hp ] desc\nxx\n", "P1592", "order: -@speed"),
        ("on_erase", "<>F |\n    hp: 1\n    on erase: fall\nxx\n", "P1591", "on erase => "),
        ("transition", "<>F |\n    hp: 1\n    transition erase when @hp <= 0\n    end\nxx\n", "P1590", "transition erase where"),
        ("link_class", "<>N |\n    m: 1\nxx\nlink N border 0...1 by [ 1 ]\n", "P1390", "hosts: Nation"),
        ("link_to", "<>N |\n    m: 1\nxx\na <> N | m: 1\nb <> N | m: 1\na link to b on trust offset 1\n", "P1391", ":link!(a, b, trust, 3)"),
        ("object_decision", "<>N |\n    m: 1\nxx\na <> N | m: 1\na score | decision against N by [ 1 ]\n", "P1491", "@taste"),
        ("overlay_on", "<>N |\n    m: 1\nxx\noverlay flu\n    hosts: N\nxx\na <> N | m: 1\noverlay flu on a\n", "P1290", ":overlay!(host, x)"),
        ("detach_from", "<>N |\n    m: 1\nxx\noverlay flu\n    hosts: N\nxx\na <> N | m: 1\ndetach flu from a\n", "P1291", ":detach!(host, x)"),
    ];
    for (name, src, code, hint) in cases {
        let err = error_of(name, src);
        assert!(err.contains(code), "{name}: expected {code}, got:\n{err}");
        assert!(err.contains(hint), "{name}: expected the new form `{hint}`, got:\n{err}");
    }
}
