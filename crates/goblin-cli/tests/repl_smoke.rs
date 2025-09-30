use std::{fs, process::{Command, Stdio}, path::PathBuf};

fn run(code: &str) -> String {
    // Spawn goblin CLI and feed code to stdin
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_goblin"));
    let mut child = cmd.stdin(Stdio::piped()).stdout(Stdio::piped()).spawn().unwrap();
    {
        use std::io::Write;
        let stdin = child.stdin.as_mut().unwrap();
        stdin.write_all(code.as_bytes()).unwrap();
        // send EOF
    }
    let out = child.wait_with_output().unwrap();
    String::from_utf8_lossy(&out.stdout).to_string()
}

#[test]
fn json_roundtrip() {
    let out = run(
        r#"
        x = {a: 1, b: [true, "hi"]}
        s = json_stringify(x)
        y = json_parse(s)
        say y.b[1]
        "#,
    );
    assert!(out.contains("hi\n"));
}

#[test]
fn read_write_json_file() {
    let tmp = tempfile::tempdir().unwrap();
    let path = tmp.path().join("x.json").to_string_lossy().to_string();
    let script = format!(r#"
        x = {{name: "Ada", nums: [1,2,3]}}
        write_json!("{}", x)
        y = read_json("{}")
        say y.name
    "#, path, path);
    let out = run(&script);
    assert!(out.contains("Ada\n"));
}
