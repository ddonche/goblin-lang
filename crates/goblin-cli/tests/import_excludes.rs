//! Import exclusions (`imports.exclude.yall`, `--import-excludes FILE`):
//! excluded files are never imported, so their top level never runs.

use std::path::{Path, PathBuf};
use std::process::{Command, Output};

/// A small project: two modules found by a pattern import, one with a
/// nested import, and a GLAM. Every file says when its top level runs.
fn project(name: &str, extra: &[(&str, &str)]) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("goblin-excl-{}-{name}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    let mut files: Vec<(&str, &str)> = vec![
        ("main.gbln", "import \"modules/*/manifest.imports\"\nuse g\nsay(\"main done\")\n"),
        ("modules/a/manifest.imports", "import modules/a/a as a\n"),
        ("modules/a/a.gbln", "say(\"a loaded\")\n:write_text!(\"a_ran.txt\", \"yes\")\nact fa(ctx)\n    return ctx + \"a,\"\nxx\n"),
        ("modules/b/manifest.imports", "import modules/b/b as b\n"),
        ("modules/b/b.gbln", "import modules/b/inner\nsay(\"b loaded\")\nact fb(ctx)\n    return ctx + \"b,\"\nxx\n"),
        ("modules/b/inner.gbln", "say(\"b inner loaded\")\n"),
        ("glams/g/g.gbln", "say(\"g loaded\")\n"),
    ];
    files.extend_from_slice(extra);
    for (f, src) in files {
        let p = dir.join(f);
        std::fs::create_dir_all(p.parent().unwrap()).unwrap();
        std::fs::write(p, src).unwrap();
    }
    dir
}

fn goblin(dir: &Path, args: &[&str], env: &[(&str, &str)]) -> Output {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_goblin"));
    cmd.arg("run").args(args).current_dir(dir).env_remove("GOBLIN_ENGINE").env_remove("GOBLIN_IMPORT_EXCLUDES");
    for (k, v) in env { cmd.env(k, v); }
    cmd.output().unwrap()
}

fn loaded(out: &Output) -> Vec<String> {
    assert!(out.status.success(), "failed:\n{}", String::from_utf8_lossy(&out.stderr));
    String::from_utf8_lossy(&out.stdout).lines()
        .filter(|l| l.ends_with(" loaded")).map(|l| l.trim_end_matches(" loaded").to_string()).collect()
}

fn run_main(name: &str, extra: &[(&str, &str)], args: &[&str]) -> (PathBuf, Output) {
    let dir = project(name, extra);
    let mut all = args.to_vec();
    all.push("main.gbln");
    let out = goblin(&dir, &all, &[]);
    (dir, out)
}

const ALL: [&str; 4] = ["a", "b inner", "b", "g"];

#[test]
fn no_file_imports_everything() {
    let (_, out) = run_main("none", &[], &[]);
    assert_eq!(loaded(&out), ALL);
}

#[test]
fn empty_list_imports_everything() {
    for (n, body) in [("empty1", "exclude: []\n"), ("empty2", "exclude:\n")] {
        let (_, out) = run_main(n, &[("imports.exclude.yall", body)], &[]);
        assert_eq!(loaded(&out), ALL, "{body}");
    }
}

#[test]
fn exact_file_is_excluded_and_its_top_level_never_runs() {
    let (dir, out) = run_main("exact", &[("imports.exclude.yall", "exclude:\n  - \"modules/a/a.gbln\"\n")], &[]);
    assert_eq!(loaded(&out), ["b inner", "b", "g"]);
    assert!(!dir.join("a_ran.txt").exists(), "a.gbln's top level ran");
}

#[test]
fn directory_is_excluded_with_everything_under_it() {
    for (n, entry) in [("dir1", "modules/b"), ("dir2", "modules/b/**"), ("dir3", "./modules/x/../b/")] {
        let (_, out) = run_main(n, &[("imports.exclude.yall", &format!("exclude:\n  - \"{entry}\"\n"))], &[]);
        assert_eq!(loaded(&out), ["a", "g"], "{entry}");
    }
}

#[test]
fn wildcards_match_like_import_patterns() {
    let (_, out) = run_main("wild", &[("imports.exclude.yall", "exclude:\n  - \"modules/*/a.*\"\n  - \"glams/*\"\n")], &[]);
    assert_eq!(loaded(&out), ["b inner", "b"]);
    let (_, out) = run_main("wild2", &[("imports.exclude.yall", "exclude:\n  - \"**/inner.gbln\"\n")], &[]);
    assert_eq!(loaded(&out), ["a", "b", "g"]);
}

#[test]
fn nested_imports_are_excluded() {
    let (_, out) = run_main("nested", &[("imports.exclude.yall", "exclude:\n  - \"modules/b/inner.gbln\"\n")], &[]);
    assert_eq!(loaded(&out), ["a", "b", "g"]);
}

#[test]
fn other_spellings_and_symlinks_do_not_bypass() {
    let main = "import \"./modules/b/../a/a.gbln\"\nimport \"linked/a.gbln\"\nimport \"modules//a/./a.gbln\"\nsay(\"main done\")\n";
    let dir = project("spell", &[("imports.exclude.yall", "exclude:\n  - \"modules/a/a.gbln\"\n"), ("main2.gbln", main)]);
    #[cfg(unix)]
    std::os::unix::fs::symlink(dir.join("modules/a"), dir.join("linked")).unwrap();
    let out = goblin(&dir, &["main2.gbln"], &[]);
    assert_eq!(loaded(&out), Vec::<String>::new());
    assert!(!dir.join("a_ran.txt").exists());
    // An entry spelled through the symlink excludes the real file.
    std::fs::write(dir.join("imports.exclude.yall"), "exclude:\n  - \"linked\"\n").unwrap();
    let out = goblin(&dir, &["main.gbln"], &[]);
    assert_eq!(loaded(&out), ["b inner", "b", "g"]);
}

#[test]
fn invalid_files_are_clear_errors() {
    let cases = [
        ("bad1", "exclude: \"modules/a\"\n", "`exclude` must be a list of paths"),
        ("bad2", "excludes:\n  - \"modules/a\"\n", "unknown key `excludes`"),
        ("bad3", "exclude:\n  - 3\n", "entry 1 must be a non-empty path"),
        ("bad4", "exclude:\n  - \"modules/*/../a\"\n", "`..` can't follow a wildcard"),
        ("bad5", "- \"modules/a\"\n", "import excludes:"),
    ];
    for (n, body, want) in cases {
        let (_, out) = run_main(n, &[("imports.exclude.yall", body)], &[]);
        let err = String::from_utf8_lossy(&out.stderr);
        assert!(!out.status.success(), "{n}: ran");
        assert!(err.contains("imports.exclude.yall") && err.contains(want), "{n}: got:\n{err}");
        assert!(!String::from_utf8_lossy(&out.stdout).contains("loaded"), "{n}: code ran before the error");
    }
    let (_, out) = run_main("missing", &[], &["--import-excludes", "nope.yall"]);
    assert!(!out.status.success());
    assert!(String::from_utf8_lossy(&out.stderr).contains("`nope.yall` does not exist"));
}

#[test]
fn explicit_file_is_used_and_replaces_the_default() {
    let extra = [
        ("imports.exclude.yall", "exclude:\n  - \"modules/a\"\n"),
        ("portals/x/imports.exclude.yall", "exclude:\n  - \"modules/b\"\n"),
    ];
    let (dir, out) = run_main("explicit", &extra, &["--import-excludes", "portals/x/imports.exclude.yall"]);
    assert_eq!(loaded(&out), ["a", "g"]);
    // After the file name too, and through the environment.
    let out = goblin(&dir, &["main.gbln", "--import-excludes", "portals/x/imports.exclude.yall"], &[]);
    assert_eq!(loaded(&out), ["a", "g"]);
    let out = goblin(&dir, &["main.gbln"], &[("GOBLIN_IMPORT_EXCLUDES", "portals/x/imports.exclude.yall")]);
    assert_eq!(loaded(&out), ["a", "g"]);
    // Without it, the default applies.
    let out = goblin(&dir, &["main.gbln"], &[]);
    assert_eq!(loaded(&out), ["b inner", "b", "g"]);
}

#[test]
fn executions_with_different_exclusions_stay_apart() {
    // Distinct action names: a missing `b::f` would fall back to a bare `f`
    // (a known limitation of action lookup by name, unrelated to exclusions).
    let main = "import \"modules/*/manifest.imports\"\nsay(:summon(\"\", [\"a::fa\", \"b::fb\"]))\n";
    let extra = [
        ("main.gbln", main),
        ("no_a.yall", "exclude:\n  - \"modules/a\"\n"),
        ("no_b.yall", "exclude:\n  - \"modules/b\"\n"),
    ];
    let dir = project("apart", &extra);
    // One after the other: each run sees only its own modules.
    let a = goblin(&dir, &["--import-excludes", "no_b.yall", "main.gbln"], &[]);
    let b = goblin(&dir, &["--import-excludes", "no_a.yall", "main.gbln"], &[]);
    let err_a = String::from_utf8_lossy(&a.stderr).to_string();
    let err_b = String::from_utf8_lossy(&b.stderr).to_string();
    assert!(err_a.contains("unknown action 'b::fb'"), "{err_a}");
    assert!(err_b.contains("unknown action 'a::fa'"), "{err_b}");
    // Started together, many times over: no crossover.
    for _ in 0..10 {
        let spawn = |f: &str| {
            let mut c = Command::new(env!("CARGO_BIN_EXE_goblin"));
            c.args(["run", "--import-excludes", f, "main.gbln"]).current_dir(&dir).env_remove("GOBLIN_IMPORT_EXCLUDES")
                .stdout(std::process::Stdio::piped()).stderr(std::process::Stdio::piped());
            c.spawn().unwrap()
        };
        let (pa, pb) = (spawn("no_b.yall"), spawn("no_a.yall"));
        let (oa, ob) = (pa.wait_with_output().unwrap(), pb.wait_with_output().unwrap());
        let (sa, sb) = (String::from_utf8_lossy(&oa.stdout), String::from_utf8_lossy(&ob.stdout));
        assert!(sa.contains("a loaded") && !sa.contains("b loaded"), "{sa}");
        assert!(sb.contains("b loaded") && !sb.contains("a loaded"), "{sb}");
    }
}

#[test]
fn load_and_eval_ignore_exclusions() {
    let main = "b | :load(\"modules/a/a.gbln\")\nsay(b[0] >> kind)\n:eval(b)\n";
    let (_, out) = run_main("evalok", &[("imports.exclude.yall", "exclude:\n  - \"modules/a\"\n"), ("main.gbln", main)], &[]);
    let so = String::from_utf8_lossy(&out.stdout);
    assert!(out.status.success(), "{}", String::from_utf8_lossy(&out.stderr));
    assert!(so.contains("code\na loaded"), "{so}");
}
