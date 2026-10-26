//! tick(regions): a world split into region actors gives the same result
//! whatever the number of worker threads, and on every run.

use std::path::PathBuf;
use std::process::Command;

fn bin() -> PathBuf { PathBuf::from(env!("CARGO_BIN_EXE_goblin")) }

fn run(name: &str, src: &str, workers: usize) -> String {
    let dir = std::env::temp_dir().join(format!("goblin-world-{}-{name}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let file = dir.join("main.gbln");
    std::fs::write(&file, src).unwrap();
    let out = Command::new(bin()).arg("run").arg(&file).current_dir(&dir)
        .env("GOBLIN_ACTOR_THREADS", workers.to_string())
        .env_remove("GOBLIN_ENGINE")
        .output().unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout).to_string();
    assert!(out.status.success(), "{name} failed:\n{stdout}\n{}", String::from_utf8_lossy(&out.stderr));
    stdout
}

/// Four towns of 60 people; flu spreads inside and between towns, and
/// wolves in each town hunt sheep in every town.
const WORLD: &str = r#"
:rand_seed(3)
<>Person |
    health: 1
    town: 0
xx
<>Sheep |
    ~fat: 20
    town: 0
xx
<>Wolf |
    hunger: .5
    town: 0
    margin: .2
    score | decision against Sheep by [
        self >> hunger * .5 + target >> town * .5
    ]
    judge using score
        >= .3: bite(target)
        else: rest(target)
    end
xx
act bite(w, s)
    s >> fat |= s >> fat - 1
xx
act rest(w, s)
    return nil
xx
link Person contact by [
    1 - abs((self >> town) - (target >> town)) * .5
]
overlay Flu |
    hosts Person
    spreads through contact at .5
    decays .04
    modifies:
        health - .01
    end
end
act make_town(t, n, sick)
    things | []
    repeat :range(0, n) as i
        p <> Person | town: t
        if i < sick
            overlay Flu on p at .9
        xx
        :put_last!(things, p)
    xx
    repeat :range(0, 3)
        w <> Wolf | town: t
        :put_last!(things, w)
    xx
    repeat :range(0, 4)
        s <> Sheep | town: t
        :put_last!(things, s)
    xx
    return {"town": t, "things": things}
xx
act town_day(st, msg)
    return st
xx
towns | []
repeat :range(0, 4) as i
    :put_last!(towns, :actor(make_town(i * .25, 60, i % 2), town_day))
xx
repeat :range(1, 13) as d
    tick(towns)
    flu | :len(:overlays(it >> overlay_name == "Flu"))
    fat | 0
    repeat :objects(it >> fat > -1000)
        f | it >> fat
        fat |= fat + f
    xx
    say("tick {d}: flu {flu}, sheep fat {fat}")
xx
"#;

#[test]
fn a_world_is_the_same_on_any_number_of_threads() {
    let one = run("w1", WORLD, 1);
    assert!(one.contains("tick 12:"), "{one}");
    for workers in [2, 4, 8] {
        assert_eq!(one, run(&format!("w{workers}"), WORLD, workers), "{workers} workers differ");
    }
    assert_eq!(one, run("w1b", WORLD, 1), "a second run differs");
}
