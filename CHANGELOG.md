# Changelog

The Goblin version is the version of `goblin` (crates/goblin-cli). Numbers
in parentheses are the pull requests that made each change.

## Unreleased

- DES: a decision formula on `~` fields (prices, money) is scored against
  the candidates: the best scores 1 and the others in proportion (spread
  worst-to-best when some score is below 0). Before, every such score
  collapsed to 0. The DES town now scores shops on `price`.

## 0.49.0 — 2026-10-26

Everything merged since 0.48.0 (2026-10-06).

### Engine
- The VM is the default engine for `goblin run`, `goblin repl` and
  `goblin start`; `--interp` picks the interpreter (#5).
- A conformance suite runs every case on both engines (#5, #6).
- Errors instead of crashes, and the VM raises errors where it used to stay
  silent (#6).

### Language rulings (VM)
- Percent values follow the percent paper (#7); `??`, bang writes on `imm`,
  `keep_between`, `:pack` and percent printing follow the rulings (#12).
- Grouped imports (#8).
- Object field rules: readonly fields, reserved `id`/`type`, nil and
  missing fields (#9); field defaults like `[]` and `{}` are evaluated
  (#23).
- Durations keep their unit (#11).
- Enum variants with fields (#13).
- Formatted values compare equal (#14).
- One bang rule: `!` changes the real thing or is destructive (#15).
- More than 255 variables per function (#53).
- `repeat` is the fast loop, and `for`/`while` are as fast (#54, #56).
- Nested actions share their enclosing action's variables (#57).
- Class methods can read the program's variables, and a module's methods
  read that module's variables first (#70).

### Concurrency
- `:freeze`, `:swarm`, `:swarm!` (#16).
- Actors and bounded channels (#24); a startup script for `goblin start`
  whose actors live with the server (#25).
- Lightweight actors on a worker pool; a waiting handler gives up its
  thread (#29). Actors share what does not change: 1,000 actors went from
  5.2 GB to 120 MB (#32).
- DB and HTTP calls in actors no longer block the worker (#34).
- `:cancel`, and shutdown that cancels after a grace period (#35, #41).
- An actor failure nobody asked about is written to stderr (#36).
- `:receive()` inside an actor (#37); `restart: "on_failure"` (#38).
- Swarm copies less: frozen history and short texts are shared (#50, #62).

### Simulation (DES)
- The DES scripts run on the VM with the interpreter's output (#42).
- DES runs inside an actor, taking only its own objects (#64).
- `tick(regions)`: one DES tick of a world split into region actors (#65).
  A region's state stays alive during the tick, and an object posted to
  another region arrives as an independent copy (#73).
- `every N ticks` blocks in classes (#68).
- Several named decisions per class (`name | decision ...`,
  `judge using name`) (#69).
- Looping over `:overlays()` no longer wipes out the overlays' hosts (#67).
- Built-in event queue: `:agenda()`, `:schedule!`, `:unschedule!`,
  `:due!` (#51).

### Speed
- Plain arithmetic 2.3x faster; values shrank from 120 to 32 bytes;
  strings are shared instead of copied (#19–#22).
- Fast sweep search (#21), regex cache (#26), faster string building
  (#27), `goblin start` hands its response to the server directly (#28).
- Imports resolve once per request (#39); `x |= x + piece` adds in place
  (#40).
- Field reads and writes: no copy for `obj >> field` (#45), nested field
  writes and large-map lookups (#49), field paths in one step (#52),
  and less work per field update (#58, #61).
- Values are handed over instead of copied (#59); common steps run right
  in the run loop (#60, #63).
- DES formulas compile once (#44) and simple ones run without a call frame
  (#47): europe_sim 8–11x faster.
- `pick` from a range no longer builds the range (#71); DES decision
  scoring and `:objects` queries are faster (#72). The DES town sim: one
  year on 4 threads went from 132 s to 15 s.

### Fixes
- Memory cleanup inside nested runs; closures keep their variables
  (#17, #18).
- A failed nested run drops what it left on the stack, so memory stays
  flat over long DES runs (#46).
- Small fixes from review (#33, #43).

### Sims
- Town sim on district actors, deterministic (#48); every loop uses
  `repeat` (#55); rebuilt on DES across districts (#66).
