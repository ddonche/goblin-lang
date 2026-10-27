# Changelog

The Goblin version is the version of `goblin` (crates/goblin-cli). Numbers
in parentheses are the pull requests that made each change.

## Unreleased

- DES (VM): faster when objects are compared by `owner`. `target >> owner
  == @owner` compares the two ids without looking either object up;
  spreads, decisions and `:objects(..., it >> owner == x >> owner)` find
  the matches through the store's field index (objects key by id); a
  decision against a class its holder holds little of checks every target
  instead of going through all the holder holds; a region no longer
  re-files every visiting object under its owner each tick. Epidemic on
  `owner` 1.85G -> 1.30G instructions (30 days), predator-prey on `owner`
  3.64G -> 2.75G (20 days); the other sims unchanged.
- Concurrency (VM): `:actor(meadow)` with no handler. The actor runs the
  meadow and what it holds in `tick(regions)` and takes no messages
  (`:post`/`:request` to it is C0107). Sims no longer need a handler that
  only returns its state.
- Syntax: `@name` is `self >> name`, anywhere `self` exists (class
  actions, `every`/`when` blocks, decisions, transitions, overlays, links,
  sweeps): reads, `@hp |= ...`, `@home >> food`, `@name.upper()`,
  `@tags[0]`, `&@hp` and `"{@hp}"`. Outside those it is error P0919.
  A bare name in a class action is no longer the object's field: it is a
  variable, and reading one that is only a field is error R0122 ("did you
  mean `@hp`?"). A method or `every` block that does not compile now says
  why when it runs, instead of going missing. The sims use `@`.
- DES: `~` before a field that starts as something other than a number
  (`~name: "Ash"`, `~owner: nil`, `~tags: []`) is a parse error, P0917.
  `~` only keeps a number as its real value instead of a 0..1 trait, so on
  any other field it did nothing. The sims, tests and benches drop it
  there.
- Concurrency + DES (VM): ownership works across actors. An actor given a
  meadow (`:actor(meadow, handler)`) runs the meadow and everything it
  holds; setting an object's `owner` to a meadow another actor runs (or to
  something that meadow holds) sends the object there, with what it holds,
  its overlays and links, as `:move!` does. The main program can set
  `owner` too, and an object it makes for a meadow goes to that meadow's
  actor. Before, the object stayed in its old actor (and one the main
  program made was never ticked).
- DES (VM): `:objects(...)` with a predicate works out once per predicate
  what running it directly needs, instead of on every call, and
  `:objects("Class", it >> owner == x)` reads `x` without calling the VM.
  Same results. Feudal sim, 40 years, 1 thread: 2.85 -> 2.51 billion
  instructions (-12%).
- DES (VM): a decision's `where [ target >> owner >> owner == x ... ]` (an
  estate of another vassal of my liege) looks only at what the things `x`
  holds hold, as `target >> owner == x` already looked only at what `x`
  holds. Same results; a chain broken by nil still stops the tick with its
  error. Feudal sim, 60 years, 1 thread: 6.6 -> 4.5 billion instructions.
- DES (VM): a decision's `where [ target >> pen == self >> pen ... ]` and
  `:objects("Sheep", it >> pen == p)` find the objects through an index of
  that field, kept up to date as objects change, instead of checking every
  object of the class (classes of 32 objects or more). Results are the same.
  Predator-prey sim, 360 days, 1 thread: 28 s -> 19 s.
- DES (VM): overlays spread much faster with `spreads where`. The condition
  is compiled once and reads objects where they live, only the classes the
  overlay `hosts` are tried (it used to try every object, so a class it may
  not go on could catch it), and a part like `target >> town == self >> town`
  tries only the objects whose field matches. Simple formulas, `where`
  conditions and `:objects` predicates also run faster. Epidemic sim, 4,000
  people, 60 days, 1 thread: 141 s -> 19 s.
- Strings: a `{...}` placeholder can hold text in quotes and nested braces:
  `say("estates {:len(:objects("Estate"))}")` and `say("{r{"winner"}} wins")`
  work (the inner quote used to end the string, and the first `}` ended the
  placeholder). Only a placeholder that starts like an expression (a name
  or `:builtin`) and closes on the same line is read this way; other braces
  are unchanged.
- Regions (VM): a field naming an object another region holds keeps its id
  when the objects cross between regions (snapshots, changes, moves). The
  program running the world reads through it (`lord >> owner >> name`
  used to read nil there), and so does a region during a tick. Once the
  object is erased the field reads nil, as before.
- VM: a variable given an object from a field (`x |= x >> owner`) or from
  another variable (`y | x`) holds that object by id instead of a copy
  written back into the store; a field read followed by another field
  read, a comparison or an action call does the same, and `==` / `!=` on
  two objects compares ids without copying either. Walking 30 owners
  20,000 times: 650 ms to 460 ms. Feudal sim: 3% fewer instructions.
- DES (VM): while a decision scores its targets, an action that only reads
  (no writes outside its own variables, nothing random, no output, and only
  calls to other such actions) runs once per set of arguments: in
  `where [ strength(self) >= 2 * strength(target >> owner) ]`,
  `strength(self)` runs once and `strength(target >> owner)` once per
  holder, not once per target. A condition that calls such actions can now
  narrow the targets too (`estates(self) >= 2` is checked once). Results
  are the same; actions that write or print still run every time. Feudal
  sim, 200 years on 4 threads: 5.2 s to 3.7 s.
- DES (VM): a decision's `where`, score and other formulas can call the
  program's actions and read its variables (`where [ kingdom(target) == 2 ]`
  was an R1220 "undefined variable" error).
- DES (VM): a decision whose `where` has `target >> owner == x` (alone or
  joined by `and`) looks only at what `x` holds, through its contents list;
  a leading condition that doesn't name `target` (`self >> gold < 0`) is
  checked once instead of per target. Only read-only conditions are used
  this way, so the result is the same. 300 lords rewarding their 3,000
  knights for 10 ticks: 11.5 s to 0.09 s.
- Regions (VM): objects cross between regions by id. An object that moved to
  another region used to carry copies of the objects its fields named (its
  owner, its owner's owner, ...); writing through such a copy wiped the real
  object's changes there. Changes and snapshots send ids the same way.
- Ownership (VM): what an object contains goes with it. Handing it to an
  actor (`:actor`) and moving it to another region (`:move!`) take along
  everything whose `owner` names it, at any depth, and what that refers to.
  A person with belongings used to fail with C0113 when handed to a region,
  and a moved container left its contents behind.
- Ownership (VM): an object's `owner` field holds the object that contains
  it, and the store keeps each owner's contents list. Writing `owner` moves the
  object (a transfer; a drop is giving it to a place). `:objects("Class", it >>
  owner == x)` (alone or as one side of `and`) reads that list instead of
  searching every object, and so do `owned_by` and `owns_tree`: 10,000 lookups
  over 20,000 items went from 684 s to 0.76 s. When an owner is erased, its
  `on erase:` runs first (now for `delete_object!` too), and whatever it still
  contains moves up to its own owner (nil if it has none).
- Regions (VM): the program that runs `tick(regions)` can change the regions'
  objects. A field write, an overlay put on or taken off, `delete_object!` and
  `delete_overlays_on!` show in the program's view at once and go to the
  object's region, which applies them at the start of the next tick (they used
  to fail with C0124). Inside a region, `delete_object!`, `delete_overlays_on!`
  and `detach` on another region's object go to its owner the same way. A
  change still on its way to a region left out of the next `tick(regions)` list
  is an error (C0123).
- DES: a decision can leave out `by [ ... ]` (in a class and on one object);
  every target that passes its `where` then scores 1. The civilization and
  predator-prey sims drop their `by [ 1 ]`, same output.
- DES (VM): in a `one of` group, an action that returns `false` didn't act, so
  the next decision in the group gets its turn. Any other result (nil included)
  still ends the group.
- DES (VM): a decision's score is exactly the number its formula gives. The
  runtime no longer rescales scores (over the candidates for `~` fields, over a
  range guessed from the formula otherwise) or clamps them to 0..1. `margin` is
  in the formula's own units and the judge compares against the same number.
  Programs written against the old 0..1 scale need their thresholds and margins
  in the formula's units (the town's `margin .8` is now `margin 10` with
  `where [ target >> open == 1 ]`, same output).
- Overlays (VM): every overlay is kept in one list with its host's and its
  name's places, updated as overlays are put on, renamed and taken off.
  `:overlays("Sick")` gives one overlay's instances and `:overlays("Sick", pred)`
  those the predicate holds for (an unknown name is R1231); `repeat Sick`,
  `:overlays_of`, `:overlay_strength` and the DES tick's overlay lookups
  (spreading, conflicts, spawning, stacking) read those lists instead of every
  overlay or tables rebuilt each tick. Conflicts between overlays now go by the
  host object, not the name of its variable. bench/des/overlay_lookups.gbln at
  20,000 objects: 16.1 s to 0.67 s.
- Objects (VM): the object store keeps each class's objects in one place,
  updated as objects are made and erased. `:objects("Rabbit")` gives a class's
  objects and `:objects("Rabbit", pred)` those the predicate holds for, without
  looking at every object; `:is_type(r, "Rabbit")` is true for a Rabbit; an
  unknown class name is R1230. `repeat Rabbit` and the DES tick's class lookups
  (decisions, `every` blocks, transitions, `spreads to`) read the same lists.
  Predator-prey 200 days: 7.3 s to 5.6 s.
- Objects (VM): a field given an object holds that object, not a copy, like a
  database entry. `p >> home >> food |= 5` changes the village; a change to the
  village shows through `p >> home`; the field reads nil once the object is
  erased. An object equals itself (any copy of it), as on the interpreter.
- DES: a decision formula on `~` fields (prices, money) is scored against
  the candidates: the best scores 1 and the others in proportion (spread
  worst-to-best when some score is below 0). Before, every such score
  collapsed to 0. The DES town now scores shops on `price`.
- DES: `margin N` on a decision (`shop | decision against Business margin .8 by [...]`)
  sets that decision's margin; without it the object's `margin` field is used.
- DES (VM): a decision written for one object (`x score | decision against ...`)
  now applies, to the object `x` holds when it is written (also through another
  name for it, or for objects made inside an action); before, the VM never found it.
- DES: an error inside a decision's action stops the tick (it was ignored), as
  in an `every` block.
- DES (VM): a decision's judge can call an action written inside the class
  (`act govern(target)`); the VM only found program-level actions and silently did nothing.
  The action gets `self`, then the arguments the arm writes (`purchase(target, 3)`,
  `rest()`); before, it always got just the target.
- VM: `repeat 3 as d` gives a clear error (a count has no items to name; use `idx`
  or `repeat :range(0, 3) as d`) instead of "local slot 0 is uninitialized".
- Parser: a `:builtin(...)` argument can use `and`, `or` and `??`
  (`:objects(it >> age > 18 and it >> district == d)`); it stopped at comparisons.
- DES (VM): scoring a decision does less work per candidate (no per-score
  lists, no id lookup for objects the tick already knows): about 14% fewer
  instructions for the DES town, same output.
- DES (VM): in `tick(regions)`, each region (and the main program's view)
  keeps its copies of the other regions' objects between ticks and rebuilds
  only the fields that changed, instead of every object every tick. DES town
  30 days: 1 thread 2.35 -> 1.78 s, 4 threads 0.80 -> 0.65 s; same output.
- DES: an overlay can say `on end: action`; when it runs out (its time is up or
  its strength reaches 0) the action runs with the host (`give_birth(mom)`, or the
  host class's own `act give_birth()`). Taking it off with `detach` runs nothing.
- DES: a class can say `on erase: action`; it runs on the object just before an
  erase transition removes it, while its fields can still be read (an estate
  goes to the heirs before the person is gone).
- DES: `:move!(obj, region)` moves an object to another region of
  `tick(regions)`. In a region it leaves at once (reading it there afterwards
  is C0125) and arrives at the start of the next tick with its overlays, link
  offsets and decision; what other regions send it meanwhile follows it. The
  main program sees it the whole time and can move an object too: its region
  moves it at the start of the next tick.
- DES town (`sims/town_des`): babies are born (overlay `Pregnant` with
  `on end:`), a resident's money goes to their district's fund when they die
  (`on erase:`), and adults now and then move district (`:move!`).
- New sim `sims/rpg_combat`: a party against monsters on DES, 1,000 battles
  over `:swarm`; `FINDINGS.md` lists what got in the way.
- DES: when a timed overlay ends, it takes back only what its `modifies:` added.
  Before, every field it modified went back to its value from before the
  overlay, so other changes meanwhile (damage taken, a second overlay) were lost.
- DES: a decision formula that fails, or gives something other than a number,
  stops the tick with R1220 naming the decision. Before, the target was
  skipped (or scored 0.5) and the decision silently did nothing. A type error
  on `*`, `-` and the like now names the side that is not a number.
- DES: a decision can leave targets out before scoring with `where [ condition ]`:
  `heal | decision against Fighter where [ target >> side == self >> side ] by [ ... ]`.
- DES: `one of heal, attack` in a class makes decisions alternatives: they are tried in
  that order each tick and the first whose judge runs an action ends the group.
- DES: overlays take `each tick:` changes (`hp - 2`), made to the host every tick the
  overlay is on and never taken back; `modifies:` stays temporary. A failing change is R1221.
- DES: `order by [ self >> speed ] desc` in a class sets the order its objects make their
  decisions in (ascending by default; other classes keep their places). A bad key is R1222.
- DES: erase transitions are checked as soon as an action that changed the object finishes
  (decision actions, `every` blocks, `on end:`, `each tick:` changes), so the dead don't act or get chosen again that
  tick. A condition that draws a random number stays a once-per-tick check at the end.
- DES: `:overlay_strength(name, host)` and `:overlays_of(host)` take the host object itself
  (the variable's name still works), so they work on objects made inside actions.
- Strings fill in expressions: `"{n - wins} left"`, `"{lost[0]}"`, `"{p >> hp * 2}"`, when every name in
  them is a variable in scope and it reads one or calls something. Anything else in braces (CSS, unknown names,
  regex `{1,3}`) stays as written.
- `:round(x, n)` keeps n digits after the point (`:round(3.14159, 2)` is 3.14); a negative n
  rounds to tens, hundreds, ... `:round(x)` is unchanged.
- DES: `overlay X on host at s for n ticks` takes expressions for the strength and the ticks
  (`at a >> power / 10 for rounds + 1 ticks`); a bad value is R1223.
- `sims/rpg_combat` uses the new DES features (`where`, `one of`, `order by`, `each tick:`, erase after
  actions, overlay builtins on objects, `{expr}`, `:round(x, n)`); FINDINGS.md has the status.
- Faster: `:objects(...)` predicates and DES formulas compare strings on the fast path
  (`it >> kind == "rabbit"` no longer runs the predicate as a call per object), and copying an
  object no longer copies its read-only and trait field sets.
- `sims/predator_prey`: rabbits, foxes and grass over four meadows (regions); the populations
  cycle. `check.sh` and FINDINGS.md.
- `sims/epidemic`: a sickness spreading through four towns (regions), with death, immunity and
  travel. `check.sh` and FINDINGS.md.
- `sims/civilization`: four villages (regions) whose people farm, cut wood, build huts, have
  children, die, move on when crowded, and send food to a hungry neighbour. `check.sh` and
  FINDINGS.md.
- DES: `spreads ... at rate` treats the rate as a chance. Each tick, each object the rule
  reaches catches the overlay with chance rate times the spreader's strength (times the link
  score for `through`), from the seeded random numbers. Before, a hidden 0.15 cutoff made it
  spread to every target at once or to none, and links below 0.3 never spread. The epidemic
  sim now uses `spreads ... at .004`.
- DES links: a link is one number, one way, from one object to another, under a name the
  program makes up. It is declared with its range (`link Nation trade 0...1`,
  `link Nation respect -100...100`, `link Nation border bool`), kept inside it, and starts at 0
  (or a `by [ formula ]`). Changes belong to the real objects, so `self link to target on trade
  offset +.3` works in actions and loops, and the change can be any number expression.
  `:link_score(a, b, "trade")` takes the two objects. The runtime reads no meaning into a link:
  `spreads through trade` is now an error that says to write `spreads where` with the formula.
  Before, links were stored under variable names (changes made in actions never reached the
  objects) and scaled into 0..1 over a range guessed from the formula.

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
