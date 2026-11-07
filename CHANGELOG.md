# Changelog

The Goblin version is the version of `goblin` (crates/goblin-cli). Numbers
in parentheses are the pull requests that made each change.

## Unreleased

- sims/capstone: a War keeps the allies each side called in as plain lists (`attackers`, `defenders`) instead of `attacks`/`defends` links. The links were `range: bool`, and the battle read them with `:link_score(w, it, "attacks") > 0`, a type error that `:objects` dropped as false, so allies never added their armies; now they do, and the history changes (year 500: 41 polities, the largest holding 69 provinces; before, 48 and 29).
- Interfaces are written as a block, `interface conquest |` ... `xx`, outside any class: it holds the interface's actions (`act annexed(p, t)` ... `xx`, free actions like any other), decisions, judge, transitions (`transition absorb` ... `xx`) and their hooks (`on absorb => annexed(self, target)`, or a block under it). `as` stays as the second way in, for actions, decisions and judges written outside the block (`act tax(x) as taxation, governance`); transitions and hooks go only in the block (`transition absorb as conquest` is P1519, as is a header without `|` or anything else in the block); one block per interface (a second is P1518). A class takes them with `uses conquest`; the transitions run where it writes that line (in a `phase:` block in that phase, at the top of the class when the tick ends), in place among the class's own transitions, and the hooks become the class's wherever it uses the interface. A class that writes a hook itself and also gets it from an interface, or gets one hook from two interfaces, is P1517. A class's `on absorb =>` (any transition hook) can now follow its fields directly (before: P0904 unless something else came between). sims/capstone takes the transitions and hooks its six classes shared from six interfaces (`founding`, `conquest`, `unification`, `downfall`, `breakup`, `regime`), with the same output and event log.
- sims/capstone step 3: forms of government and transitions. `government.gbln` holds the 15 forms as members of one overlay class (`<^>Government`, `dup: replace`): each is allowed only where the plan allows it, bends traits (and their baselines) and leans decisions; a polity's form changes in the new settle phase by chance from its state (interregnum, claimant, coup, usurper, revolution, the fall of a republic, religious takeover, secularization, reform, chiefs and elders), or by rule when its class no longer allows it. A third interface, `realm` (reform, settle, migrate, union, cede, release, and a vassal's revolt), draws once a year beside `govern`. Each class's settle phase carries its transitions with all eight kinds: tribes settle into village communities, villages grow into city-states and back, city-states into kingdoms, kingdoms into empires or nation-states and back, far provinces break away (fracture), civil wars split realms, broken losers are absorbed or made vassals (with tribute and client regimes), unions merge, colonies and leagues are spawned, and leagues take in their members or break up. `check.sh` checks that every kind of transition happens.
- sims/capstone step 2: polities. `history.gbln` runs a prehistory of the land, then places 20 kingdoms and the tribes and village communities on the settled land left, and runs the history. One aspect (`Polity`: 16 traits, 12 resources) and two interfaces (`govern`, 6 decisions in the inside phase; `dealings`, 15 in the between phase) are used by all seven polity classes; each judge draws one decision a year by weight. Wars are objects that fight each year. Provinces gain an owner, a settlement size, buildings and trade. `check.sh` checks both steps (deterministic output and event log, every decision taken, wars end).
- Interfaces: named groups of behavior that classes `uses`, written once outside any class. An action belongs to one with `act tax(x) as taxation` (several: `as taxation, governance`), a decision with `decision trade as diplomacy` ... `xx`, and the judge over its decisions with `judge as diplomacy` ... `xx` (or `judge_all as`). A class takes them with a `uses diplomacy` line (the same line as aspects): written at the top of the class, the interface's decisions and judge run when the tick ends; inside a `phase: between` block, they run in that phase. Its actions are free actions the class's code calls. `targets: uses diplomacy` scores the objects of every class that uses it (in a decision, or a merge, absorb or subjugate transition; an overlay's transition can't, P1512), and `:objects(uses diplomacy)` lists them, across classes that share nothing else. It is not inheritance and nothing about it passes through a transition: an object's class uses what that class says. A name that is neither an aspect nor an interface is P1340, a bad `as` list P1344, one decision name twice P1345, and a class that already has a judge of its own where it uses an interface with one P1346. Several interfaces with a judge in one place run their judges one after the other, in the order the class names them, each over its own decisions (so a polity draws once from each group a year).
- Aspects: `aspect Political |` ... `xx` names a set of fields, one a line (`~aggression`, `~stability: .5`, `gold: 100`, `army`), and a class takes them with a `uses Political` line in its body (`uses Political, Wealth` for several). It is not inheritance: the class gets the fields as if it wrote them. A field with no value starts at 0 when it is a `~` trait and at nil otherwise; a value in the aspect is the starting value, a line the class writes wins over it, and an object's own value wins over both. The aspect may come after the classes that use it. An aspect no one declared is P1340, a bad aspect line P1341, an aspect or field named twice P1342, and two aspects giving one field different starting values P1343.
- DES overlay parentage: a kind a transition makes (mutate, split, fracture or merge with no `into:`) is a child of the kind it came from, and its name says so. A declared overlay's children are numbered after its family (`language_1`, `language_2`); a made kind's children add a letter after a number and a number after a letter (`language_1` → `language_1A`, `language_1B` → `language_1A.1` → `language_1A.1A`). A merge's new kind carries both names, as merged companies do (`language_1A` and `language_1B` make `language_1A-1B`; different families, `latin-greek`); the same two kinds merging again make that same kind, and two overlays of one kind merge into that kind. Spawn and spread keep the kind. An overlay record's `parents` lists the kinds it was made from (none for a declared overlay; both for a merge). Names stay unique across regions, so a region's names can skip letters or numbers.
- DES event log: `:event_log(path)` writes a line to the file for each thing the DES does, as it happens, and `:event_log(nil)` stops it: `tick 12 | Polity Arveth | raid | Province Oss` (tick, who, what, to whom). Who is the class and the object's `name` field, else its variable, else its id; an overlay is `flu on Town Ashford`. It records the actions a class judge's deed calls (not what its conditions read), every object and overlay transition with the objects or overlay kinds it made or took, erases, and overlays put on, spreading, pushing others off (`dup:`/`conflicts:`), running out and taken off. With regions, each region hands back its lines with its tick and the program writes them in region order; an overlay that spreads to, or is put on or taken off, another region's object is logged where that happens, the next tick, still naming where it spread from. A bad argument is R1251, a file that can't be written R1252.
- DES phases: a class or overlay can put members in a `phase: NAME` block (closed with `xx`), and a `tick()` block lists the phases to run, by name, in order (`tick()` / `land` / `inside` / `xx`). The block is ordinary code: a bare name runs that phase, so `repeat` repeats one. A class's phase holds decision blocks with their judge, `every N ticks` blocks, transitions and `one of`; an overlay's holds `spread:`, `every N ticks` blocks and transitions; anything else there is P1581, a phase inside a phase P1580, the same phase twice P1582. A phase runs as part of the tick it is in (`every N ticks` counts that tick); the tick ends where the block does: what is in no phase runs then, and ageing, decay, `lasts:` and the tick count move once. A bare `tick()` runs only what is in no phase. A name no class or overlay has a phase for is R1250. Over regions, each phase runs in every region together and its changes reach the other regions before the next phase; the regions' handlers get `{"tick": n}` once, at the end. A member of an overlay class gets the class's phases. The interpreter does not run `tick()` blocks.
- DES across regions: `:tick()` runs every region with no list passed in. A region is an actor the program started that holds DES objects: it was handed some, was posted or moved one, or made its own. Regions keep their order from tick to tick: one that starts holding objects joins at the end (during a tick when a region moves or posts an object to it), one that ended leaves, and the tick count goes on. `tick(regions)` is gone (`:tick(x)` is now an arity error). An actor that holds no DES objects is not ticked. The program's own objects still do not tick while it runs regions, as before.
- A class's own code (its actions, `every` blocks and `on` hooks) calls an imported module's action by its alias, `m::double(x)`, as top-level code does (before: the name read as a program variable, nil, and the call failed with "value of type 'nil' is not callable").
- Objects: an object that has left the world (erased, or taken in by a transition) reads as nil wherever it is held: a variable and a loop variable now read it as a field and a list item already did (before: a `dangling ref` error, so `x.nix?` failed in `repeat allies as x`). One sent to an actor or another region is still an error that says where it went. An erased holder in `:objects(Class, it >> owner == h)` now matches the objects that have no holder.
- DES across regions: `absorb` and `merge` take a target in another region (before: they silently never fired). The claim is settled when the tick ends, in a fixed order (by region, then as made), so an object is taken once a tick and two claims on it never both win; at the start of the next tick the claimer's region carries it out and the target leaves the world. What the target owned goes to the absorber (or the merged object) in every region; its overlays go along; its own link offsets arrive one tick later (they were held in its region). A region's handler and the messages it gets now see the other regions' objects as its DES rules do (as of the end of the last tick); a region applies what others changed before it reads its messages. `:overlays`, `:overlays_of` and `:overlay_strength` read another region's object's overlays (in a region and in the program that runs the world). A transition in the program that runs a world can't absorb or merge a region's object (R1248).
- `:metrics()` with no argument reports what a program costs: current and peak RSS, the memory the live DES state takes (`des_mb`, separate from RSS), live objects and overlays with how many were made and are gone, and tick time by pass (`tick_ms`: links, spread, decay, overlay blocks, overlay transitions, `on end`, `every`, decisions, transitions) and by layer (`layer_ms`: each class, and each overlay kind or overlay class). In a world split into regions it adds up every region (times are then thread time). The capstone's `capstone_metrics.csv` now records all of it every 100 years. `:metrics(collection)` is unchanged.
- sims/capstone: step 1 of the 8,000-year history sim. A continent from a seed (elevation, rivers that reach the sea, rain with a steppe belt, eight terrains), 400 provinces linked to their neighbours, people growing toward what their land feeds and moving to land with more room. River valleys fill first and the population levels off; `capstone_metrics.csv` records speed, memory and counts every 100 years.
- DES: a decision's `where` and `score`, written in a class in an imported module, call that module's actions by their plain names, as the module's other code does (before: R1220 undefined variable).
- DES: `:objects(Class, :link_score(x, it, "border"))` and a decision's `where :link_score(self, target, "border") ...` (alone or compared with a number) look only at the objects `x` has a link to, instead of checking every object, when a link never set would fail the test (no `score` formula). Province neighbours by link: 400 provinces, 210 → 14 ms a year with `:objects`; the capstone's migration decision 170 → 4 ms a year. The same objects come back in the same order.
- Objects in collections are the objects, not copies. A list or map built from objects, or given back by a builtin (`:objects`, `:get_first`, …), names each object, as a field does: reading it gives the object as it is now, and an erased one reads nil. Before, it held a copy taken when it was made, so `lst[0] >> hp` showed old values after a write or a tick, and a `repeat lst as e` loop (or `x | lst[0]`) put that old copy back over the live object, undoing every change made since. Printing a collection shows the objects as they are now.
- DES: `owner` is a relationship, not a place (owner, 2026-11-02). Setting `owner` never moves an object between regions. `:actor(state)` runs the objects its state names (no longer what they own). `:move!(x, y)` moves `x` alone to the region running `y` (a region actor, or any object); the main program's own objects go at the start of the next tick. Ownership works across regions: `subjugate` reaches another region's object, owner chains cross regions, and an erase moves what the object owned up in every region (each region hears of it). An `owner` that would make an object own itself is an error (R1246); two regions making a loop in the same tick stop the next tick (R1247). The epidemic and predator-prey sims say `:move!` and give the same output; feudal defectors keep their lands where they are.
- DES transitions: all eight (`spawn`, `erase`, `mutate`, `split`, `fracture`, `merge`, `absorb`, `subjugate`) run on class objects on the VM (before, five did nothing). A successor starts as a copy of its parent (current fields; another class gets the shared fields and its defaults), then its field list; `spawn` starts from defaults. What the old object owns, its overlays and its links move as each transition says; `subjugate` sets the target's `owner` and nothing else. Hooks `on split =>`, `on fracture =>`, `on merge =>`, `on absorb =>`, `on subjugate =>`, `on mutate =>`, `on spawn =>` run after the transition with the removed object still readable (writing it is R1240) and the successors by name. An overlay transition with no `into:` makes a new overlay kind named after its class (`language_76`), numbered apart in each region; it travels with its overlays. An overlay's split, fracture or merge must say each intensity (no hidden 55/45, 20%, 40%); a successor's fields are all read; `strength:` there is `intensity:`. What a transition can't do is a parse error (P1510-P1517).
- AGENTS.md is a style guide for writing idiomatic Goblin (crossbones, `..`, nix, `??`, `send`/`return`, `repeat`, `judge`, pipelines, classes, DES); every example runs. The VM tracking notes it held are in CLAUDE.md.
- DES overlay classes: `<^>Mood |` ... `xx` declares one, `overlay calm <^> Mood` makes a member, which gets the class's lines it doesn't write; the class's `dup:` decides between two members (no `dup:`: they stay together). `dup:` gains `stronger` (the higher intensity stays; on a tie the one already there). `conflicts:` stays for rules across classes.
- DES: the forms the syntax pass replaced are errors that name the new form (`name | decision against`, `one of`, `order by`, `on erase: action`, `transition ... when`, `link Class name ...`, `a link to b ... offset`, `overlay X on`, `detach X from`); per-object decisions are gone (an object varies a class decision through its fields). A class judge may come before its decisions; a matrix keeps its class's judge. Conformance cases, tests/test.gbln and bench/des are in the new syntax.
- DES syntax pass: overlays, decisions, judge, transitions, links and `order:` are written as Goblin blocks (`overlay sick` ... `xx`, `decision prey` with `targets:`/`score =>`/`margin:`, one class `judge`/`judge_all` reading picks by name, `transition erase where`, `on erase =>`, `link culture` with `hosts:`/`range:`/`score =>`); calls `:overlay!`, `:detach!`, `:link!`, `:unlink!`, `:overlays(host[, overlay])`; `modify` is a film over stored fields; collection `*_where` take code with `it`. All sims rewritten with unchanged output; wall time vs the old syntax: epidemic 4,000 people -52%, civilization -8%, feudal -2%, predator-prey -2%, RPG +6%.
- VM object store: each object keeps a fixed place and each id remembers where it was found, so reading an object by id checks one place instead of hashing; owner queries keep their class check; each DES decision keeps its compiled formula and condition; call frames are smaller (instructions: predator-prey -10%, feudal -8%, epidemic -3%; plain call loop +2%).
- VM: object `==` compares ids without copying them, `repeat` over a list skips a builtin call, remembered-call keys read objects in place, and returned calls hand their local slots to the next call (feudal -2.6%, call loop -1.5% instructions).
- DES decisions look objects up by id instead of by text, and check once per decision whether the condition and formula only read (feudal -3.5%, predator-prey -3.9%, epidemic -3.8% instructions).
- DES: the erase check after an action uses object ids and keeps each class's compiled `erase when` conditions (feudal -3.3% instructions).
- Object fields keep their names in a layout shared by the objects of a class, and a field read remembers where it found its field: feudal -2.8%, predator-prey -1.5%, a call loop -10%.
- Regions send an object unchanged since last tick as it was, and the others keep their copy: small epidemic -13% instructions.
- Regions swap other regions' objects by id, not by text: small epidemic -7% instructions, predator-prey -3%.
- VM: in a decision pass, what actions that only read gave is kept from one
  decision to the next (and the next object's) until a judge runs an
  action or something that may change the world runs. Feudal -6%
  instructions.
- VM: `:len(:objects("X", it >> owner == x))` counts what x holds without
  listing (and copying) it. Feudal -5%, predator-prey -2% instructions.
- VM: an object id hashes by where its one copy of the text is, not by
  reading the text, and the store is looked up by id (`IdKey`; text is
  found among the ids held). Feudal -7%, predator-prey -7%, epidemic -2%
  instructions.
- VM: `:len`, `:count(x)`, `:is_empty` and `:has` read their list in place
  instead of copying it first. 50k `:len(:objects(...))` calls -18%,
  feudal -1.4%, predator-prey -1% instructions.
- VM: `:objects("X", it >> owner == x)` reads x (a variable, or a field
  path from one) and its contents at once, without building the query's two
  closures; an action doing it still counts as only reading. Feudal -12%
  instructions.
- VM: an object id is held once per thread and shared by every copy of
  it (`ObjId`); the object store, ownership lists and field indexes key
  by it. Copying a reference copies no text. Epidemic -8%, predator-prey
  -7%, feudal -5% instructions.
- VM: object ids and class names are copied without going through text
  formatting (a slowdown from #136), and an object passed to an action is
  passed as a reference without copying the object first. Action call +
  field read loop -10%; feudal -2.8%, epidemic and predator-prey -1.5%.
- VM: owner queries look each held object up once instead of twice;
  feudal -2%, predator-prey -1% instructions.
- VM: the class, action and object-name tables use Fx hashing; epidemic
  -1%, predator-prey -2%, feudal -2.4% instructions.
- goblin uses the mimalloc allocator. The sims ran about a quarter of
  their instructions inside glibc malloc/free; epidemic -26%, predator-prey
  -23%, feudal -18% instructions.
- VM: `:objects("Item", it >> owner == x)` returns x's contents without
  testing each one again, and the holder may be a field path
  (`it >> owner == f >> owner`) as well as a variable. Feudal 6%,
  predator-prey 6% fewer instructions.
- VM: an object's class name and id are shared strings, so copying an
  object (reading `:objects(...)`, passing it to an action) copies no
  text. Predator-prey 7%, feudal 6% fewer instructions.
- VM: making an object (`x <> Class | ...`) no longer copies the whole
  class declaration first. Epidemic 3%, predator-prey 6%, feudal 4%
  fewer instructions.
- VM: `pick ...` passes its settings straight to the pick instead of
  building a map on every call, and picking one item no longer makes a
  hash table. Epidemic 12%, predator-prey 6%, feudal 1% fewer
  instructions; same draws, output unchanged.
- VM: object fields and the DES id sets and indexes use a faster hash
  (Fx instead of SipHash). Epidemic 12%, predator-prey 7% and feudal 6%
  fewer instructions; output unchanged.
- DES (VM): `~` now means a trait. `~fear: .2` stays in 0..1 (its starting
  value is clamped too); every other number field is a plain value,
  whatever it starts at. Before, a field that started in 0..1 (0 and 1
  included) silently became a trait and `~` opted out, so a counter like
  `defected: 0` stopped at 1. A matrix row keeps a field's `~`, and an object a `spawn` transition
  makes gets its class's traits. Tests,
  sims and `tests/test.gbln` are switched over (`~` dropped from plain
  numbers, added to the 0..1 fields); every sim gives the same output.
- DES (VM): in a region, setting an object's `owner` to another region's
  object sends it at once, with what it holds, as `:move!` does; it does
  nothing more in its old region that tick (it used to graze or hunt there
  against the new region's objects until the tick ended).
- Sims: epidemic, predator-prey and feudal rewritten around ownership. A
  person's or animal's `owner` is its town or meadow, and moving one is
  setting `owner`. Regions are `:actor(x)` with no handler, and there is no
  list of regions to pass around and no `:move!`. All three give the same
  output as before. 62/102/301 lines instead of
  71/115/312; bench/vs_mesa README updated.
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
