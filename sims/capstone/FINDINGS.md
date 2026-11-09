# Capstone step 1: land and people

`world.gbln` builds a continent from a seed (`map.gbln`), makes one `Land`
province per land cell with a `border` link to each neighbour
(`land.gbln`), and runs years with `:tick()`. People grow toward the food
their land gives (logistic, 0.5% a year) and move to bordering land with
more room through the `settle` decision. They start in 20 homelands, the
best-fed provinces with no two side by side.

Run it from this folder: `goblin run world.gbln` (`YEARS=n` for a shorter
run). `check.sh` runs 3,000 years.

## Result (seed 42, 400 provinces)

- Terrain: 41 river, 124 plain, 59 coast, 45 forest, 56 hills,
  32 mountains, 33 steppe, 10 desert. One province is an island with no
  border, so nobody reaches it until sea lanes exist.
- River valleys reach 80% full first (year 1,200), then steppe and desert
  (1,300), forest, hills and mountains (1,400), plain (1,500) and coast
  (1,600).
- The population levels off near what the land can feed: 154,914 people
  in year 3,000, growing 0.04% over the last 300 years.
- Speed: about 6 ms a year (3,000 years in 18 s); RSS stays between 33 and
  58 MB. Every 100 years, `capstone_metrics.csv` records wall time,
  years per second, time spent in `:tick()`, RSS, objects, overlays,
  people, settled provinces, and how full each kind of land is.

## Fixed in the runtime on the way (each its own PR)

1. Lists held copies of objects. A loop over a list put an old copy back
   over the live object (#163).
2. Finding the provinces linked to one checked every object: 210 ms a year
   for `:objects`, 170 ms a year for the migration decision. Now both look
   only at the linked objects: 14 ms and 4 ms a year (#164).
3. A decision in a class from an imported module could not call that
   module's actions (#165).

## Missing (named, not worked around)

- **Instrumentation.** Not available: peak RSS, live DES memory apart from
  RSS, counts of objects and overlays created and destroyed, and time per
  DES pass (growth against migration). The CSV has what exists now: RSS
  from `:mem_total()`, live counts, and time inside `:tick()` against the
  rest of the program.
- **`capacity` can't be a field name.** In a class, `capacity: N` is the
  container-capacity clause, so the next field fails with P0910 ("only
  'act', 'action' ... allowed here"). The sim calls the field `food`.
- **A map read can't go in a string** (`"{cfg{"k"}}"`): bind it first.
  Already known.
- **Sea lanes.** Links can carry them; the map does not make them yet.

# Capstone step 2: polities

`history.gbln` runs the land alone for a prehistory (1,200 years), then
places the polities at year 0 and runs the history (1,000 years by
default; `YEARS=n`, `PREHISTORY=n`). Each year is a `tick()` block:
`land` (step 1), the accounts (plain code), `inside`, `between`.
`check.sh` runs both steps.

## What is built

- **Provinces** (`land.gbln`) gained `owner`, a settlement size worked
  out each year from people, buildings and trade (hamlet 1, village 50,
  town 200, city 500, great city 1,000), six kinds of building and a
  `trade` level set from the holder's trade links. Step 1's output is
  unchanged.
- **One aspect, seven classes** (`polity.gbln`). `aspect Polity` holds
  the 16 traits and 12 resources from the plan, plus the bookkeeping
  fields (capital, lands, cities, power, culture). Tribe, Village,
  CityState, Kingdom, Empire, NationState and League each write
  `uses Polity`, their `rank` and the traits they start with.
- **Two interfaces, used by all seven classes.** `govern` (in
  `phase: inside`): tax, relieve, build, levy, disband, borrow, each
  aimed at one of the polity's own provinces. `dealings` (in
  `phase: between`): trade, envoy, aid, ally, treaty, spy, denounce,
  embargo, demand, show of force, raid, strike, war, break a treaty, sue
  for peace, aimed at a bordering polity. A decision's `where` is "can
  take it when"; its score is its weight. Each judge draws one decision a
  year by weight among the possible ones (`:sample_weighted`).
- **Links**: `border` between polities (set each year from the provinces
  they hold), `trade`, `alliance` and `grievance` (one way). Each change
  lasts a number of years, so a link left alone fades.
- **War** is an object. The `war` decision makes one over the target's
  province nearest the attacker's capital, and each side's allies come if
  their honor holds (one that does not come breaks the alliance). Each
  year in the between phase it fights: army, discipline, iron, horses,
  people at the front (halved every 4 provinces from the capital, roads
  stretch it), walls and luck. It ends when one side wins or breaks, by
  treaty or accepted peace, or when one side is gone. A winning attacker
  takes the province. Three war overlays from the plan's War table come
  with it: `at_war`, `war_weariness`, `war_memory`.
- **Every event goes to the log**: `history_events.txt` has each
  decision taken (`tick 1203 | Kingdom Lorvar | trade | Kingdom Orisk`),
  overlays and erasures.

## Result (seed 42, 1,000 years)

- Year 0: 20 kingdoms, 63 village communities, 65 tribes (148).
- Tribes and village communities are taken one by one; nothing makes a
  new polity yet (spawn and fracture are step 3), so the count only falls:
  83 at year 100, 43 at 300, 25 at 600, 21 at 1,000.
- The largest holds 39 provinces at year 1,000 (about 11% of the held
  land); the distance halving keeps the largest from running away so far.
- About 1,200 wars in 1,000 years; 22% of polity-years are spent at war.
- Speed: about 70 ms a year with 148 polities at the start, falling as
  they merge (1,000 years in 70 s; the prehistory 7 s).

## Choices to confirm

1. **One decision per interface a year**: one govern decision and one
   dealing. The plan says a polity can take several in a tick (trade with
   one, marry a second, fight a third). The judge can draw more than once;
   how many is your call.
2. **The draw is written inside the judge**, with builtins only. The event
   log records the actions a judge arm calls, so a helper action doing the
   draw would be logged as the deed.
3. **Govern decisions aim at a province of its own**: tax, relieve, build
   and levy pick one (a decision needs a target, and never targets
   itself); disband and borrow aim at the capital.
4. **21 of the 42 polity decisions are in.** Marry waits for rulers (step
   4), meddle for factions (step 5), proselytize and the faith group for
   faiths (step 6), the realm group (reform, settle, migrate, union,
   cede, release) for the transitions (step 3).
5. **Class differences are rank and starting traits only.** Every class
   has the same decisions (your ruling); what only some classes can do
   (migrate, mercenaries, governors) comes with the realm decisions and
   the transitions.
6. "Village and up" reads `rank`; "city and up" means holding a province
   with a city, as the plan says.
7. The numbers (tax rate, army upkeep, settlement sizes, the 0.15
   spread of starting traits, 2% a year back toward the baseline) are
   mine, and are all in `polity.gbln` and `land.gbln`.

## Missing or wrong in the runtime (named, not worked around)

- **A field can't be read or written by a name held in a variable.**
  `p >> k` reads the field called `k` (R0403), and `p{k}` doesn't take an
  object. So the 16 traits are written out where they are varied and
  pulled back. `tests/test.gbln` does this (`self >> self_resource`) in
  trade and aid, which is part of why those two fail there.
- **Calls in a decision's score are slow.** Writing the `grudge` and
  `weariness` helpers out by hand in the scores cuts the between phase by
  30% and the inside phase by 40%. The sim keeps the helpers.
- **`score` can't be a class field name** (P1400, like `capacity`). The
  War's field is `standing`.
- **A name means an action before a link or overlay.** With an action
  called `trade`, `:link!(p, t, trade, ...)` gets the action (R1224); a
  global called `at_war` in the main program hides the `at_war` overlay
  inside the module. The sim names links as strings.
- **An erased object can't be tested.** Any read of a reference to it,
  `.nix?` and `:is_type` included, is an error. The sim clears wars and
  ally lists in `on erase`.
- **`aim?: nil` doesn't parse** (P0904), though the error's own example
  is `email?: ""`. A field whose default is nil can't be given nil when
  the object is made (T0207), so `war` sets `aim` after making the War.
- **A lasting `:link!` change can't lower a timed one.** With a
  grievance of .5 added for 40 years, `:link!(a, b, "grievance", -.3)`
  leaves it at .5: the lasting part is clamped on its own (0 - .3 = 0).
  Only `-.3` for some years gives .2. So envoy, aid and treaty ease
  grievance for 40 years. Is that the intended meaning?
- **Aspects and interfaces don't carry `transition` or `on erase`**:
  fixed, interfaces now hold transitions and hooks (see step 3).
- **No sea lanes** yet (from step 1): coastal polities trade and fight
  only across land borders.

# Capstone step 3: forms of government and transitions

Each year now ends with a `settle` phase: a polity's form of government
may change, and then its class's transitions may fire. `check.sh` checks
that every kind of transition happens in 500 years.

## What is built

- **Forms of government** (`government.gbln`). The 15 forms in the plan
  are members of one overlay class, `<^>Government` with `dup: replace`,
  so putting on a new form takes off the old one. Each form:
  - is allowed only where the plan's "Allowed at" column says;
  - bends the traits in its "Bends" column;
  - leans the decisions in its "Weights toward" column.
  A form's lean multiplies that decision's weight: each existing
  decision has a `lean_` field, 1 by default and 1.5 under a form that
  favours it. The form is drawn by fit at founding.
- **Changes of form**, in the settle phase, from the plan's Government
  table:
  - a monarchy falls into interregnum, and a claimant ends it;
  - coup, usurper, revolution, the fall of a republic;
  - religious takeover and secularization;
  - reform into a constitutional monarchy;
  - elders and chiefs, each way;
  - a client regime is put in on subjugation.
  Each possible change has a chance a year (how far past its conditions
  the polity is). One draw takes at most one of them, and the chance left
  over means nothing changes. A form the class no longer allows is
  replaced by rule.
- **The realm group**, a third interface (`realm`) in the inside phase:
  - reform, settle (a colony), migrate (tribes), union, cede and release;
  - revolt, for a vassal or league member breaking free.
  Settle and union only choose; the colony (spawn) and the union (merge)
  are transitions in the settle phase.
- **Transitions with all eight kinds**, in each class's `phase: settle`:
  - tribe → village community → city-state → kingdom → empire, and back
    down (mutate);
  - kingdom or empire → nation-state, and back (mutate);
  - a far province breaks away: a city as a city-state, a stretch of land
    as a kingdom (fracture);
  - a civil war, or an interregnum that drags on, splits a realm (split);
  - a broken loser is taken in (absorb) or made a vassal (subjugate), as
    is a polity that offered to serve when it met a demand;
  - an agreed union (merge);
  - a colony, or a league of allies who share an enemy (spawn);
  - a league takes in its members once one of them has dominated it for
    generations, then becomes a kingdom (absorb, then mutate), or breaks
    up when the shared enemy is gone (erase).
  A transition is possible when its conditions hold, and its chance a
  year rises the further past them the polity is, never above 0.9.
- **Vassals and colonies** pay a tenth of their gold to their overlord
  each year, and their grievance against it grows. They are called into
  its wars as allies are.

## Result (seed 42, 1,000 years)

- Transitions: 343 mutate, 212 fracture, 7 split, 28 merge, 349 absorb,
  43 subjugate, 73 spawn (colonies and leagues), and the erasures.
- The count falls more slowly than in step 2, since new polities now
  break away and are founded: 74 at year 100, 57 at 300, 43 at 500,
  19 at 1,000 (step 2: 83, 43, 27, 21).
- The largest holds 57 provinces at year 1,000 (14% of the held land;
  step 2: 39).
- At year 1,000 the classes are 1 tribe, 6 city-states, 4 kingdoms and
  8 empires. No nation-state forms: that needs learning above .3, and
  little raises learning yet.
- 26% of polity-years at war (step 2: 22%).
- Speed: 1,000 years in 116 s (step 2: 70 s). The added time is the realm
  decisions and the transitions' targets (below).
- These numbers were taken before the last changes (settle is only
  chosen when free land borders the polity; a split or merge successor
  starts with no wars; a landless league's traits drift). The 500-year
  check after them: 282 mutate, 91 fracture, 11 split, 13 merge, 220
  absorb, 39 subjugate, 65 spawn; 48 polities at year 500, the largest
  29 provinces; 17% of polity-years at war; every one of the 28
  decisions is taken.

## Choices to confirm

1. **I built the realm group in this step.** The transitions need its
   decisions: settle makes colonies, union makes merges, release and
   revolt free vassals.
2. **Revolt is a seventh realm decision.** The plan says breaking free is
   "chosen by a decision (revolt, refusing tribute)", but none of the
   seven groups of six lists it.
3. **The realm draw can choose nothing** (asked on a card). The weight
   for "nothing" is (stability + tradition) / 2. Without it, a polity
   with a city reforms most years and one at war cedes land most years.
4. **A form bends a trait and its baseline together.** The yearly pull
   back toward the baseline reads the trait through its overlays. So a
   lasting bend on the trait alone would fade over about 50 years; with
   the baseline bent too, it holds for as long as the form lasts. See
   the runtime gaps below.
5. **I added two changes of form the plan's table doesn't have:**
   - a monarchy in an empire becomes an imperial autocracy
     (centralization and ambition high);
   - a republic or elders' council becomes a merchant oligarchy (trade
     and openness high).
   No row in the table leads into either form, so without these they
   would never appear.
6. **Stand-ins until later steps:**
   - `succession` is still only a trait. Heirs come in step 4, and only
     then does it drive interregnums.
   - `peoples` counts the polities of a far-off culture taken in. It
     stands in for tongues and faiths (step 6), and decides empires and
     nation-states.
   - Factions (step 5) are left out of every condition that names one.
   - Union has no kinship or faith term yet.
   - Resettlement (an empire moving a people) waits for tongues.
7. **Sizes are mine:** a `+` in Bends is .1 and a `++` is .2; a form's
   lean is +.5; tribute is a tenth. Every chance is a product of
   0-to-1 terms, written out in `polity.gbln` and `government.gbln`.
8. **A league becomes a kingdom by rule** once it has taken in all its
   members and holds land; the chance was in taking the members in.
9. **Colonies start as village communities** (the plan allows "village
   community or city-state"). They only look for free land that borders
   their founder; sea colonies need sea lanes.

## Fixed in the runtime on the way

- A class's own code (methods, `every` blocks, `on` hooks) could not call
  `module::action`: it read as nil (#177).
- A transition's `targets:` now takes `uses NAME` as a decision's does
  (added to #175).
- Two interfaces with a judge can share a phase: each judge runs in turn,
  with locals of its own (added to #175). Before, it was P1346, and once
  that was allowed, a duplicate-local error.

## Missing or wrong in the runtime (named, not worked around)

- **Goblin code could only read a field through its overlays.** Fixed
  in the runtime: `:field(p, "x")` reads what the field holds, and
  `p >> x` still reads it with its overlays. The forms of government bend
  only traits now (not baselines), the yearly pull works on the traits'
  own values, and a colony or league copies its founder's own values, so
  the founder's form is no longer copied in as culture.
- **A reference to an object a transition took in** (absorbed, merged,
  split) is inconsistent:
  - read from a field, it gives nil;
  - held in a list or a local, `.nix?` on it is an error ("dangling
    ref").
  So War allies were links from the War (`attacks`, `defends`). Fixed in
  the runtime (#179: a gone object reads as nil everywhere), so they are
  plain lists on the War again (`attackers`, `defenders`).
- **An `:objects` condition that fails reads as false.** The ally links
  were `range: bool`, and `:link_score(w, it, "attacks") > 0` compares
  `true` with 0, a type error; inside `:objects(...)` the error was
  dropped and the condition read as false, so no ally's army ever counted
  in a battle. With the lists, allies count, and the history changed
  (year 500: 41 polities, the largest 69 provinces; before, 48 and 29).
  The runtime still drops such errors: open.
- **Interfaces carry decisions, judges, transitions and `on` hooks, but
  not `every` blocks.** The six classes now take the shared transitions
  and hooks from six interfaces (`founding`, `conquest`, `unification`,
  `downfall`, `breakup`, `regime`); the settle-year `every` block is still
  copied into all seven classes.
- **An object fires at most one transition a tick**, and a successor
  can't fire in the tick it is made. So an empire's collapse ("split,
  then fractures in the same ticks") takes several years.
- **Binary transitions test every pair.** Absorb, subjugate and merge
  evaluate `where` for every polity against every other each year (about
  70,000 checks), unlike decisions, whose targets are narrowed by links.
  That is about 10% of the year's time.
- **`ns::action(...)` doesn't work inside `{...}` in a string** (known):
  bind it first.

# Capstone step 4: rulers and dynasties

`family.gbln` holds the rulers. 0.51.4 modelled people (every member of
a ruling family, with parents, children, regents, great families and
plots). The owner asked for as little machinery as possible, with
`:event` telling what the sim does not need to model (2026-11-08), so
0.51.6 replaces the people with one ruler per realm and a few numbers.
`check.sh` checks that ruling houses marry across realms (`match`) and
reports what happened in the families.

## What is modelled

- **One `Ruler` per realm**: name, age, house, spouse (a name only),
  fame, and four traits (aggression, ambition, competence, piety). A
  tribe's or village's chief is a Ruler with no house.
- **`Dynasty`**: a name, prestige, how many realms it rules, how many
  of it have ruled, the thrones it claims, and its famous rulers'
  names. It has no members. It is erased (settle phase, `on erase`
  tells it) once it rules nowhere and claims nothing.
- **The ruling family is numbers on the realm**: `heir` (the heir's age,
  −1 for none), `kin` (other kin who could contend, up to 4) and
  `claimants` (other houses' claims on the throne).
- **A ruler's year** (inside phase): it ages; it may die, with the
  chance the sum of its hazards (illness by age, old age from 60,
  famine, battle while at war, two accidents, murder); it may wed at
  home; a married ruler of 16 to 50 may have an heir (under the
  eldest-son law half of births are daughters, told and not counted) or
  more kin; the heir may die (the next of kin becomes heir); kin are
  lost slowly.
- **Succession**, by the plan's seven laws, set by the form of
  government as in 0.51.4. Hereditary: contested into an interregnum
  with a chance of how far `succession` is under .5 (the house keeps a
  claim); else the heir, or under strongest kin a stronger kinsman, or
  the next of kin, or a house with a claim, or no one, and the line
  ends. Elective, faith and force draw among the family's own (if it
  has an heir of age or kin), a claiming house (elective) and two of
  the realm. Divided: a kingdom or empire with kin left is split, and
  the second half goes to the younger son when its new form has a
  ruling house. Term leaders as in 0.51.4.
- **Changes of form** seat a ruler as before: a crown keeps its ruler
  (who founds a house if it has none), or takes a claimant or a new
  house; a tyranny is seized (a new house), a junta's general rules
  without one, priests name one of their own, a republic elects. A
  house that loses a throne keeps a claim to it. A regent may seize a
  child's throne (chance .03 × ambition × (1 − honor) a year).
- **Marriage between realms** (the `court` interface, `match`): the
  ruler weds one of another realm's ruling house. Kinship rises, a
  grievance eases, and if the realm has no heir and no kin the other
  house gains a claim. Kinship joins union, a claim joins war (as in
  0.51.4).
- **The ruler's push**: one overlay, `reign`, bends aggression,
  ambition, discipline, stability and spirituality by .2 × (trait − .5)
  (stability by half). A child ruler pushes nothing; the push starts
  when it comes of age.
- **Fame** as in 0.51.4: wars, buildings and famines move the ruler's
  fame and the house's prestige; famous names are reused and numbered.

## What is told (`:event`)

Every death (how, drawn by the hazards that could have caused it), new
ruler (and how: "the heir", "elected", "the house of Holbry, by its
claim"), contested succession, deposed ruler (put to death or fled,
by the new ruler's cruelty), wedding, birth, heir's death, house
founded or faded, regency, and the end of a line. One telling feeds
back: a death told as a murder ("was poisoned", "was murdered by a
kinsman") costs the realm legitimacy .15 and stability .1. That is
modelled, not only told: murder is one of the hazards, so it can only be
told where the realm's state made it likely.

A run of one house, from the event log of the expected run:

    tick 1200 | Kingdom Corjor | new ruler | Ruler Casa Ashbry | "the first of a new house"
    tick 1216 | Empire Corjor | ruler died | Ruler Casa Ashbry | "of a fever"
    tick 1216 | Empire Corjor | succession contested | Ruler Casa Ashbry | "no candidate was accepted"
    tick 1217 | Empire Corjor | new ruler | Ruler Jafiha Ashbry | "the house of Ashbry, by its claim"
    tick 1220 | Empire Corjor | ruler wed | Ruler Jafiha Ashbry | "Yazeha, of a great family of the realm"
    tick 1231 | Empire Corjor | heir born | Ruler Jafiha Ashbry | "the first child who can inherit"
    tick 1252 | Empire Corjor | ruler died | Ruler Jafiha Ashbry | "of old age"
    tick 1252 | Empire Corjor | new ruler | Ruler Wensazena Ashbry | "the heir"
    tick 1252 | Empire Corjor | royal wedding | Ruler Wensazena Ashbry, Kingdom Toresk | "Wensazena Ashbry wed Cariko Nyelm, of the ruling house of Toresk"
    tick 1269 | Empire Corjor-Toresk | ruler died | Ruler Wensazena Ashbry | "of a fever"
    tick 1287 | CityState Kelul | new ruler | Ruler Jakemove Ashbry | "the house of Ashbry, by its claim"

## Removed from 0.51.4

Persons (parents, children, spouses as objects), coming of age with
upbringing, regents as objects, the great families as objects, the
`plot` decision and `scheme`, the `home` decision (a match at home is
a yearly chance now), the `crown` overlay (honor, fear, legitimacy and
the cruelty and charisma traits), health, and sex.

## Result (seed 42, 500 years)

- 40 polities, the largest 44 provinces (11.1% of the held land);
  16.6% of polity-years at war. (0.51.4: 47, 35 and 18.4%.)
- 1,253 rulers seated, 1,015 deaths (chiefs included: 501 of a fever,
  404 of old age, 40 hunting, 26 a fall, 23 poisoned, 21 in battle),
  74 interregnums, 4 lines ended, 56 claims won, 30 usurpations, 3
  executions, 3 regencies, 112 new houses, 115 marriages (80 between
  realms), 164 births, no partition in this run (one in an earlier
  run).
- 3,452 lines of narration in an event log of 253,636 lines.
- Forms at the end: 14 merchant oligarchy, 8 theocracy, 6 elective
  monarchy, no hereditary monarchy (0.51.4: 5). The runs are chaotic:
  an earlier run of this step ended with 2.
- Speed: 500 years of history in 67.4 s (0.51.4: 79.3 s; step 3:
  69.7 s).

## Choices to confirm

1. **The hazards and their sizes**: illness .004 × e^(.08 × (age − 30))
   (a child .05 under 5, .008 to 16); from 60 four fifths of it is told
   as old age; famine half of illness more; battle .01 × aggression;
   two accidents .001 each; murder .02 × (1 − legitimacy) ×
   (1 − discipline) × (.2 + .2 per kin up to 3 + .2 per claimant up to
   2) × (.5 + .5 × ambition), half of it by a kinsman when there is
   kin.
2. **Births**: a married ruler of 16 to 50 with no heir, .2 a year; with
   an heir, more kin at .08 a year up to 4. Kin are lost at .02 each a
   year. A match at home: .06 a year from 18 to 50.
3. **Claims lapse**: each claim has a .03 chance a year of lapsing
   (about 23 years to a half). Without it every house that ever lost a
   throne would claim it forever. A contested succession leaves the
   house a claim; a line that dies out leaves none.
4. **A claim is the strongest way back**: after an interregnum the
   house with the most prestige among those claiming comes in first.
   So a contested succession is often the same house back a year later
   (56 claims won).
5. **Regent's coup** in plain code, no regent object. A deposed child
   is told like any deposed ruler.
6. **Who weds whom**: only the ruler, and only once. One realm's ruler
   weds a member of the other's house; that house gains the claim.
7. **`house` is a field, not `owner`**: a ruler is not contained by its
   house (ownership is containment).

## Runtime gaps (named, not worked around)

- **A field with default `nil` can't be given nil when the object is
  made** (T0207): `x <> Ruler | house: d` fails when `d` is nil, though
  `x >> house |= nil` afterwards works. The sim sets it afterwards.
- Still from 0.51.4: a `modify` can't read through a nil field (the
  sim takes `reign` off while no adult rules), and a `modify` reading
  another object is worked out when the overlay goes on and once a
  tick (the sim puts `reign` on again when the ruler changes).
- 0.51.4's missing log line for plain code is `:event` (0.51.5).

## Fixed in the runtime on the way (0.51.3)

- **`where target == x` checked every object of the class.** Every
  `target == @capital` decision ran its condition on all 400
  provinces. Now only x is checked, with the same result. Step 3's
  500-year history: 73.2 s to 69.7 s, output unchanged.
