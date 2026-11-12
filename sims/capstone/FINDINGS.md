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

A run of one house, from the event log (first 0.51.6 run):

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

## Result (seed 42, 500 years, 0.51.9)

- 40 polities, the largest 45 provinces (11.4% of the held land);
  18.3% of polity-years at war. (0.51.4: 47, 35 and 18.4%.)
- 1,070 rulers seated, 1,069 deaths (chiefs included: 525 of a fever,
  414 of old age, 42 hunting, 30 a fall, 28 poisoned, 3 by a kinsman,
  27 in battle), 65 interregnums, 8 lines ended, 51 claims won, 43
  usurpations, 6 executions, 8 regencies, 106 new houses, 99 marriages
  (54 between realms), 127 births, no partition in this run.
- 3,315 lines of narration in an event log of 254,678 lines.
- Forms at the end: 13 theocracy, 7 merchant oligarchy, 5 elective
  monarchy, 1 hereditary monarchy (0.51.4: 5). The runs are chaotic:
  small changes move these a lot.
- Speed: 500 years of history in 62.8 s (0.51.4: 79.3 s; step 3:
  69.7 s).

**0.51.9: rulers leave only by erase** (owner, 2026-11-09: "An object
cannot be nil. It must be erased."). 0.51.6 also cleared the realm's
`ruler` by hand when a ruler died, was deposed, voted out or lost the
throne in an interregnum, which did the erase's job twice. Now the
ruler is marked (`dead` or `gone`) and its `transition erase` removes
it; the realm's `ruler` reads nil once it is erased. `reign` is not put
on for a ruler that is leaving, and a leaving ruler can't wed.

## 0.51.11: realism tuning (owner, 2026-11-09: fix before the next layer)

Before (0.51.10, 500 years): at the end 1 hereditary monarchy, 5
elective, 13 theocracies, 7 merchant oligarchies; about 30 successions
went to the heir. Causes found in the event log: an interregnum after a
hereditary crown usually ended in an elective one (its fit is
cooperation × (1 − centralization), the hereditary one's tradition ×
legitimacy, and legitimacy is low then); every successor polity (a
split's halves, a union, a village risen to a city-state) drew its form
anew by fit among eight or so, so a chief's realm became a monarchy one
time in eight; a merchant oligarchy had no road back to a crown; and
rulers married late (6% a year) and came to the throne with no family.

Changes:
- **Successors keep their form** (`government::keep`): a split's
  halves, a union and a polity that rose or fell a class keep the
  parent's form if they can have it; if not, a form that carries it on
  weighs .5 more in the draw by fit (a chieftainship becomes a
  hereditary monarchy, an elders' council a council of nobles or an
  elected crown, an empire's or a constitutional crown a hereditary
  one, a citizens' assembly a council of nobles; and back down).
- **An interregnum ends in a hereditary crown the more houses claim
  it**: each claim adds .3 (up to three) to its weight.
- **An elective crown turns hereditary** when one house has held it for
  generations: chance .2 × past(rulers / 6, .3) × past(centralization,
  .4) a year.
- **Rich families make themselves lords**: a merchant oligarchy (city-
  state and up) may become a hereditary monarchy or a tyranny, chance
  past(ambition, .5) × under(cooperation, .5) × (.5, or 1 at war).
- **Rulers marry readily** (25% a year from 16), and a grown ruler of a
  house comes to the throne with the family it has: married with a
  chance of .1 a year past 16 (up to .9), then an heir with .08 a year
  past 19 (up to .85) and up to three other children. Told as one event.
- **After an interregnum the restorer is drawn**: each claiming house
  weighs .2 + prestige / 50, a new house risen in the strife .5. Before,
  the most prestigious claimant always came in, usually the house that
  had just lost.
- **More ways to die**: 19 tellings drawn from the same hazards
  (illness split into fever, flux, wasting sickness and a rotten tooth;
  old age into old age and a sudden fit; a wound gone bad while at war;
  drowning, a palace fire, a surfeit at a feast; murder as poison,
  strangling, a stabbing at a feast, or by a brother or a cousin).

After (500 years): at the end 8 hereditary monarchies, 4 elective, 1
imperial, 8 merchant oligarchies, 7 theocracies, 1 aristocratic republic
and 2 elders' councils (31 polities, the largest 36 provinces); 127
successions to the heir; 39 interregnums (was 65); 312 marriages, 561
births; 59.2 s. With a different prehistory (1,100 years) the end has
14 hereditary monarchies, 4 imperial, 1 elective, 9 theocracies, 4
merchant oligarchies.

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

# Capstone step 5: groups and ideas

`factions.gbln`. Design: goblin-docs/factions-design.md (owner rulings,
2026-11-09). A group is people of a realm who decide (an object); an
idea is an influence that decides nothing (an overlay on a land).

## What is built

- **Ideas.** `Faith` and `Creed`, overlay classes on `Land`. Intensity is
  how firmly the land's people hold it. They spread to bordering land
  (`.05 × intensity` a year for a faith, `.04` for a creed), fade (`.002`,
  `.003` a year) unless preached, and mutate into heresies (lineage) by
  chance, likelier under an idea held across much of the world (the
  land's `dominance`). Each realm's culture is pulled toward its ideas'
  `bends`, weighted by how many of its people hold each (`drift_traits`
  reads `bends`, never a name). At year 0 each kingdom's capital holds an
  old faith of its own with two random bends.
- **Groups.** `Group`, one class. The 14 kinds are data (`kinds`):
  faithful, holy order, merchant houses, guilds, bankers, officers, free
  companies, great families, peasants' league, scholars, brotherhood,
  outlaws, smugglers' ring, benevolent club. Each says what realm field
  it grows from, its share of the people, its six traits, what it wants
  and what its members earn. A new kind is one more entry.
- **One decision a year, eight actions for every group**, each weighed by
  traits and state (a 0 leaves it out; "nothing" weighs 1 − power/2):
  give gold (loyalty × (1 − grievance) × gold), withhold (greed ×
  grievance × power), give men (loyalty × (1 − grievance) × arms, more in
  war), coup (ambition × militancy × arms × grievance × the realm's
  weakness), preach (zeal × power × (1 − held)), found an idea (zeal ×
  ambition × power, a faith or a creed by the realm's spirituality),
  press (ambition × unmet wants × power), rise (militancy × max(zeal,
  ambition) × grievance × power). A benevolent club has no militancy:
  it never plots or rises.
- **The realm answers** (`answers` interface in every realm): crush,
  concede (its wants move the realm's baselines), patronize (its idea
  firmer on the realm's lands, the realm's rule surer), or nothing.
- **Grievance** rises only from what is done to a group (crushed, a
  rival favored, wants far off) and fades 5% a year. Traits drift back
  to the group's own.
- **Coming and going.** New groups form by chance from what they grow
  from, rarer as the realm's people are taken up, likelier against a
  group grown strong. A large, disunited group splits (the splinter may
  go to a bordering realm); two of one kind and idea in a realm merge;
  a group with no members or no realm ends.
- **Into the existing machinery.** A plot raises the coup odds in
  `government::odds` (`plot`); a rising points the realm's `rebel` land
  at where the group is strongest and raises the breakaway odds
  (`unrest`). Plotters who took power get their wants.
- **Told with `:event`:** formings, splits, endings, gifts (temple,
  the poor, a loan, a school), refusals, plots, foundings, petitions,
  risings, crushings, concessions, patronage, heresies, each telling
  weighted by state.

## Result (seed 42, 1,000 years)

| | before (0.51.15) | with groups |
|---|---|---|
| polity-years at war | 21.6% | 21.6% |
| polities at year 1000 | 22 | 18 |
| usurped | 59 | 49 |
| history time | 92.6 s | 147.4 s |

Groups over 1,000 years: 960 formed, 891 ended, 30 splits, 1 merger,
98 alive at the end (between 70 and 210 throughout). Actions: give gold
13,417, give men 3,447, preach 2,007, press 1,083, found 257, withhold
38, coup plots 14, risings 14. Realms: patronize 668, concede 80, crush
64. 32 ideas at the end; the most widely held holds 8.3% of all people
(weighted by intensity). The largest group holds 27% of its realm.

## Runaways found and checked on the way

1. **Endless splitting.** A group with low founding unity (the
   peasants' league, .3) split every 50 years whatever its size, and
   the splinters split again: 1,173 groups by year 1000, and the run
   took 275 s. Now only a large group splits (`past(share, .05)`).
2. **Splinters regrew to full size.** Each group of a kind grew toward
   the whole of what its kind draws on. Now groups of one kind and idea
   share it.
3. **One faith took the world** (92.6% by year 400): patronage added .1
   to every land each time. Now .03, and heresies grow likelier under
   an idea held almost everywhere.

## Choices made here (flagged)

1. The 14 kinds and every number in them.
2. The weight formulas above and "nothing" weighing 1 − power/2.
3. Each kingdom starts with an old faith (tribes and villages with none).
4. The realm answers decision, its weights, and that conceding moves the
   realm's baselines (for good) while pressing moves its traits (for a
   while).
5. `Land.dominance`, a field written each year so a heresy's chance can
   read how widely the land's leading idea is held.
6. A group whose realm is gone ends; it does not follow a successor.

## Checks

`check.sh` now also checks that groups take each action, realms each
answer, and groups form, split, merge and end. In 500 years no merger
happens, so that check fails (flagged, not tuned away). Over 1,000
years one merger happened, and no plot ended in a coup (14 plots).

## Runtime fixed on the way (0.51.16)

- `:is_type(o, "Faith")` was false for a copy read with `:overlays`:
  only its kind's name matched. An overlay is an object of its class.
- An overlay's `on` blocks written in an imported module could not call
  that module's actions ("value of type 'nil' is not callable").
- Speed, same results: a `spread:` starting with `:link_score(self,
  target, "border")` tried every Land for every copy; a transition's
  `targets: Group where (target >> realm) == @realm ...` tried every
  group for every group. 300 years: 72.2 s to 65.3 s.

## Runtime gaps (named, not worked around)

- An overlay's `on mutate` block can't reach the copy that mutated:
  `self` is the host and no `mutated` is given (objects get `self`).
  The heresy telling doesn't rename the copy for this.
- `{:type_of(o)}` and `{look >> score}` inside a string print literally
  (known).

# Capstone step 6: overlays

`overlays.gbln`. Design: goblin-docs/overlays-design.md. Owner go
2026-11-10: faith transitions "like any other transition", year-0 seeding
as proposed, the rest by judgment.

## What is built

- **Ideas on land** (`Tongue`, `Faith`, `Creed`). A land holds several;
  each kind is put on once. A copy that has just spread in starts at .1
  (it carries its source's `at`; `account` marks it held here). Each
  year: a tongue held beside a firmer one loses 2% of the gap, the
  firmest gains 1% of what it lacks, and the court's tongue (firmest at
  the realm's capital) gains 1% × centralization on its realm's land and
  does not give way. A faith beside a firmer one loses 1% of the gap. A
  creed loses .02 a year on land with no city, and spreads only to cities.
- **Their transitions**, by chance from a cause. `apart` counts a copy's
  years cut off: +1 a year when no bordering land holds it at .3 or more,
  +.25 when only land under another holder does, 0 when land under the
  same holder does. Tongue and faith mutate at `.01 × intensity ×
  past(apart / 200, .25)` a year. Two tongues of .3 or more on one land
  merge at `.001 × (.2 + trade)`. A faith fractures into a sect at `.005 ×
  intensity × rival` (rival: the other faiths' intensity on its land). A
  creed fractures at `.003 × intensity × libraries / 3`. A daughter
  faith or creed has one trait of its teaching moved a little.
- **Conditions on land.** Plague starts at `.0001 × fullness × (.2 +
  trade)` a year on land of 100+ people; kills `deadly` (.12) ×
  intensity a year; halves trade at full intensity; spreads at `.5 ×
  intensity × (.2 + stored trade) × the target's fullness` to bordering
  land of 20+ people without immunity; decays .15 a year; mutates at
  `.05 × intensity` (deadly moved ±.05); ends where fewer than 20 are
  left; leaves `immunity` 40 years. Famine when people pass food × 1.02:
  kills 6% × intensity / (1 + granaries) a year, ends when people are
  down to food. Disasters by terrain: flood (rivers, .004), quake
  (hills, mountains, .002), drought (steppe, desert, plain, .003; food
  −40% for 3 years), fire (200+ people, .001). Flood, quake and fire
  kill 3%, 5%, 2% once and may bring down two kinds of building.
- **Moods of a realm.** `dread` (.6, decays .04) after a plague, flood or
  quake on its land: fear +.1, tradition and spirituality +.05.
  `discontent` (decays .05): famine +.05 a year, a tax +.01, a lost war
  +.3, crushing a group +.05; relief −.05, conceding −.05, a won war
  −.1. It adds .5 × intensity to `unrest` (breakaway odds), and lowers
  stability .1 and legitimacy .05. `golden_age` (60 years) at .01 a year
  for a realm at peace, no war memory, stability .55+, commerce .5+:
  learning, openness +.1, ambition, legitimacy +.05; erased by a lost war
  or unrest .3+; after 20 years mutates `into: decadence` at .03 a year
  (40 years: discipline, cohesion −.1, ambition −.05).
- **What a realm reads.** `peoples` = tongues spoken by 15%+ of its people
  (intensity-weighted). `bends` gain cohesion `(share speaking the court's
  tongue − .5) × .3` and legitimacy `(share holding the court's faith −
  .5) × .2`.
- **Year 0.** Each kingdom's land: a tongue of its own (.9) and a faith of
  its own (.8). Each tribe and village community takes the tongue and
  folk faith of the first bordering non-kingdom realm that has them, or
  gets new ones (.8, .6).

## Result (seed 42, 500 years)

| | step 5 (0.51.17) | step 6 |
|---|---|---|
| polities at year 500 | 33 | 31 |
| polity-years at war | 19.4% | 19.8% |
| imperial autocracies at the end | 4 | 5 |
| theocracies at the end | 9 | 7 |
| group mergers | 0 | 3 |
| 100 years (one thread) | 23.7 s | 36.7 s |

Overlays: 8 new tongues, 43 merged, 8 new faiths, 35 sects, 2 creed
splits; 17 plagues (spreading to about 4 lands each), 12 strains; 320
famines; 77 floods, 78 quakes, 261 droughts, 97 fires; 169 dreads, 26
golden ages, 11 decadences. At the end 75 tongues (the largest spoken by
9.5% of all people), 45 faiths (largest 4.3%), 11 creeds; 18 of 31 realms
rule more than one people.

## Tuning on the way

1. 153 new tongues in 500 years: every land on a border counted as cut
   off. Now only firm copies (.3+) count as kin, a border cuts a quarter
   as fast, and drift starts after 50 years alone.
2. 343 tongue mergers: each merge makes a new kind on its land. Now
   .001 × (.2 + trade).
3. Conquered land never took its ruler's tongue (it gave way to the
   local one). Now the court's tongue gains and does not give way.

## Choices made here (flagged)

1. Every number above.
2. Cut from the list: the faction "movement" overlay (press already moves
   the realm toward a group's wants); groups of other faiths gaining
   grievance where one faith holds most of the realm (patronize already
   does it); dread after drought and fire.
3. The realm overlay for unrest is named `discontent`, since the realm
   already has an `unrest` field (the groups' part, reset each year); the
   overlay adds to it.
4. One `immunity` covers every strain.
5. A copy that has just spread starts at .1, set by `account` (the
   runtime puts a spread copy on at its kind's intensity, which for a
   kind made by a transition is the copy's intensity when it was made).

## Runtime fixed on the way (0.51.18)

- An overlay's transition could not read its host: `self` is the copy,
  so `@people` read a field the copy doesn't have, and the condition was
  false with no error. Famines never ended, golden ages never ended
  early, a tongue merge's trade term never held. Now `host` is the
  host.

## Runtime gaps (named, not worked around)

- An overlay class is known only in the file that declares it (P1295),
  as aspects and interfaces are. Groups found new faiths and creeds by
  calling `overlays::found`.
- A condition that reads a missing field is false with no error
  (open question, listed in memory).

## 0.51.19: tongues from real life (owner, 2026-11-10)

Daniel asked why tongues merged all the time (43 in 500 years), then:
"think in terms of real life and then make the sim match". In real life:
tongues almost never merge (creoles only where many meet and none is on
top); the usual change is shift, toward prestige weighed against numbers,
in towns and among the elite first, sometimes the conquerors to the
locals' tongue (Franks, Normans, Manchu); new tongues come from
separation over 500 to 1,000 years, slowed by central rule and writing;
two tongues can live side by side for centuries where neither dominates.

Built that way (`shift`, `diverge`, `born` in overlays.gbln):

- Shift on each land between each tongue and the most spoken one, by the
  Abrams-Strogatz model (a = 1.3): a year's move is `.05 × pace × (a ×
  b^1.3 × s − b × a^1.3 × (1 − s))`; pace 1 in the country, 2 in a
  town, 3 in a city; standing 1, and 1 + 3 × (the share of the realm
  speaking it) for the court's tongue. Under .02 a tongue is gone there.
- A tongue's regions: bordering land speaking it (.3+) under one holder.
  The most peopled region is its heart (drift falls 5% a year); every
  other region drifts 1 a year (.25 where it borders land of another
  holder speaking it), × (1 − .5 × the holder's learning). A new tongue
  at `.01 × past(drift / 500, .4)` a year, and the whole region takes it.
- A creole: two tongues each .25 to .55 on a trading city (trade .5+),
  .002 a year.

500 years: 152 lands changed their main tongue, 10 new tongues, 0
creoles, 45 tongues at the end (the largest spoken by 10.9% of all
people), 9 realms of more than one people. 500 years took 160 s (150 s in
0.51.18): the regions are worked out every year.

## 0.51.20: groups tuned toward real life (owner, 2026-11-10)

Daniel: "Try it. See how it works." then "let's tweak them." What was
off, against real life: 13 of 38 realms were theocracies at the end
(priests ruled wherever the people were devout, with no clergy needed);
coups almost never happened (8 plots, 0 won: the odds needed an army
past 3% of the people *and* past .5 discipline); groups mostly gave
gold (7,348 gifts, 30 refusals); officers and peasants' leagues founded
religions.

Changed:

- **Theocracy** (`takeover`): `past(spirituality, .7) × under(tolerance,
  .5) × past(clergy, .4) × under(legitimacy, .5)`. `Polity.clergy` is
  the strongest group's power × zeal, set each year. `secular` also
  happens at `.05 × under(clergy, .3)`: the lords take the rule back.
- **Coup**: `past(armed, .1) × under(legitimacy, .6) × (.1 + .9 ×
  max(lost, unpaid, plot))`. A plot's weight is the plotters' arms
  against a twentieth of the army, × militancy. The group's coup score
  no longer needs grievance: `ambition × militancy × arms × weak × (.2
  + grievance + .5 × lost war)`. A junta can become a hereditary
  monarchy or a tyranny (`lords`).
- **Gifts** × `(.2 + .8 × max(at war, short of gold))`. **Refusals**:
  `greed × power × (grievance + .5 × hard + .2 × weak)`, where `hard`
  is the realm's `discontent`; hard times add `.03 × discontent` to
  grievance a year.
- **Founding**: `past(zeal, .4) × ambition × (.3 + .7 × power) × .5`.
- `SEED` sets the map seed of history.gbln (default 42).

Result, four seeds (1 to 4), 500 years, average per run:

| | 0.51.19 | 0.51.20 |
|---|---|---|
| polities at the end | 33.5 | 38.3 |
| theocracies at the end | 7.3 | 1.8 |
| juntas at the end | 1.5 | 3.0 |
| juntas formed | 8.0 | 21.8 |
| coup plots | 5.5 | 47.5 |
| gifts of gold | 6,583 | 3,421 |
| refusals | 29 | 136 |
| ideas founded | 154 | 117 |

Founders before: holy orders 225, peasants' leagues 145, guilds 119,
brotherhoods 58, officers 23, great families 22. After: holy orders,
brotherhoods and scholars only.

Left as found (flagged):

1. Theocracies still form about as often (26 to 48 a run); they now end
   sooner. Some come by other paths than `takeover` (a form not allowed
   at a new rank is replaced by the best fit).
2. Plotters seldom take power themselves (0 to 2 a run): most coups
   come from defeat or an unpaid army.
3. Risings stay rare (8 to 17 a run); hard times did not raise them.
4. Every number above is my choice.

## 0.51.21: faiths from real life (owner go, 2026-11-10)

In real life: conversion is a slow shift toward the faith with more
followers and standing, cities first; faiths are also carried on purpose
(missionaries along trade routes) and pushed from the top (a ruler
converts, the people follow; an intolerant court presses harder, a
tolerant one lets faiths live side by side); schisms split whole regions
along political lines; isolated communities keep rites of their own;
new faiths come from a teacher; blending is common in folk practice but
rarely makes a new named faith.

Built: `shift` (shared with tongues) at .03 with the court's standing ×
(1.5 − tolerance); `missions` spread `.02 × intensity × (1 + 2 × trade)`,
twice into towns and cities; no decay. `diverge` (shared) drifts cut-off
regions (new faith at `.005 × past(drift / 600, .4)`) and builds
`tension` in a region whose ruler holds the faith as the court's apart
from its heartland (`.5 + learning` a year); a schism at `.01 ×
past(tension / 300, .3)`, .7 of the region following. Blend: two faiths
each .3 to .6 on a land, .0005 a year.

500 years: 247 lands changed their main faith, 7 schisms, 2 new faiths
from isolation, 4 blends; 50 faiths at the end (the largest held by 8.4%
of all people); theocracies at the end 3 (13 in 0.51.19). No creed split
in 500 years (creeds not rebuilt yet). 500 years 183 s.

Flagged: no faith reaches across the continent. Every faith acts alike;
in real life universal, missionary faiths (Buddhism, Christianity, Islam)
spread far beyond their realms while ethnic faiths do not. A faith field
for how much it seeks converts would give that.

## 0.51.22: creeds from real life

In real life a philosophy is held by the learned (scholars, courts, the
well-off) in cities; it travels with scholars from city to city, along
trade, far more than between neighbours; schools compete for students
(Athens kept a few great schools for centuries, the small ones vanished);
they split where scholars gather, a pupil against his master; where
cities and learning collapse they fade, kept alive where books are kept.

Built: intensity = share of a land's learned; `schools` spread `.02 ×
intensity × (1 + libraries)` to bordering cities and cities of trading
realms; competition by `shift` at .05; fade `.05 / (1 + libraries)` a
year with no city; a school split at `.002 × intensity × libraries / 3 ×
(.5 + learning)` for a creed held .3 or more.

Every idea now spreads only from land where it is held .3 or more. Before,
faint traces kept sending copies that died and were sent again: by year
300 about 1,000 copies of each class, and each century ran slower (41,
48, 98 s). Now about 40 s a century.

500 years: 9 school splits, 32 creeds at the end; 177 faith conversions,
9 schisms, 5 new faiths, 5 blends; 96 tongue shifts, 7 new tongues;
theocracies at the end 1. 242 s.

## 0.51.23: conditions and moods from real life

In real life:
- **Famine** comes from bad harvests: a wet cold summer in the north, a
  dry one in the south, over whole regions at once (the Great Famine of
  1315-17 covered northern Europe); from the cold years after a great
  eruption over the whole world (536, 1257, 1600, 1815); from armies
  living off the land; and floods. Land near its limit has no margin. A
  severe dearth hits a region every 10-30 years. Most deaths come from
  the fevers of hunger; granaries and grain bought by trade soften it; it
  ends with the next good harvest, and people leave for other land.
- **Plague**: a new disease arrives from outside along trade and kills a
  quarter to a half; then it returns every 10-20 years for centuries,
  killing fewer each time, mostly the children born since the last wave,
  worst in cities. Ships and caravans carry it between ports and cities.
- **Disasters**: river floods every few decades destroy the harvest more
  than they drown people; quakes kill most where people are crowded in
  stone houses; nearly every town burned more than once a century, with
  few dead.
- **Dread** after catastrophe: piety, processions, a turn to the old
  ways, scapegoats (strangers, other faiths), doubt in a ruler heaven
  seems to have left.
- **Golden age / decadence**: "golden age" is a label put on peace,
  trade and a steady rule over land with room, when food is cheap and
  there is surplus for patrons. "Decadence" is a moralist's word, not a
  cause. What ends the good years (Turchin's secular cycles): the
  surplus feeds more children, the land fills, food per head falls, and
  the number of heirs and office seekers outgrows the lands and offices
  for them, so the great houses fight.

Built: `dearth` (regional weather: 6 by 6 provinces, .06 a year, a cut
of .1 to .35; dry open land gets a 3-year drought instead half the time;
dark years at .003 a year for 2 years, .25 off everywhere; floods take
half the harvest; `ravage` .3 where an army fights). Famine intensity
`(people / harvest - 1) × 2.5`, deaths `.1 × intensity / (1 + granary +
trade)` a year, ends when people are at or under the harvest; a famine
of .5 or more brings dread. Plague: `toll` = (1 - immunity) × 1.5 in a
city; also spreads between cities of trading realms; `immunity` decays
.04 a year, a plague catches under .6; an outbreak is 20 times likelier
on land with immunity left (it came before) and 5 times with famine.
Fires .02 a year in towns and cities (.002 dead); floods .03 on rivers
(.01 dead); quakes .02 dead, .08 in a city. Dread adds tolerance -.1,
legitimacy -.03. Golden age needs fullness (people over what the land
feeds in a good year) under .7; mutates `into: elite_strife` at `.05 ×
crowd` after 10 years, crowd = how much fuller the realm is than when
the age began, 1 at eight points. `elite_strife`: cohesion -.1,
stability -.1, legitimacy -.05, 40 years.

500 years: 1,769 famines, 2,188 droughts, 626 floods, 2,523 fires, 91
quakes, 4 dark years, 17 plagues, 45 strains, 640 dreads, 37 golden ages,
11 turned to strife. People level off at 84,000-97,000 (0.51.22: rising
from 105,000 to 152,000).

Flagged:
1. People fall from 105,000 to 85,000 in the first century: prehistory
   runs only the land, with no bad harvests or plague, so year 0 starts
   above what these conditions allow. Running the weather and plague in
   prehistory would remove the drop.
2. Land growth is .005 a year at most (step 1); real populations
   recovered at about 1% (after the Black Death in 100-150 years). Land
   refills slowly here, so few golden ages turn to strife (11 of 37).
3. The map has no fault lines: quakes strike any hills or mountains.
4. A returning plague is the first plague kind, not the strain last seen
   on that land.
5. Fewer people means fewer cities: creed splits fell from 9 to 2 and
   creeds from 32 to 24.
6. What a plague does to labour (wages up, land cheap, lords pushing
   back, the revolts of 1358 and 1381) is not modeled.
7. `dearth` lasts 2 ticks so it is felt from one harvest to the next
   (`lasts: 1` is gone before the next year's land phase).

## 0.51.24: tongues and faiths between realms

In real life:
- A shared tongue makes trade easier (lingua francas: Aramaic, Greek,
  Arabic, the Hanse's Low German); merchant networks of one faith or
  people (Jewish, Armenian, Muslim traders) trust each other.
- A shared faith makes alliances, royal marriages and aid easier
  (marriages across faiths were rare among Christian and Muslim courts);
  peace is harder to make across faiths. A different faith gives a war
  its justification (crusade, jihad), though most wars were still fought
  between realms of one faith.
- Conquerors absorbed kindred land (the English and Frankish kingdoms
  from their neighbours) and held other peoples as vassals or tributaries
  (the Mongols' Rus, the Ottomans' Danubian princes).

Built: `shared_tongue`, `shared_faith` links (overlays.gbln, `kindred`),
set each year as lasting links between bordering realms: half the overlap
of their peoples' shares, half whether the courts agree. Read by trade
(`.8 + .4 × tongue`), aid and ally (`.6 + .8 × faith`), treaty and
sue_for_peace (`.7 + .6 × faith`), match (`.5 + faith`), war (`1 + (.5 -
faith) × spirituality × .6`), denounce (`1 + (.5 - faith) × spirituality ×
.5`), annex and vassal odds (`akin` = closeness `+ (tongue + faith - 1) ×
.2`). Every factor is 1 at .5. A first try averaged closeness with the two
links, which lowered it for nearly every pair: vassalages went from 23 to
176.

500 years: 29 polities at the end (0.51.23: 34), 18.2% of polity-years at
war (15.7%), 53 vassalages (23), 192 absorptions (199 in 0.51.22). At the
end 59 bordering pairs: 5 share a tongue, 3 a faith; both wars under way
are between faiths.

Flagged:
1. Few neighbours share a faith late in the run (3 of 59): faiths split
   into ~50 small ones and none spreads across many realms, so faith acts
   mostly as a difference. Universal missionary faiths (the faith-traits
   item) would make blocs of realms of one faith, as in real life.
2. Only bordering realms are linked; trade partners further away are not
   (real merchant diasporas reached across seas).
3. Breakaways do not yet seek a neighbour of their own faith or tongue,
   and a realm does not yet claim land of its own people (that is mostly
   a modern, national idea).

## 0.51.25: a few great faiths and tongues, many small ones

Daniel 11-10 23:43: by year 500 faiths and tongues should look very
different, some very strong and some not, and lands can speak many
tongues. Measured at 0.51.24, year 500: 46 tongues and 55 faiths, the
largest of each held by 6.5% and 7.7% of all people, all of similar size;
30 of 395 lands with two tongues each held by a fifth or more. The top
faiths at year 300 were all year-0 cults and folk faiths: founded faiths
were crushed by the court's pressure everywhere.

In real life:
- The great faiths spread from the top: Constantine, Ashoka, Clovis,
  Vladimir, Mieszko, the Khazar khagans, the Mongol khans; a ruler took
  the faith of a stronger neighbour or one that had grown in his realm,
  and the people followed over generations. Neighbours converted as
  their neighbours had (alliances, marriages). Old-faith reactions came
  within a generation where few had converted (Poland 1030s, Hungary
  1046).
- Universal faiths with scripture and clergy (Christianity, Islam,
  Buddhism, Manichaeism) seek converts; the gods of a people do not and
  give way when the court converts. Universal faiths grew first in towns
  (early Christianity about 40% a decade; `pagan` meant country people).
- Tongues spread mostly by settlers (Slavs, Saxons, Bantu) and by
  empire: officials, garrisons and towns first (Latin in Gaul). A world
  faith's scripture carries its tongue (Arabic, Church Slavonic, Pali).
  Without a prestige gap, lands stay bilingual for centuries (Coptic and
  Arabic, Greek and Aramaic, the Balkans).

Built: Faith `seeks` and `tongue`; `missions × (.1 + 1.8 × seeks)`;
`standing` = 1 + 2 × world share (+ 3 × court pull) + 2 × seeks × pace
for faiths + the holy-tongue weight for tongues; shift speed `× (.2 + 1.6
× |status gap|)` for tongues and faiths; `convert` at `.03 × pull × (1 -
own seeks) × (.5 + openness)`, pull = neighbours' `seeks × power share`
summed per faith or `seeks × 2 × share` for a faith inside the realm
(5%+), × `(.3 + 2 × world share)`; reaction `.02 × (1 - court faith
share)` while under .3 and within 40 years; `land::carry` on every move;
court tongue and faith arrive in towns at .05 (.05 a year), a world
faith's tongue at .05 (.03 × seeks).

500 years: the largest faith 27% of all people, then 18%, then under 5%
each (101 faiths, most tiny); the three largest were founded in years
38-147. 58 court conversions, 4 reactions. 37 of 68 bordering pairs
share a faith. Lands with two tongues at .2+: 59 of 397. Tongues 40, the
largest 8.5%. 217 s.

Flagged:
1. Tongues stay spread out (largest 8.5%). In real life a tongue gets
   very big by a long-lived large empire (Latin, Arabic, Chinese) or by
   settling; here no realm holds more than about 15% of the land and few
   last centuries, so no tongue is pressed on many lands for long.
   That is the political balance (whole-sim tuning).
2. No creed split this run (the check fails): creeds need cities and
   there are fewer since 0.51.23 (15 creeds at the end, 19 copies at
   year 150). Creeds need another look in the tuning pass.
3. Year-0 faiths are all cults and folk faiths; world faiths only come
   from groups founding them. A world could start with one already.
4. An action named `lower` (or `raise`) cannot be called with three
   arguments (P0715, the parser treats `lower(` as the builtin): renamed
   `cut_down`/`firm_up`. A user action shadowing a builtin name should
   either work or give a clear error.

## Faith traits: militant and merchant faiths (0.51.27)

Real life first:
- Some faiths call the faithful to fight for them: the first caliphs'
  conquests, the crusades and the Reconquista, the Teutonic Order in
  Prussia, the Aztec flower wars, the war gods of the Norse. Where the
  faith is militant and its clergy hold the realm, faith, court and army
  fuse and expand (the early caliphate, the Papal States, the Teutonic
  Order's state).
- Conquerors of another faith brought their priests with the army
  (Charlemagne and the Saxons, the Order in Prussia, the Spanish in the
  Americas), and forced faith was resented and rebelled against.
- A holy war lost can discredit the call to arms (the crusades after
  Hattin and Nicopolis, the Ottoman ghazis after Vienna); a holy war
  suffered can raise the defenders' own zeal (Zengi and Saladin after the
  first crusade).
- Reforms can take up the sword (Hussites, Wahhabis) or lay it down
  (Mennonites, Quakers).
- Some faiths travel with merchants and their law suits trade: Islam on
  the Indian Ocean, Buddhism on the Silk Road, the Jains, the Armenian
  and Jewish trading houses. Merchants of one faith trade more with each
  other.

Built: Faith `militancy` and `mercantile` (0 to 1). New faiths draw both
at random 0 to .7; a kingdom's old cult militancy 0 to .5 (war gods),
mercantile 0 to .2; folk faiths 0. A schism's new faith moves militancy
up to .3 and mercantile up to .2 either way. `missions`: the land's trade
counts up to three times more for a merchants' faith (`× (1 + 2 ×
mercantile)`). Each year each realm gets `holy` = the court faith's
militancy at the capital × (.5 + clergy), and `faith_trade` = its
mercantile × (.5 + .5 × the share of the people holding it) (`zeal`).
War on a realm of another faith is likelier (`× (1 + (.5 − shared_faith)
× holy × 1.2)`, 1.6 at most, .4 against one of the same faith); trade
with one of the same faith is likelier (`× (1 + faith_trade × (.2 + .6 ×
shared_faith))`). A war is holy when its attacker's `holy` is .3 or more
and the two share less than .5 of a faith. A holy war won with land
taken: by chance `holy`, the land gets the conqueror's faith at `.3 ×
holy` and the conqueror is stirred by `.2 × holy`. Defender beaten: by
chance its spirituality, its faith's militancy +.2 (`rally`). Attacker
beaten: by chance 1 − its spirituality, −.2 (`discredit`).

500 years (seeds 42 / 7 / 99): holy wars 36 / 43 / 132, holy conquests
6 / 6 / 7, rallies 8 / 12 / 14, discredited 0 / 1 / 2. Faiths at the end
militant (.5+) 7 / 12 / 22, mercantile 15 / 17 / 13. Realm-years with
`holy` .5 or more: 2 / 0 / 225 (none of them theocracies); realms
trading for their faith (.5+) at the end 0 / 9 / 0. Years at war 18.7% /
16.4% / 16.1% (before 17.4% / 14.6% / 14.8%).

Flagged:
1. Every coefficient above is mine (the founding draws, 1.2, .2 + .6,
   × 3 for missions, .3 and .2 × holy, ±.2, the .3 and .5 lines).
2. A first version faded militancy by .996 a year on every copy; over
   1,200 years of prehistory that left every faith at almost 0, so no
   holy war ever started. It was replaced by the two war outcomes
   (rally, discredit).
3. The fused state is rare and never a theocracy: a theocracy needs
   clergy over .4, and its court faith is seldom militant. In real life
   the faithful of a militant faith fight (ghazis, crusaders): the
   faithful group could take its militancy from its faith. Not built.
4. No counter-alliance against a holy aggressor (the leagues against
   the Ottomans) beyond what shared faith already does for alliances.
