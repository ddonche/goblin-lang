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
- **Aspects and interfaces don't carry `transition` or `on erase`**: the
  same two lines are in all seven classes. Fine for now, since step 3's
  transitions differ by class.
- **No sea lanes** yet (from step 1): coastal polities trade and fight
  only across land borders.
