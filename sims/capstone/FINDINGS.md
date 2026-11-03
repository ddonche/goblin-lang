# Capstone

## Step 1: land and people

`world.gbln` builds a continent from a seed (`map.gbln`), makes one `Land`
province per land cell with a `border` link to each neighbour
(`land.gbln`), and runs years with `:tick()`. People grow toward the food
their land gives (logistic, 0.5% a year) and move to bordering land with
more room through the `settle` decision. They start in 20 homelands, the
best-fed provinces with no two side by side.

Run it from this folder: `goblin run world.gbln` (`YEARS=n` for a shorter
run). `check.sh` runs 3,000 years.

### Result (seed 42, 400 provinces)

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

### Fixed in the runtime on the way (each its own PR)

1. Lists held copies of objects. A loop over a list put an old copy back
   over the live object (#163).
2. Finding the provinces linked to one checked every object: 210 ms a year
   for `:objects`, 170 ms a year for the migration decision. Now both look
   only at the linked objects: 14 ms and 4 ms a year (#164).
3. A decision in a class from an imported module could not call that
   module's actions (#165).

### Missing (named, not worked around)

- **Instrumentation.** Not available: peak RSS, live DES memory apart from
  RSS, counts of objects and overlays created and destroyed, and time per
  DES pass (growth against migration). The CSV has what exists now: RSS
  from `:mem_total()`, live counts, and time inside `:tick()` against the
  rest of the program. (Since then `:metrics()` gives all of them.)
- **`capacity` can't be a field name.** In a class, `capacity: N` is the
  container-capacity clause, so the next field fails with P0910 ("only
  'act', 'action' ... allowed here"). The sim calls the field `food`.
- **A map read can't go in a string** (`"{cfg{"k"}}"`): bind it first.
  Already known.
- **Sea lanes.** Links can carry them; the map does not make them yet.

## Step 2: kingdoms and war

`kingdom.gbln` adds the `Polity`: a kingdom owns provinces through their
`owner` field, and a vassal is a kingdom whose `owner` is its overlord.
Each year a kingdom counts its provinces and people, and moves its capital
to its most populous province if it lost the old one. A kingdom with no
land is erased (`transition erase`), and its vassals go free.

War is a decision on `Land` (`invade`): a province a kingdom owns looks at
the bordering provinces of other realms. Its kingdom's strength there is
its people times its cohesion, halved for every `reach` provinces (4) from
the capital. Free land holds out with its own people. A war starts with
odds of `war_rate` (2%) a year when the attacker is at least `boldness`
(1.5) times as likely to win as to lose. The attacker wins with that
chance, takes the province, and both provinces lose 5% of their people. A
kingdom that loses its capital and keeps other land becomes the winner's
vassal.

The map is cut into 8 bands of 50 provinces, west to east. Each band is a
region actor, and a kingdom runs in its capital's region. Conquest across
a band edge is a field write to another region's province, which arrives
the next year.

### Result (seed 42, 3,000 years)

- 10 of the 20 kingdoms fall. 8 more end as vassals, which leaves 2
  free realms. The first fall comes in year 393. Most fall between 500
  and 900, while settlers fill the land.
- **The distance penalty caps size.** The largest kingdom stops at 196
  of 400 provinces, its farthest 15.7 provinces from its capital. With
  the penalty switched off (`REACH=1000`), one realm takes all 399
  settled provinces, 329 of them its own.
- Population behaves as in step 1. River valleys fill first (year
  1,200), and the world levels off at 154,908 people.
- **After about year 2,000 the map stops changing.** Once the land is
  full, no border has odds good enough for a war, and nothing in step 2
  weakens a kingdom from inside. That is step 3's job: cohesion, crisis
  and secession.

### Speed and memory (4 cores)

- 3,000 years in 15.3 s on 4 threads, which is 196 years a second (5.1 ms
  a year). On one thread it takes 40.1 s, so the regions get 2.6 times as
  much done on 4 cores. Step 1, in one program with no war, took 18 s.
- Thread time is spent in provinces' decisions (26.7 s, migration and war
  together), provinces' `every` blocks (3.4 s) and kingdoms (1.3 s).
- RSS levels off at about 140 MB after year 500 (step 1: 33 to 58 MB).
  Each of the 8 regions has its own session, holding copies of the other
  regions' objects. Live DES memory stays flat at 0.69 MB. 410 objects
  are live, 420 were made, and the 10 that are gone are the fallen
  kingdoms.

### Fixed in the runtime on the way (each its own PR)

1. A world could not be built from objects the program made. `:move!`
   before the first `tick(regions)` was C0125 ("there are none here"),
   and `:actor(state)` refuses objects that a list or variable still
   holds. Now `:move!(obj, region)` before the first tick puts the object
   in that region.
2. A class's `every` block, decision or `judge`, written in a module,
   could not call an action of a module that module imports
   (`kingdom::war`). The call read nil. It now works, both on its own and
   in a region. An action like that which only reads also keeps the link
   narrowing from step 1: the war decision went from 127 to 7.6 ms a year
   on one region.

### Missing (named, not worked around)

- **History stops once the map fills** (see above). Step 3 adds cohesion
  that wears away, crisis and secession.
- **A vassal is never freed** except when its overlord falls. Secession is
  step 3.
- **Two wars on one province in the same year:** each region's write
  arrives the next year, and the last region in the order wins. It is
  deterministic, but no battle decides between them.
