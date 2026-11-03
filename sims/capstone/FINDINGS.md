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
