# Town on DES: what is built and what still stops it

`town.gbln` is the town written with DES. Residents, businesses and district
funds are classes. Each district is a region, and `tick(districts)` runs a
day. It is 377 lines; the plain-map town in `sims/town` is 841.

What it does now:

- **Residents** age every day, shop every day (decision `shop`), take a job
  at a business that is hiring (decision `work`), pay rent to their
  district's fund every 30 days, and die (transitions).
- **Businesses** update whether they are hiring every day, and pay their
  staff every 14 days. Staff can live in any district. When a business is
  short of cash it lays off its newest staff, and it shares spare cash as a
  bonus. A business with no staff and no cash closes.
- **Funds** pay a weekly benefit to adults without work, and spend what
  they hold above 20,000 at their district's open businesses.

`check.sh` runs 60 days and checks four things: the output is as expected,
two runs match, 1 thread matches 4 threads, and with one district (so no
money is in flight between districts) the money total never changes.

## Gaps

| # | Gap | Status |
|---|-----|--------|
| 1 | A class can't run a rule every tick | **Built**: `every N ticks ... end` (PR #68) |
| 2 | A class can have only one decision | **Built**: several named decisions, `judge using <name>` (PR #69) |
| 3 | Scores on real-value (`~`) fields collapse to 0 | **Fixed**: they are measured against the candidates; the town scores shops on `price` |
| 4 | Nothing happens when an overlay ends | **Built**: `on end: action` on an overlay; the town has births (overlay `Pregnant`, 270 days, then a baby) |
| 5 | An erased object takes its fields with it | **Built**: `on erase: action` on a class; a resident's money goes to their district's fund when they die |
| 6 | Objects can't move between regions | **Built**: `:move!(obj, region)`; now and then an adult moves district |

## Found while building this

- **Fixed: class methods couldn't read program variables** (PR #70). The VM
  read `adult` inside a method as a field of `self` and got nil. The
  interpreter reads the program's variable.
- **Fixed: looping over `:overlays()` deleted the overlays' hosts** (PR #67).
- **Fixed: `margin` was one number for all of a class's decisions.** A
  decision can now carry its own: `shop | decision against Business margin .8 by [...]`;
  without one it uses the object's `margin` field, as before.
- **Fixed: a decision's action failed silently.** An error inside `buy` or
  `apply` was ignored, so a typo there showed up only as "nothing happens".
  It now stops the tick, as an error in an `every` block does. This also
  showed that the VM never ran an action written inside the class
  (`act govern(target)`); it does now.
- **Fixed: `and` inside `:builtin(...)` parentheses didn't parse**, so
  `:objects(it >> district == d and it >> age > 18)` had to be a loop
  with an `if`.
- **Open: `overlay X on r for N ticks` takes only a number**, not an
  expression, so residents who are already pregnant on day 1 all start
  halfway (135 days) instead of at a random point.

## The 365-day run

Nearly every adult has a job, all 50 businesses stay open, and the
poorest fifth averages 123. Ten babies were born (eight to mothers who were
already pregnant on day 1), nobody died this year, and adults moved district
93 times. No money leaves the town: the total is short only by what is in
flight between districts. It drops most on paydays (day 210 is one), because
wages to staff in other districts arrive the next day.

```
town on DES: 1000 residents, 50 businesses, 4 districts, 365 days
money at the start: 853247
day 30: population 1000, adults 736, working 736, open businesses 50, money 833350; born 0, died 0, moved 9
day 60: population 1000, adults 740, working 740, open businesses 50, money 842634; born 0, died 0, moved 18
day 90: population 1000, adults 741, working 741, open businesses 50, money 842824; born 0, died 0, moved 26
day 120: population 1000, adults 744, working 744, open businesses 50, money 843436; born 0, died 0, moved 33
day 150: population 1008, adults 745, working 745, open businesses 50, money 848905; born 8, died 0, moved 41
day 180: population 1008, adults 746, working 746, open businesses 50, money 853127; born 8, died 0, moved 48
day 210: population 1008, adults 747, working 747, open businesses 50, money 793967; born 8, died 0, moved 53
day 240: population 1008, adults 748, working 737, open businesses 50, money 835519; born 8, died 0, moved 61
day 270: population 1008, adults 749, working 740, open businesses 50, money 843524; born 8, died 0, moved 72
day 300: population 1010, adults 753, working 753, open businesses 50, money 843402; born 10, died 0, moved 83
day 330: population 1010, adults 755, working 755, open businesses 50, money 843789; born 10, died 0, moved 90
day 360: population 1010, adults 755, working 755, open businesses 50, money 848919; born 10, died 0, moved 93
day 365: population 1010, adults 755, working 746, open businesses 50, money 833395; born 10, died 0, moved 93

=== End of run: day 365 ===
Where the money is (total 833395; 19852 in flight between districts):
  people            372500   45%
  businesses        244922   29%
  district funds    215973   26%

Wealth of the 755 adults, poorest fifth to richest:
  fifth   holds   average   lowest   highest
  1         8%       123        0       316
  2        21%       331      316       345
  3        23%       354      346       362
  4        23%       369      362       377
  5        25%       392      377       443

Businesses by price:
  price  businesses  open  staff  avg cash  avg sales
     15           6     6     70       762     125803
     16           6     6     72       844     131061
     17           3     3     40       906     139807
     18           8     8     98       799     144415
     19           5     5     65      1422     151928
     20           4     4     56      3912     160559
     21           6     6     89      8720     164137
     22           4     4     62     13276     173584
     23           8     8    122     12247     176764

Districts:
  district  people  adults  working  avg adult money    fund
         0     251     201      98%              324    57411
         1     257     191     100%              311    54694
         2     245     176      98%              303    50815
         3     257     187      99%              315    53053
```

Speed: a year takes about 10 seconds on 4 threads (about 27 ms a day; 9 s
before births and moving house). It took about 15 seconds with PRs #71
and #72; it took about 2 minutes 12 seconds before them, with the
same output. #71 stops `pick 1 from 1...200000` from building the whole
range on every call. #72 speeds up decision scoring and `:objects(...)`
queries. The plain-map town takes about 8 ms a day. What is left: copying
every region's objects to the other regions each day, and the actions that
decisions call.
