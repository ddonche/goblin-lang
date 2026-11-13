# Changelog

The Goblin version is the version of `goblin` (crates/goblin-cli). Numbers
in parentheses are the pull requests that made each change.

## 0.51.34 — 2026-11-13

- sims/capstone: some new rulers, groups and realms are unlike those that made them (owner: some new entities, not all). One in twenty of each has one trait drawn anew from anywhere between 0 and 1, instead of near its source:
  - a ruler, unlike their kin and culture (Akhenaten, Caligula, Ivan the Terrible, Peter the Great), told when crowned (`unlike` in the families line);
  - a group, unlike others of its kind (the Zealots, the Assassins, the Anabaptists of Münster), told when formed (`unlike` in the groups line);
  - a colony or a land broken away, which takes one part of its culture anew, trait and baseline both (the Dutch Republic's tolerance, the American colonies against centralizing), told as `unlike_realm`.
  Every other new entity is still drawn near its source as before. Faiths already draw their militancy and trade at random. The one-in-twenty chance is chosen, not measured.
- check.sh: also checks that some rulers and groups are unlike, and prints how many realms were (80 checks).
- Capstone, 500 years (seed 42; seeds 7 and 99 in parentheses): 58 unlike rulers of 1,172 crowned (66 of 1,324, 67 of 1,265), 36 unlike groups of 704 (26 of 713, 46 of 782), 4 unlike realms (7, 1); 11.7% of polity-years at war (10.6%, 11.9%); empires last a median 193 years (157, 170). Expected history regenerated. Every check passes.

## 0.51.33 — 2026-11-13

- sims/capstone: each realm's own year's accounts (its food, gold, trade, dues, tribute to its overlord and its traits' drift) run where the realm runs, on its region, at the start of the `inside` phase, instead of in the program. The program still counts each realm's land first, and the year's ideas have bent the realm's culture (`bends`, from its groups and from how much of its people share its court's tongue and faith) before its traits drift.
- sims/capstone: the realms' shared tongue and faith are worked out for the year's bordering pairs only (from the border list `polity::account` makes), instead of testing every pair of realms for a border. The history is the same.
- sims/capstone: schools of thought split ten times as often. They split about once in 1,500 years before, against the comment's "often" and real life (the Confucians split into eight schools within two centuries of Confucius, the early Buddhists into eighteen); now 1 to 10 times in 500 years. The `creed_split` check had been passing by luck.
- Capstone, 500 years (seed 42; seeds 7 and 99 in parentheses): 11.4% of polity-years at war (9.7%, 10.3%); empires last a median 114 years (128, 155); 18 coup attempts, 10 won (10/8, 23/15); 138 realm-years fighting for their faith (34, 2,480): that last number turns on whether a strongly militant faith appears and spreads. Expected history regenerated. Every check passes (78).
- Speed, 4 cores, 60 years (seed 42), the median of four runs of each, interleaved on one machine: 24.5 s, from 26.1 s (6% faster; single runs vary by up to 10%).

## 0.51.32 — 2026-11-12

- DES regions: what the program sets on a region's object now reaches every region's copy in the same round, not only the owner's. Before this, a region far from the object read last round's value (in the capstone, last year's year in the first phase).
- DES regions: an overlay kind made while a world runs (a new faith in one region) reaches every region with the next round, not only with its overlays. A region that met such a kind only by name (`:overlays(l, name)` with the name read from a field) stopped with R1231 before this.
- sims/capstone: the yearly account of each province (its tongues and faiths shifting, the court's tongue and faith reaching its towns, plague, weather, disasters, famine) runs on the regions, in the `ideas` phase, instead of in the program. What it needs from the whole world is kept in objects: the year's weather and dark years and each idea's reach on `land::Clock`, each realm's court tongue and faith and how much of the realm holds them on the realm (`court_tongue`, `court_faith`, `court_share`, `court_fshare`). The program still sums the realms' people by tongue and faith.
- sims/capstone: the regions are cut so each holds about as many provinces (they were cut by equal widths of the map, which gave one region 42 provinces and another 157, and every round waited on the biggest).
- Capstone, 500 years: the history is new (the program's writes reach the regions sooner). Seed 42: 12.1% of polity-years at war, 29 empires lasting a median 152 years, 14 coup attempts, 8 won, no realm fighting for its faith; seeds 7 and 99: 11.1% and 11.4% at war, empires a median 253 and 208 years, 3,529 and 577 realm-years fighting for their faith. Expected history regenerated. Every check passes.
- Speed, 4 cores: 100 years (seed 42) in 49 s, from 52 s; 30 years in 15.5 s, from 17.2 s. The program's accounts of realms, groups and families still run on one thread between phases.
- Conformance: new case `world_tick_kind_known_by_name`.

## 0.51.31 — 2026-11-12

- An overlay class can have members in other files: `overlay calm <^> Mood` in a file that imports the one declaring `<^>Mood |` (at the top level or inside an action) gets the class's lines it doesn't write when its `overlay` line runs. A member of a class no file run so far declares is R1296 there. A member inside a block above its class in the same file is still P1295.
- An overlay's `on mutate` block gets the copy it became as `mutated` (its kind, intensity and fields), as `on split` gets `first` and `second`. sims/capstone's new tongues and faiths use it instead of looking for the copy among the land's overlays.
- A DES condition that fails is an error, not false: an object's or overlay's `transition ... where`, a transition's target test, an `erase` condition and a judge arm's condition that read a field the object doesn't have stop the tick with R0403 (as decisions' `where` and `score` already did). A spread grouped by a field falls back to the plain run when a host lacks the field, so it errors the same way. A condition that gives something other than true, false or nil is R1262.
- Every sim's check passes unchanged; capstone history unchanged.

## 0.51.30 — 2026-11-12

- sims/capstone: coups, from real life (owner: "whatever is most realistic"). A group's coup is now an attempt that succeeds or fails at once, as about half of real attempts do (Powell and Thyne, 1950 to 2010): the plotters' armed men are matched against the guard (a twentieth of the realm's soldiers), and fewer of the guard stand by a ruler whose right is doubted or in an army without discipline. A won coup puts the officers in power (a military junta); a coup against a junta puts other officers in its place (a counter-coup). A failed coup costs the plotters half their members and frightens the ruler (`coups_failed`). One coup invites the next (the coup trap, Londregan and Poole): a realm's recent coups (`coups`, fading by a tenth a year) make the army's coup and a group's plot up to three times likelier. The realm's `plot` field is gone.
- Capstone, 500 years (seed 42; seeds 7 and 99 in parentheses): 21 coup attempts, 14 won (22/12, 8/6; 32 of 51 in all, 63%); 10 military juntas at the end (10, 7); 10.6% of polity-years at war (9.7%, 10.8%); empires last a median 212 years (163, 121). The guard's size and the coup trap's strength are chosen, not measured. Every check passes. Expected history regenerated.

## 0.51.29 — 2026-11-12

- DES regions: what crosses from one region to another now works for everything the capstone uses.
  - Changing a field of an overlay copy on another region's object (`o >> f |= v`, on a copy from `:overlays(host)`) goes to that region, which changes its copy at the start of the next round. The one that wrote it reads it back at once. Before this it was R1249.
  - Links are kept with the object they go from, in that object's region. Other regions and the program that runs the world read them as they were at the end of the last round. A `:link!`, `:unlink!` or `link to ... offset` on another region's object goes to its region. Before this, each region saw only its own links, and a link the program set never reached the regions (in the capstone, realms in regions saw no borders, so no wars).
  - A whole number the program changes on a region's object goes to it as what it added, when the next round starts, so the program and the regions can count on one object. Any other field goes as the value set, when it is set.
  - An `overlay` line run while regions run (a new faith, a new creed) makes a kind with a name of its own, numbered by region, so two regions never make two kinds of one name. The kind goes along with its overlays to regions that have not seen it. Before this, a region that had not seen the kind stopped with "unknown overlay".
  - `:move!(obj, actor)` with no world yet starts one. Narrowing (a transition's targets) works in regions. Imports are in each region's image. The world's tick goes on from the ticks already run.
  - Speed: the program's `:overlays(host)` and its writes to region objects use indexes built once per round. Without them, the capstone ran nine times slower on regions.
- sims/capstone runs on region actors, as the town, feudal and epidemic sims do. There are `REGIONS` (default 4) bands of the map, west to east: a province goes by where it lies, a realm by its capital, a ruler and a group with their realm, a house with its first ruler. Anything made later is placed next to what it belongs to (`land::place`). `REGIONS=1` runs without regions.
- sims/capstone: what every region must share is kept in objects (owner, 2026-11-12):
  - the year: `land::Clock`, set by the program as each year begins;
  - the tallies: `FamilyRecord`, `GroupRecord` and `IdeaRecord`;
  - each realm's fears and how full its land is: `Polity.fears` and `Polity.fullness`, replacing the module maps `feared` and `fullness` (golden ages never turned to elite strife in a region before this);
  - the name counters: a name is drawn at the realm's capital (`Land.named`, `land::name_no`), so two regions never draw the same one.
- Conformance: 4 new cases (VM): `world_tick_links_across_regions`, `world_tick_overlay_fields_across_regions`, `world_tick_program_counts_add_up`, `world_tick_new_kinds_across_regions`. check.sh also checks that a run on one thread gives the same history.
- Capstone, 500 years (seed 42) on four regions: 11.7% of polity-years at war; 34 empires, a median of 93 years from becoming an empire until gone; 120 coalitions, 176 submissions, 19 holy wars, 43 golden ages turned to elite strife; 8 coup plots, 1 won. The same history on one, two or four threads. The history is new (names are drawn differently and each region rolls its own dice). Expected history regenerated. Every check passes (78). Time on 4 cores: 100 years in about 50 s on four regions, 104 s on four regions on one thread, 65 s with no regions. The yearly accounts still run in the program, on one thread.

## 0.51.28 — 2026-11-12

- sims/capstone: balance pass, from the map of what drives what (goblin-docs/capstone-behaviour-map.md). Ambition no longer pushes taxing, building, levying, borrowing or spying: taxes follow need, centralization and war, held back by doubted legitimacy; building follows stability and spare gold; levies follow fear and war; borrowing follows need and war; spying follows fear and discipline. Embargoes and shows of force follow aggression instead of ambition.
- sims/capstone: counterweights where a decision had none. A vassal revolts less when its ruler is honorable and its realm stable; a realm sure of its right concedes less to a group; loyal groups withhold less, plot less and rise less; a disciplined army is harder to turn against its realm.
- sims/capstone: traits that drove little now drive more. Stability holds back breakaways; tolerance makes a land of another people less likely to break away; discipline and a settled succession hold back coups; a contested succession invites a usurper.
- sims/capstone: coups. A plot lingers for a few years (× .6 a year) instead of lasting one; officers who plot can act without a doubted crown, and a large army against a realm can topple a ruler whose right nobody doubts (Rome's praetorians). A group's coup counts as won only when the realm becomes a military junta.
- sims/capstone: the empire check line follows a realm from becoming an empire until it is gone (taken in, merged, erased), not until it is demoted to a kingdom.
- Capstone, 500 years (seed 42; seeds 7 and 99 in parentheses): 9.8% of polity-years at war (11.6%, 8.9%; was 13.9%, 14.4%, 13.1%); 8 coup plots, 1 won (10/0, 16/0); 7 military juntas at the end (8, 4; was 1, 1, 0); 80 withholds (was 256); 38 risings (was 51); 164 revolts (was 178); 286 breakaways; empires last a median 232 years until gone (166, 102). Every coefficient here is chosen, not measured. Every check passes. Expected history regenerated.

## 0.51.27 — 2026-11-12

- sims/capstone: militant and merchant faiths, from real life. A faith has `militancy` and `mercantile` (0 to 1): new faiths draw both at random, a kingdom's old cult may have war gods, folk faiths have neither, and a schism moves them either way. A merchants' faith travels further along trade (`missions`). Each year a realm fights for its court's faith as far as the faith is militant and its clergy hold the realm (`holy`), and trades for it as far as the faith is mercantile and its people hold it (`faith_trade`). `holy` makes war on a realm of another faith likelier (up to 1.6 times) and on one of the same faith less likely; `faith_trade` makes trade with a realm of the same faith likelier. A war by a realm with `holy` .3 or more on one of another faith is a holy war: land it takes may be converted at the sword's point, which stirs the conqueror's realm; the beaten defender's faith may take up the sword in turn (`rally`), and a beaten attacker's may lose it (`discredit`). Believers take up arms: a group holding a faith, and a holy order for the court's faith, drift toward that faith's militancy, and their hold counts more in the realm's `clergy` (× 1 + .5 × militancy), which opens the way to a priests' takeover. New check line: militant and mercantile faiths at the end, and realms fighting or trading for their faith (and how many years, and how many of those ruled by priests).
- sims/capstone: leagues against a great threat (owner, 2026-11-11). Each year a realm sees a threat in each stronger neighbour, as far as it has lately seized land by war or fights for another faith (`menace`, Polity `seized`). Realms that fear the same one ally even when they are not neighbours, and old grudges count for less (`shared_threat`); the alliance fades as before once the threat is gone. Counted as `coalition`.
- sims/capstone: a realm weighing war counts each side's allies (`war_edge`: allies' power × their honor), as far as it fears them (× 2 × fear): a cautious ruler is held back by a league, a reckless one is not. A feared neighbour may be struck first (`threat × fear` as a cause of war; counted as `struck_first`).
- sims/capstone: a realm clearly losing a war may offer to serve its enemy rather than be taken (`submit`, a realm decision): likelier the further behind, afraid and weary it is, less likely the more ambitious and proud. An aggressive, ambitious enemy may refuse; otherwise the war ends and the realm becomes its vassal.
- sims/capstone: empires fall from within, along their peoples. The province likeliest to break away is more often one held by another tongue or faith, and breaks away more readily; such a people takes all its joined land with it. Each breakaway leaves the old realm `crumbling` for about a decade (legitimacy and centralization down), so the next one comes easier and the center lets lands go more readily. In a loose realm a breakaway of the court's faith may stay its vassal in name. The old realm keeps a claim on the lost land for a century (a cause of war). New check line: how long empires last.
- sims/capstone fixes: a land could send off more people than it had after a failed harvest (now at most its `leaving` share a year); a blend of two faiths or tongues could be held by more than 1 (now at most 1). Either could stop a run.
- Capstone, 500 years (seed 42): 48 empires, median 56 years as an empire, longest 417; 44 peoples broke away, 7 kept the oath in name; 202 submissions, 121 coalitions, 58 holy wars; 13.9% of polity-years at war (17.4%). Every check passes. Expected history regenerated.
- Earlier on this branch (seed 42, before empires fell from within): 188 submissions (71 refused), 242 vassals made (52 before), 166 realms taken in (202), 178 revolts (108); 119 coalitions; 134 holy wars; 14.6% of polity-years at war (17.4%). Every check passes (a creed split happened this run). check.sh checks the `submit` decision. Expected history regenerated.
- Earlier on this branch (seed 42, before coalitions): 119 holy wars, 12 holy conquests, 17 rallies; 108 realm-years fighting for their faith; 18 militant and 5 mercantile faiths at the end; 16.9% of polity-years at war (17.4%). A creed split happened this run, so every check passes. Expected history regenerated.

## 0.51.26 — 2026-11-11

- Builtins always take their colon, and your own actions never do (owner, 2026-11-11). `:lower(x)` calls Goblin's builtin; `lower(x)` calls an action of yours named `lower`. "Builtin" means anything Goblin provides, so `say` is `:say`, and `tick()` is `:tick()`, also with its block of phases (`:grid(...)` with its `tile`/`regions` block likewise). A bare call to a builtin's name that is no action of yours is R1260 ("no action named `lower`. `lower` is a Goblin builtin: write it `:lower`"); a colon on your own action, or on a name Goblin doesn't provide, is R1261. A variable that holds a function may still share a builtin's name. Statement words (`if`, `repeat`, `judge`, `collect`, `attempt`, ...) and sentence forms (`pick 1 from xs`, `roll 2d6`, `raw "..."`, `sweep`) are syntax and take no colon. A bare `tick()` block is P0717, a bare `grid(...)` statement P0718. Before this, a colon on your own action called the action and a builtin's name without one called the builtin.
- `goblin fix colons [--check] [--md] <dir|file>...` rewrites older code: it adds the colon to builtin calls, drops it from calls to actions the file declares, fixes code inside `{...}` in strings, Goblin samples in Markdown (`--md`) and programs inside Rust test strings, and lists what it leaves alone (a name that is also a variable). Run over this repository: 1,593 Goblin files, 10 Rust test files and 4 Markdown files.
- Parser: a colon that starts a new line begins a new statement (`:say(r)` right after a `judge` block was read as a colon-call on the judge).
- Sheriff (13 files) and Campfire (7 files) were rewritten with the same command on their own branches; Sheriff's built site and 30 Campfire requests are identical before and after.
- Conformance: six `colon_rule_*` cases (VM; the interpreter does not enforce the rule). The interpreter is unchanged.

## 0.51.25 — 2026-11-11

- sims/capstone: how faiths and tongues come to be a few great ones and many small ones, from real life. A faith now has `seeks` (0 to 1): the gods of a people and old kingdom cults seek no converts; a faith with a founder mostly does (.3 to 1), sends missions in proportion, and has the appeal of its scripture and clergy, most in towns (`standing` adds 2 × seeks × the size of the place, 1 to 3). Rulers convert (`convert`): to the faith of stronger bordering courts (several of one faith pull together) or to a faith that has won a twentieth of the realm, likelier for a faith held by more of the world, an open ruler and a court whose own faith seeks no converts; the capital takes it and the court's men bring it into the towns. Counterweight: an old-faith reaction within 40 years where few have converted. Settlers carry their tongue and faith (`land::carry`). The court's officials bring its tongue and faith into the towns it rules; a world faith's clergy bring its scripture's tongue (`tongue`), which has standing where the faith is held. Shift between tongues or faiths is driven by the gap in prestige; numbers alone shift people slowly, so a land without a prestige gap stays bilingual. Prestige also grows with a tongue's or faith's share of the world.
- Capstone, 500 years: the largest faith holds 27% of all people (was 8.5%), 58 courts converted, 4 reactions; bordering realms sharing a faith 37 of 68 (3 of 59). Lands with two tongues held by a fifth or more: 59 of 397 (30). The largest tongue holds 8.5% (5.7%). No creed split in this run (creeds 15 at the end; they need cities, fewer since 0.51.23): the creed-split check fails (the only one). check.sh: an overlay count is matched as a whole word (`converted` also matched `court_converted`). Expected history regenerated.

## 0.51.24 — 2026-11-10

- sims/capstone: what tongues and faiths do between realms. Each year two lasting links are set between every pair of bordering realms, `shared_tongue` and `shared_faith` (0 to 1): half how much their peoples hold alike (the overlap of their shares), half whether their courts hold the same one (`overlays::kindred`). A shared tongue makes trade likelier (.8 to 1.2 times). A shared faith makes alliances and aid likelier (.6 to 1.4), treaties and suing for peace (.7 to 1.3), royal marriages (.5 to 1.5); a different faith makes war and denouncing likelier for a pious realm (holy war, up to 1.3 times; a shared one makes them less likely). A conqueror takes in land of its own tongue and faith and keeps other peoples as vassals (`akin`: closeness moved up to .2 either way). Every factor is 1 at a share of .5, so the averages stay where they were.
- Capstone, 500 years: 29 polities at the end (34), 18.2% of polity-years at war (15.7%), 53 vassalages (23), 192 absorptions. A new check line counts bordering realms sharing a tongue or a faith and wars between faiths. Expected history regenerated.

## 0.51.23 — 2026-11-10

- sims/capstone: plague, famine, disasters and the realm moods rebuilt from how they work in real life. Bad harvests come from the weather over whole regions (6 by 6 provinces, one year in about sixteen, a tenth to a third of the harvest; on dry, open land a drought of three years), from the dark years after a great eruption over the whole world (about once in three centuries, two years), from floods (they take the harvest) and from armies fighting on the land (`dearth`, `ravage`). Famine is people over this year's harvest; it kills mostly through the fevers of hunger, less with granaries and trade, and ends with a good harvest. A plague hits cities half again as hard, also travels between the cities of trading realms, leaves an immunity that fades over a generation, and can come back once most survivors are gone (hunger makes an outbreak five times likelier). Fires in towns and cities about once in fifty years, few dead; quakes deadlier in cities. Dread also follows a great famine and the dark years, and lowers tolerance and legitimacy. A golden age needs land with room to spare (a realm under .7 of what its land feeds); `decadence` (a moralist label) is replaced by `elite_strife`, which a golden age turns into as the land fills (eight points fuller than when it began): more heirs and office seekers than lands and offices.
- Capstone, 500 years: 1,769 famines, 2,188 droughts, 626 floods, 2,523 fires, 91 quakes, 4 dark years, 17 plagues and 45 strains, 640 dreads, 37 golden ages and 11 turned to strife. People level off near 85,000 to 97,000 (before: rising to 152,000). Expected history regenerated.

## 0.51.22 — 2026-11-10

- sims/capstone: creeds rebuilt from how philosophies behave in real life. A creed's intensity is the share of a land's learned who hold it, so it lives in cities. Scholars carry it to bordering cities and to cities of realms its realm trades with, more from a city with libraries (`schools`, `traded`). Schools compete for one pool of students (`shift`, .05), so a city holds a few and the small ones die out; with no city left a creed fades, slower where a library keeps the books. Schools split where scholars gather: a creed held by .3 or more, more with libraries and under a learned realm. It no longer fades by itself in a city.
- sims/capstone: a tongue, faith or creed spreads only from land where it is held by .3 or more (people move, missions and scholars go out from where it is established). Before, every faint trace sent out copies that died and were sent again: 1,000 copies of each class by year 300, and each century took longer than the last.
- Capstone, 500 years: 9 school splits, 32 creeds at the end; 177 lands changed faith, 9 schisms, 5 new faiths, 5 blends; 96 lands changed tongue, 7 new tongues. 500 years 242 s. Expected history regenerated.

## 0.51.21 — 2026-11-10

- sims/capstone: faiths rebuilt from how faiths behave in real life (owner go, 2026-11-10). Conversion is a shift between a land's faiths, as for tongues but slower (.03), faster in towns and cities; the court's faith has standing as far as the realm holds it, pressed harder by an intolerant ruler (× 1.5 − tolerance). Preachers carry a faith along borders, more from trading land and into towns (`missions`); it no longer fades by itself. Isolated believers drift into rites of their own, slower than a tongue (`drift` to 600), and the whole cut-off region takes the new faith. Schisms split whole regions and follow politics: a region whose own ruler holds the faith as the court's, apart from the faith's heartland, builds up `tension`, more under a learned ruler, and by chance breaks away; .7 of the region follows (`faith_born`, `region`). Two faiths held side by side with neither on top blend, rarely. The random single-land sects (a faith pressed by a rival) are gone.
- Capstone, 500 years: 247 lands changed their main faith, 7 schisms, 2 new faiths from isolation, 4 blends; theocracies at the end 3 (13 before). `check.sh` checks conversions and schisms and reports blends without requiring one. No creed split happened in 500 years, so that check fails (creeds are not rebuilt yet). Expected history regenerated.

## 0.51.20 — 2026-11-10

- sims/capstone: groups tuned toward real life (owner, 2026-11-10: "let's tweak them"). Priests take the rule only where a group of believers is the strongest power and the crown's right is doubted (`takeover` reads the new `Polity.clergy`, set each year by `factions::account`, and legitimacy), and lords take it back when the clergy's hold slips. An army takes power where it is large against the realm and the crown is doubted, mostly after a defeat, unpaid, or when officers holding a twentieth of the soldiers plot (`coup` no longer needs a disciplined army); a junta can found a dynasty or a tyranny (`lords`). Groups give gold mostly when the realm is at war or short of it; they refuse taxes from grievance, hard times (the realm's `discontent`) and a weak crown; hard times raise grievance. Only the zealous found faiths and creeds. `SEED` sets history.gbln's seed (default 42). Four seeds, 500 years: theocracies at the end 7.3 to 1.8 per run, coup plots 5.5 to 47.5, gifts 6,583 to 3,421, refusals 29 to 136.

## 0.51.19 — 2026-11-10

- sims/capstone: tongues rebuilt from how languages behave in real life (owner, 2026-11-10: "think in terms of real life and then make the sim match"). 0.51.18 merged tongues 43 times in 500 years; in real life tongues almost never merge. Now: speakers shift between the tongues on a land toward more speakers and more standing (the Abrams-Strogatz model of language shift, `overlays::shift`), twice as fast in towns and three times in cities; the court's tongue has standing as far as the realm speaks it, so a tongue of a few conquerors gives way to the locals'; a tongue dies on a land when under 2% speak it. New tongues come only from separation: each tongue's speakers form regions (bordering land under one holder); every region but the most peopled one drifts a year each year (a quarter across a border, less under a learned realm), and after centuries, by chance, the whole region speaks a new tongue (`diverge`, `born`). A creole only in a trading city where two tongues meet and neither is on top. The court's tongue no longer gains by a fixed amount.
- Capstone, 500 years: 152 lands changed their main tongue, 10 new tongues, 0 creoles; 9 realms rule more than one people (18 before). `check.sh` checks shifts and new tongues and reports creoles without requiring one. Expected history regenerated.

## 0.51.18 — 2026-11-10

- sims/capstone step 6: overlays (owner go, 2026-11-10: faith transitions like any other transition, year-0 seeding as proposed, the rest by judgment). New `overlays.gbln`. On land, for centuries: `Tongue`, `Faith` and `Creed` (moved here from factions.gbln). A land can hold several of each; one that has just spread in starts weak; a tongue gives way to a firmer one beside it, faster the further behind, and the court's tongue gains on the land its realm rules; a faith fades the same way beside a firmer one. Transitions, each by chance from a cause: a tongue or faith cut off from the rest of its kind for generations mutates into one of its own; two tongues held side by side on trading land merge; a faith pressed by another held beside it fractures into a sect; a creed's scholars split into two schools where there are libraries. On land, for years: `Plague` (kills a share each year, cuts trade, spreads along borders from trading land into crowded land, mutates into deadlier or milder strains, dies out where few are left, leaves `immunity` for 40 years), `famine` (more people than the land feeds; kills, less with granaries; ends when the people are down to what it feeds), and the `Disaster` class: flood, quake, fire (kill some once, may bring down buildings) and drought (cuts food for 3 years). On realms: `dread` (after a plague, flood or quake), `discontent` (stirred by famine, taxes, a lost war and crushing a group; eased by relief, concessions and a won war; raises the unrest a breakaway reads), `golden_age` (after peace with trade and steady rule; ends with a lost war or unrest, or mutates `into: decadence`) and `decadence`. Year 0: each kingdom's land gets a tongue and a faith of its own; tribes and village communities share one tongue and folk faith with bordering ones that have them, so neighbours form clusters. Each year `overlays::account` sets each realm's `peoples` (tongues spoken by at least 15% of its people; the closeness arithmetic in conquest and union is gone) and bends its cohesion by how much of it speaks its court's tongue and its legitimacy by how much of it holds its court's faith. Step 5's random heresy chance and `Land.dominance` are gone. An overlay class is known only in its own file, so groups found new ideas through `overlays::found`.
- VM: an overlay's transition lines can read the object the copy is on as `host` (`transition erase where host >> people < 20.0`). `self` stays the copy, so `@field` reads the copy's own field. Before, a transition could not read its host at all, and a condition naming a host field (`@people`) read a field the copy doesn't have and was false with no error.
- Capstone, 500 years, seed 42: 8 new tongues, 43 merged, 8 new faiths, 35 sects, 2 creed splits; 17 plagues, 12 strains; 320 famines; 77 floods, 78 quakes, 261 droughts, 97 fires; 169 dreads, 26 golden ages, 11 decadences. 75 tongues and 45 faiths at the end; 18 realms rule more than one people. Groups now merge (3), so that check passes. `check.sh` checks that each overlay comes and transforms. Expected history regenerated.
- Capstone time, one thread: 100 years after 1,200 of prehistory 23.7 s → 36.7 s; 500 years about 150 s. Not tuned for speed (owner, 11-10: measure against Mesa and NetLogo once the sim is whole).
- Conformance: `core/des_overlay_transition_reads_host`.

## 0.51.17 — 2026-11-10

- VM speed, same results: DES formulas (a decision's `where` and score, a spread's condition, a transition's condition) that call an action or a math builtin now run on the fast formula path instead of a full call frame per target. On that path: `:abs`, `:min`, `:max`, `:floor`, `:ceil`, `:round`, `:sqrt`, `:clamp` and `:pow` on plain numbers (through the same code the VM calls); a call to an action that only reads (`short(@gold, 10.0)`), where only the action gets a frame and its answer is kept while a decision is scored, as the VM keeps it; a global holding a plain value. An action that may change things or roll dice (`chance(...)`) still sends the formula to the full VM.
- VM speed, same results: `:link_score(self, target, "border")` ran on the fast path only when its first two arguments were not the formula's own `self` and `target`, so every `where :link_score(self, target, ...)` ran on the full VM. A formula that gives back `self` or `target` itself did the same. Both run on the fast path now.
- VM: an action using `a ?? b`, or calling another module's action by its full name (`family::claims_to(d, p)`), counted as one that may change things. So a decision calling it was never narrowed to the targets that can pass (`target >> owner == self`, `:link_score(self, target, ...)`), and its answers were never kept. Both only read now: `??` asks whether a value holds something, and a module action counts by what its own code does.
- Capstone, 100 years after 1,200 of prehistory, one thread: 29.7 s → 23.7 s (decisions pass 24.0 s → 17.8 s). History output and event log identical.
- Conformance: `core/des_formula_calls_actions`.

## 0.51.16 — 2026-11-09

- sims/capstone step 5: groups and ideas (owner rulings, 2026-11-09). A group (`Group`, factions.gbln) is people of a realm who decide: a guild, a sect, a legion, a club, a gang, a cartel. The kinds are data (`factions::kinds`, 14 of them: what realm field each grows from, its share of the people, its traits, what it wants, what its members earn); the code reads their fields, never a name. Every group has the same eight actions (give gold, withhold it, give men, plot a coup, preach, found an idea, press its wants, rise), each weighed by its traits and state (`~loyalty`, `~ambition`, `~militancy`, `~zeal`, `~greed`, `~unity`, its grievance, power and holdings), so a group with no militancy never plots or rises. Realms answer their groups (`answers`, polity.gbln): crush, concede, patronize. An idea is an overlay on a land (`Faith`, `Creed`): it spreads to bordering land, fades unless preached, makes heresies (likelier under an idea held across much of the world), and pulls its realm's culture (`bends`, read by `drift_traits`). Groups form by chance from what they grow from, split when large, merge, and end; a coup plot raises `government::odds`' coup odds and a rising its breakaway odds. What they do that changes nothing modeled is told with `:event`. Numbers and choices in `sims/capstone/FINDINGS.md`; design in goblin-docs/factions-design.md.
- VM: an overlay's record from `:overlays(host)` is of its overlay class: `:is_type(o, "Faith")` is true for a copy of a kind of `Faith` (an overlay is an object of its class). Before, only its own kind's name matched.
- VM: an overlay's blocks (`on mutate => heresy(self)`, the other `on` lines) written in an imported module call that module's actions, as the rest of its code does. Before, they were compiled seeing no names at all, so a call to the module's own action failed with "value of type 'nil' is not callable".
- VM speed, same results: an overlay's `spread:` whose condition starts with `:link_score(self, target, "border")` tries only the linked hosts (it tried every host); a transition's `targets: K where (target >> f) == x and ...` tries only the objects whose `f` can match, as decision targets already did.
- Conformance: `core/des_overlay_hook_in_module`, `core/des_overlay_copy_is_of_its_class`.

## 0.51.15 — 2026-11-09

- `obj >> {name}`: the field whose name `name` gives (any expression giving text), read through the object's overlays as `obj >> piety` is; `obj >> {name} |= v` writes it (owner ruling, 2026-11-09, the braces of `import ... as {n}`). The name must be text: R0414 (`field-name-not-text`). Parser (P0406 for a missing `}`) and VM (`GetMemberDyn`, `SetFieldDyn`); the interpreter doesn't have it.
- VM: an overlay is an object, so `:objects(Government)` on an overlay class gives its overlays: declared ones and those made while the program runs, in the order they were made, each with `overlay_name`, `parents`, the `intensity` a new copy starts at, and its fields (with the class's for those it doesn't write); `:objects(Government, it >> rule == "crown")` picks among them. Their fields are what each new copy starts with: writing one is R1249 (change a copy's fields instead).
- sims/capstone: the forms of government are described by their fields and the code reads the fields, never a form's name, so a new form works with no code change (owner, 2026-11-09). Each form says who rules (`rule`), its own law (`law`), a holder's term (`term`), what a polity needs to have it (`ranks`, `city`, `overlord`, `realms`, `leagues`), whether a polity takes it by rule (`chosen`), how well it suits a polity (`fit_up`, `fit_down`: traits multiplied), the forms that carry it on (`carries_on`), whether it is a republic, whether a constitution binds its crown, whether the throne is contested, and what it can become by each kind of change (`becomes`: restore, harden, lords, coup, usurper, revolution, falls, takeover, secular, reform, empire, trade, strongman, chief_dies, with each kind's odds in `government::odds`). The lists `forms`, `monarchies`, `republics` and `carry_on` and the name tests in `allowed`, `fit`, `choose`, `restored`, `regime`, family.gbln (`kind`, `law_for`, `term_of`, interregnum) and polity.gbln (client regime, confederation, interregnum) are gone. Output identical (world, history and event log match); history 500 years 66.8 s against 68.1 s for the old code on the same build.
- Conformance: `core/field_computed_name`, `core/field_computed_name_not_text_errors`, `core/des_overlay_class_objects`.

## 0.51.14 — 2026-11-09

- A field named `order` is refused with a clear error (owner ruling, 2026-11-09). In a class, `order:` sets the order its objects make their decisions in, so `order` is never a field. Before, `order: 1` in a class quietly became that ordering line: `k <> Land | order: 5` dropped the 5 without a word, `@order` in `modify` said Land had no field named order, and a field line after it failed with the general P0910. Now: giving `order` when making an object is R0413 (`order-not-a-field`, VM); a field line after `order:` in a class is P1569 (parser, both engines); reading `order` (`>>`, `:field`, `modify`) says why it isn't a field.
- Conformance: `core/class_order_not_a_field_errors`, `core/class_field_after_order_errors`; `core/des_overlay_dup_stronger` names its field `rank` (it was written `order: 0`, which had quietly been the ordering line).

## 0.51.13 — 2026-11-09

- VM: an overlay copy's fields are written like any object's (owner, 2026-11-09: an overlay is an object). `o >> intensity |= .9` on a record from `:overlays(...)` changes that copy (kept in 0..1), as do `o >> ticks_remaining |= n` (a whole number or nil) and `o >> fervor |= x` for the overlay's own fields; its `modify` lines are worked out again at once, and the variable gets the copy as it is now. Before, every such write failed with R1240 ("an object that is no longer in the world"). The fields that say which copy it is (`overlay_name`, `host_uuid`, `host_var`, `parents`) and its `age` are the runtime's, and a copy no longer on its host can be read, not changed: both R1249 (`overlay-field-write`).
- VM: an `overlay` line makes a new overlay each time it runs, as `<>` makes a new object (owner, 2026-11-09: "each run makes its own overlay"). The first run makes the overlay it names, as before; each later run makes a kind of its own with no parent, named after its family as a transition's kinds are (`faith_1`, `faith_2`, from the same count). In the code that ran the line, the name means the one just made (`:overlay!(l, new_faith)`, `send new_faith`). Before, running the line again redefined the one kind, so every land given "a new faith" shared one. Storage unchanged (each kind is one more definition).
- Conformance: `core/des_overlay_fields_written`, `core/des_overlay_field_write_identity_errors`, `core/des_overlay_line_each_run_its_own`.

## 0.51.12 — 2026-11-09

- VM: an overlay made inside an action sees that action's variables, as other code written there does (owner, 2026-11-09: "WHY CAN SOME SHIT NOT SEE THE FIELDS OF OTHER SHIT?"). Its fields (`fervor: f >> zeal`) are worked out with the action's variables when the `overlay` line runs; before, they saw only top-level variables, so `fervor` was left out (and `modify` failed on it later) or quietly took a top-level variable of the same name. Its `modify`, `spread:`, `every`, `on ...` and transition lines see the variables too, as they were when the `overlay` line ran; an object is kept as a reference to it, so its fields are read as they are when the line runs. The overlay's own fields and the tick's names (`self`, `target`, `intensity`, `strength`, `age`) come first. An overlay made at the top level is unchanged. The VM's overlay storage is unchanged (the variables are kept on the overlay's definition).
- VM: an overlay field that can't be worked out is an error, R1233 (`overlay-field`), instead of a field quietly left out.
- Conformance: `core/des_overlay_made_in_action_sees_values`, `core/des_overlay_field_error_errors`.

## 0.51.11 — 2026-11-09

- sims/capstone realism tuning (owner, 2026-11-09): successor polities (a split's halves, a union, a polity that rose or fell a class) keep their parent's form of government, or draw one that carries it on (`government::keep`: a chieftainship becomes a hereditary monarchy, an elders' council a council of nobles or an elected crown, ...); an interregnum ends in a hereditary crown likelier the more houses claim it; an elective crown held by one house for generations can turn hereditary; a merchant oligarchy's rich families can make themselves lords (hereditary monarchy or tyranny). Rulers marry readily (25% a year) and a grown ruler comes to the throne married with an heir by chance of age; after an interregnum the restoring house is drawn (a new house weighs a half) instead of always the most prestigious claimant; 19 tellings of death instead of 8. Year 500: 8 hereditary monarchies (was 1), 4 elective, 8 merchant oligarchies, 7 theocracies; 127 successions to the heir (was about 30); 39 interregnums (was 65); 59.2 s. Details and constants in `sims/capstone/FINDINGS.md`.

## 0.51.10 — 2026-11-09

- Parser: the field modifiers `name?:` (nullable) and `name!?:` / `name?!:` (readonly and nullable) parse again, on both engines. The lexer stopped fusing `?` into the name (as it had `!`, which the parser already re-attached), so `email?: ""`, the form the P0904 help itself suggests, failed with P0904. A nullable field can be given nil when the object is made; one without `?` still can't (T0207). Conformance: `core/class_field_nullable_suffix`, `core/class_field_nil_without_suffix_errors`.
- sims/capstone: `Ruler.house` is `house?: nil` (no house is a plain nil field), given when the ruler is made. Output unchanged. FINDINGS no longer lists "a nil-default field refuses nil when made" as a runtime gap: the `?` modifier is the language's way, and it was the parser that was broken.

## 0.51.9 — 2026-11-09

- sims/capstone: a ruler leaves only by its erase transition (owner, 2026-11-09: "An object cannot be nil. It must be erased."). When a ruler died, was deposed or voted out, or an interregnum began, `family.gbln` also cleared the realm's `ruler` by hand; now the ruler is marked `dead` or `gone`, `transition erase` removes it, and the realm's `ruler` reads nil once it is erased. `reign` is not put on for a leaving ruler, and a leaving ruler can't wed. FINDINGS no longer lists "an object variable can't take nil" as a runtime gap. Year 500: 40 polities, the largest 45 provinces, 1,069 deaths, 65 interregnums; 62.8 s (0.51.6: 67.4 s, run noise included).

## 0.51.8 — 2026-11-09

- GLAMs have no dependencies, and the Box is the only way in or out (owner, 2026-11-08). A GLAM's code is every file under `glams/<name>/`: its entry file, its own helper files, and their actions, whenever they run. That code:
  - imports only files under `glams/<name>/` (a GLAM split across many files works as before); any other import is B0110;
  - never runs `use`; loading another GLAM is B0109 (the project `use`s both, and they share values through the Box);
  - can't read the project's variables or call its actions, or any other module's (B0111): a bare name that isn't the GLAM's own fails when the GLAM loads, a name found at run time fails when it's read, `{name}` in a string stays as written, and `:invoke`/`:summon` of an outside action fails. An outside action is wired in `[needs.actions]` and called with `:need`, as before;
  - writes only into its own Box namespace (B0102) and only the names its `[provides] values` lists (B0105), at any time. Before, both rules held only while `use` ran the GLAM's top level, so its actions could write anywhere later.
  - Reading any Box variable is still allowed, and so are builtins, file I/O and `:need`. Code outside `glams/` is unchanged.
- The VM is goblin-vm 0.2.8. The interpreter has none of this (conformance: `known-gap: interp`).

## 0.51.7 — 2026-11-08

- Import exclusions are removed (owner, 2026-11-08): `imports.exclude.yall`, `--import-excludes FILE` and `GOBLIN_IMPORT_EXCLUDES` no longer exist, and `import` and `use` skip nothing. A program picks its modules in code and imports only those (`repeat names as n` / `import "modules/{n}/{n}" as {n}` / `xx`, 0.51.1–0.51.2), so a second list of what not to import is not needed. An `imports.exclude.yall` left in a project is ignored, and `--import-excludes` is no longer a goblin flag (before the script it is read as the script's name; after it, it is passed to the script). Ordinary, grouped, computed and pattern imports, `as {...}`, manifests, `:load`, `:eval` and `:inject` are unchanged.
- The VM is goblin-vm 0.2.7.

## 0.51.6 — 2026-11-08

- sims/capstone step 4 reworked (owner, 2026-11-08: "as little machinery as possible", events for what the sim doesn't model). The people are gone: no Person or Chief objects, no parents, children, regents, great families or plots. Every realm has one `Ruler` (name, age, house, fame, aggression, ambition, competence, piety); a `Dynasty` has no members (prestige, realms ruled, claims, famous names) and fades once it rules nowhere and claims nothing; the ruling family is numbers on the realm (`heir` age, `kin`, `claimants`). A ruler dies by the sum of its hazards (illness by age, old age, famine, battle, accidents, murder) and `:event` tells how, drawn by those same hazards; a death told as a murder costs legitimacy and stability. Births, weddings at home, an heir's death, successions, depositions, houses founded and faded, regencies and lines ending are told with `:event`. Succession laws, claims, kinship, partition and the court `match` decision stay; the `home` decision, `plot`/`scheme` and the `crown` overlay are gone; `reign` bends five traits and a child ruler pushes nothing. The metrics CSV counts rulers and dynasties. Year 500: 40 polities, the largest 44 provinces; 1,015 deaths, 74 interregnums, 23 murders, 115 marriages; 3,452 lines of narration; 67.4 s (0.51.4: 79.3 s). `family.gbln` 1,193 lines to about 720. Choices to confirm in `sims/capstone/FINDINGS.md`.

## 0.51.5 — 2026-11-08

- DES events (owner, 2026-11-08): `:event(who, what, whom, telling)` writes a line of narration to the event log, for what a sim tells rather than models (how a ruler died, an omen). It changes nothing in the program. The line is `tick 812 | Kingdom Arveth | ruler died | Person Toren | "of a fever"`: who is an object (named as the log names objects) or a text; what a text; whom an object, a text, a list of them or nil (optional); the telling (optional) a text, or a map of tellings to weights, `{"fell in battle": :min(p >> wars, 1), "of old age": past(r >> age / 80.0, .6)}`, from which one is drawn by weight, so a telling with weight 0 can't come up (all 0: no line, and nil back). It gives back the telling written. The draw has its own random stream, seeded with the program's (`:rand_seed`, an actor's, a swarm task's), so a program draws the same with or without events; with the log off nothing is written but the draw still happens. In a region the lines come back in region order like the rest of the log. A bad argument is R1253. The interpreter has none of this (conformance: `known-gap: interp`).
- The VM is goblin-vm 0.2.6.

## 0.51.4 — 2026-11-08

- sims/capstone step 4: rulers and families (`family.gbln`). A tribe or village community is led by a `Chief` (a name and three traits); a city-state and up by a `Person` (age, health, seven traits, parents, spouse, fame), most of them members of a `Dynasty`, which owns its living members and keeps a prestige, the thrones it claims through marriage and its famous rulers' names; each realm also has two great families. Each year in the inside phase people age, may die, have children (traits drawn around their parents'), come of age with their upbringing, marry (a new `court` interface: `match` with another realm's ruling family, `home` with a great family's noble, or wait), and an ambitious heir may plot (`scheme`). The plan's seven succession laws are set by the form of government; the line and the `succession` trait are worked out each year (no longer a drifting trait). A ruler's death brings the next by the law, an interregnum when the succession is unclear enough (step 3's yearly chance of one is gone), a claim from marriage when the line is empty, a regent for a child, and under the divided law a split for a second son. A change of form seats a usurper, a general, a priest, an elected term leader or a new dynasty, and the deposed house claims the throne. Two overlays (`reign`, `crown`, class `<^>Rule`) bend the realm by its ruler's or regent's traits. Fame (wars, buildings, famine) raises a house's prestige; famous names are reused and numbered. Kinship (a new link) joins the union decision, a claim the war decision. `check.sh` checks marriages and plots; the metrics CSV counts persons, chiefs and dynasties. Year 500: 47 polities, the largest 35 provinces; 137 interregnums, 16 murders, 496 marriages; 79.3 s (step 3: 69.7 s). Choices to confirm and runtime gaps are in `sims/capstone/FINDINGS.md`.

## 0.51.3 — 2026-11-08

- DES: a decision's `where target == x` (`target == @capital`), as the first condition that names `target`, checks only `x` instead of running the condition on every object of the class. The same targets pass, in the same order. sims/capstone (step 3, 500 years): 73.2 s to 69.7 s, the same output and event log; its reform, settle, migrate and disband decisions each looked at all 400 provinces for one.
- The VM is goblin-vm 0.2.5.

## 0.51.2 — 2026-11-08

- `import path as {x}`: the alias is the string `x` gives when the import runs (owner, 2026-11-08). Any expression goes in the braces (`as {n}`, `as {"m_" + n}`), and the path may be literal or computed. So a loop imports a list of modules under their own names: `repeat names as n` / `import "modules/{n}/{n}" as {n}` / `xx`, then `alpha::step()` or `:summon(ctx, ["alpha::step"])`. The name must be letters, digits and `_`, not starting with a digit; anything else, or a value that isn't a string, is an error, and so is `as {...}` on a `.imports` manifest. Otherwise it is `import path as name`, including the rule that one file can't give an alias to two different modules (the second is skipped with a warning). VM only: the interpreter reads `as {...}` as no alias (conformance: `known-gap: interp`).
- The VM is goblin-vm 0.2.4.

## 0.51.1 — 2026-11-08

- The VM runs `import` with a computed path, as the interpreter always has: a quoted path with `{...}` in it (`import "modules/{n}/{n}" as m`) is built when the statement runs, then imported like the same literal path (it runs once, a missing extension becomes `.gbln`, and exclusions apply). So a program can import modules from a list it builds, with no manifest per module and no `:eval`. As on the interpreter, a computed path to a Goblin file needs `as`, and one to a `.imports` manifest takes none. Before, the VM stopped with "not yet implemented: dynamic import paths". - The VM is goblin-vm 0.2.3.

## 0.51.0 — 2026-11-08

- `import` takes a pattern: `import "modules/*/manifest.imports"` imports every file that matches, sorted by path. `*` matches inside one part of the path, and a `**` part matches any number of directories. A pattern that matches nothing imports nothing. A pattern can't take `as` (one alias names one module). The pattern resolves the way any `import` path does: against the project root, or the GLAM's directory inside a GLAM.
- A file imported twice runs once even when the two paths are spelled differently (`a/../a/x.gbln`, a symlink). Before, the check compared the path as written. An `import ... as m` of a file that already ran under another spelling still gives `m::` its actions.
- `:load(sources)` takes a path or a list of paths and returns a bundle, which is a list with one map per file, in the order given. A Goblin file (`.gbln`, `.gob`, `.imports`) becomes `{path, kind: "code", text}`, and a data file (`.yall`, `.json`) becomes `{path, kind: "data", data}`, parsed when it loads. Any other kind of file is an error. Nothing runs.
- `:eval(x)` evaluates a bundle, one source from a bundle, a path, or a list of paths. Code sources run in order, every time (unlike `import`, which runs a file once). With data in the bundle, eval returns the data: one data source as it is, and several merged by key (a key in two of them is an error). With no data, eval returns what the last code source returns (`return` at its top level; nil without). The bundle is not changed.
- `:inject(action, "before" | "after" | "override", target)`, in code run by `:eval`, changes the eval's data: every list of action names that holds `target` gets `action` before it, after it, or in its place. All of an eval's injections are applied together, after its code has run and before eval returns, and nothing in the lists runs. A target listed twice in one list: `before` goes before the first and `after` after the last (owner, 2026-11-08), and `override` replaces each one. A target in two lists is an error that asks for the 4th argument, the list's name (`:inject(a, "after", t, "pre")`). Several injections at one target keep the order of the sources, then their order inside each source. Targets are the names as loaded, so one injection can't target another's action. Errors, each naming the source: two overrides of one target, a target no list holds, a 4th argument naming a list that doesn't hold the target, an action Goblin can't find, an unknown operation, and injections with no data to change. Outside `:eval`, `:inject` is an error for now.
- An import cycle is still skipped (the file already being imported does not run again), but now says so on stderr, naming the files: `warning: import cycle: a.gbln -> b.gbln -> a.gbln; ...`. A file imported twice without a cycle says nothing.
- Import exclusions: `imports.exclude.yall` in the project root (the directory `goblin` runs in) lists files and directories that `import` and `use` skip, so their top level never runs: `exclude:` followed by a list of paths (`- "modules/my_cool_module/**"`). An entry is relative to the project root (or absolute) and takes `*` and `**` as import patterns do; it excludes what it names and everything under it. Files are compared by real path, so another spelling or a symlink can't get round it. It applies to direct, pattern and nested imports and to `use` (an excluded GLAM's glam.toml isn't read either). `--import-excludes FILE` (anywhere on the `goblin run`, `start` or `repl` line) or `GOBLIN_IMPORT_EXCLUDES=FILE` uses that file instead; it must exist. No file, or an empty list, imports everything as before. A bad file stops the run before any code runs, with a message naming it. `:load`, `:eval` and `:summon` ignore exclusions. Each process reads its exclusions once, so runs with different files don't affect each other; every request of one `goblin start` server uses the server's.
- `m::f` by name (`:summon`, `:invoke`, `:need`) no longer runs another module's `f` when `m` has none: it is an unknown action. Before, a name not found under its namespace fell back to any action named `f`, so `:summon(ctx, ["module_a::render"])` could run module B's `render`. `:need` calls its provider by the full `namespace::action` name for the same reason (before: the bare name).
- The VM is goblin-vm 0.2.2. The interpreter has none of this (conformance: `known-gap: interp`).

## 0.50.0 — 2026-11-07

- `:field(obj, "x")` reads what a field holds, without the overlays `obj >> x` reads it through (an overlay's `modify` is a calculation on top of the field); an unknown field is R0403. sims/capstone uses it: the forms of government bend only traits (before: traits and baselines, because the yearly pull read the bent trait), the pull works on the traits' own values, and a colony or league copies its founder's own values (before: its founder's form came along as culture, and bent the child twice when it took the same form). The history changes (year 500: 40 polities, the largest 38 provinces).
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
- Tokens: `GOBLIN::EMPTY` is a built-in token that stands for nothing again, on the VM (in `{{{GOBLIN::EMPTY}}}` inside a string, `:resolve_token(text)`, `:resolve_token("GOBLIN", "EMPTY")` and a literal token), so a Sheriff template slot set to it renders empty. It was added on 2026-07-06 (62234cc) on a branch that never reached this one.
- Brought back changes from the July `claude/rc-value-perf` branch that never reached jareth: `:total_time()` and `:split_time()` (time since start / since the last split, as `"1234ms (1.234s)"`); `=> a => b` chains after an inline `if`, an inline `act`, and the `?? =>` guard; the `name | expr !! => stmt` error guard (runs stmt when expr fails; name stays bound); `x !|`, `a[i] !|` and `m{"k"} !|` delete in place, and `m{"k"} |! v` (before: P04U1); `provoke(cond, msg)` as a statement (before: P0715), and a failed provoke stops the program with A0502 instead of being caught by `attempt`; Y'all writer and parser changes so the yall-spec suite passes (multiline strings as `"""`, `true`/`false`/`nil` strings quoted, small collections inline, `/` in bare values, tab indentation and error wording per the spec); doc edits to `crates/needs.md`, `crates/count.md`, the documentation checklist and `api/gmark.gbln`. `:assert` stays removed (owner, 2026-10-06).
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
