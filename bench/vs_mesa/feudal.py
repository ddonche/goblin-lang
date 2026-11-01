"""A feudal world in Mesa: the same rules as sims/feudal/kingdom.gbln.

Three kingdoms. Everything is held by someone (`owner`): realms hold kings,
nobles hold estates, vassals and children. Mesa has no notion of one agent
holding others, so this model keeps each holder's list of what it holds
itself (`Held`), and moves what a dead noble held up to his liege by hand.
"""
import os
import mesa

N_KINGDOMS = 3
YEARS = int(os.environ.get("YEARS", 200))
HOUSES = ["Arden", "Bracken", "Corvel", "Dunmore", "Elsing", "Fenwick", "Garrow", "Holt",
          "Ivers", "Jessop", "Kell", "Lorne", "Marsh", "Norwood", "Orme"]
REALM_NAMES = ["Aldmark", "Brennor", "Caddis"]


class Held(mesa.Agent):
    """An agent that can be held by another, and hold others."""

    def __init__(self, model, owner):
        super().__init__(model)
        self.holds = []
        self._owner = None
        self.owner = owner

    @property
    def owner(self):
        return self._owner

    @owner.setter
    def owner(self, new):
        if self._owner is not None:
            self._owner.holds.remove(self)
        self._owner = new
        if new is not None:
            new.holds.append(self)

    def held(self, cls):
        return [x for x in self.holds if isinstance(x, cls)]


class Realm(Held):
    def __init__(self, model, n):
        super().__init__(model, None)
        self.n = n
        self.name = REALM_NAMES[n]
        self.counts = dict.fromkeys(
            ["crowned", "inherited", "escheated", "granted", "sold", "seized", "defected", "arrived"], 0)

    def yearly(self):
        crown(self)


class Estate(Held):
    def __init__(self, model, owner, name):
        super().__init__(model, owner)
        self.name = name
        self.yield_ = 3 + self.random.randint(0, 6)

    def yearly(self):
        if isinstance(self.owner, Noble):
            self.owner.gold += self.yield_


class Noble(Held):
    def __init__(self, model, owner, house, age, lifespan, gold=0, loyalty=50, parent=None):
        super().__init__(model, owner)
        self.house = house
        self.name = house
        self.age = age
        self.lifespan = lifespan
        self.gold = gold
        self.loyalty = loyalty
        self.vows = 0
        self.parent = parent

    def gone(self):
        return self.age >= self.lifespan or self.vows == 1

    def yearly(self):
        self.age += 1
        live(self)

    def deeds(self):
        # At most one deed a year: the first that is done.
        for deed in (self.defect, self.sell, self.seize, self.grant):
            if deed():
                return

    def best(self, candidates, score):
        if not candidates:
            return None
        top = max(score(c) for c in candidates)
        return self.random.choice([c for c in candidates if score(c) == top])

    def defect(self):
        liege = self.owner
        if not (self.age >= 16 and self.loyalty < 20 and isinstance(liege, Noble)
                and strength(self) * 2 > strength(liege)):
            return False
        home = kingdom(self)
        realms = [r for r in self.model.agents_by_type[Realm] if r.n != home]
        r = self.best(realms, lambda r: -r.counts["seized"])
        if r is None:
            return False
        kings = r.held(Noble)
        if not kings:
            return False
        realm_of(self).counts["defected"] += 1
        r.counts["arrived"] += 1
        self.owner = kings[0]
        self.loyalty = 60
        return True

    def sell(self):
        if not (self.gold < 0 and estates(self) >= 2):
            return False
        buyers = [b for b in self.owner.held(Noble) if b is not self and b.gold >= 60] if self.owner else []
        buyer = self.best(buyers, lambda b: b.gold)
        if buyer is None:
            return False
        cheapest = min(self.held(Estate), key=lambda e: e.yield_)
        price = cheapest.yield_ * 12
        if buyer.gold < price:
            return False
        buyer.gold -= price
        self.gold += price
        cheapest.owner = buyer
        realm_of(self).counts["sold"] += 1
        return True

    def seize(self):
        if self.age < 16 or self.owner is None:
            return False
        mine = strength(self)
        lands = [e for v in self.owner.holds if v is not self and isinstance(v, Noble)
                 for e in v.held(Estate) if mine >= 2 * strength(v) + 2]
        e = self.best(lands, lambda e: e.yield_)
        if e is None:
            return False
        if self.random.randint(1, 10) > 1:
            return False
        e.owner.loyalty -= 20
        e.owner = self
        realm_of(self).counts["seized"] += 1
        return True

    def grant(self):
        if estates(self) < 3:
            return False
        takers = [t for t in self.held(Noble) if t.age >= 16 and estates(t) == 0]
        to = self.best(takers, lambda t: t.loyalty)
        if to is None:
            return False
        min(self.held(Estate), key=lambda e: e.yield_).owner = to
        to.loyalty += 20
        realm_of(self).counts["granted"] += 1
        return True

    def die(self):
        inherit(self)
        # What is left goes up to his liege.
        for x in list(self.holds):
            x.owner = self.owner
        self.owner = None
        self.remove()


def realm_of(x):
    while x is not None and not isinstance(x, Realm):
        x = x.owner
    return x


def kingdom(x):
    r = realm_of(x)
    return -1 if r is None else r.n


def estates(n):
    return len(n.held(Estate))


def strength(n):
    return estates(n) * 2 + sum(estates(v) for v in n.held(Noble))


def live(n):
    n.gold -= 3 + 3 * len(n.held(Noble))
    liege = n.owner
    if isinstance(liege, Noble) and n.gold > 0:
        t = n.gold // 10
        n.gold -= t
        liege.gold += t
    if isinstance(liege, Noble):
        n.loyalty -= 1
        if strength(liege) < strength(n):
            n.loyalty -= 2
    land = estates(n)
    rnd = n.random
    if land == 0 and n.age >= 25 and rnd.randint(1, 10) == 1:
        n.vows = 1
    if 20 <= n.age <= 45 and land >= 1 and rnd.randint(1, 10) == 1:
        Noble(n.model, n, n.house, 0, 40 + rnd.randint(0, 35), loyalty=60, parent=n)


def inherit(d):
    r = realm_of(d)
    kids = [c for c in d.held(Noble) if c.parent is d]
    if not kids:
        if r is not None and estates(d) > 0:
            r.counts["escheated"] += 1
        return
    heir = max(kids, key=lambda c: c.age)
    heir.owner = d.owner
    heir.gold += d.gold
    for x in list(d.holds):
        x.owner = heir
    if r is not None:
        r.counts["inherited"] += 1


def crown(r):
    claimants = r.held(Noble)
    if not claimants:
        return
    if len(claimants) == 1 and not r.held(Estate):
        return
    king = max(claimants, key=strength)
    for c in claimants:
        if c is not king:
            c.owner = king
    for e in r.held(Estate):
        e.owner = king
    r.counts["crowned"] += 1


class Feudal(mesa.Model):
    def __init__(self, seed=11):
        super().__init__(rng=seed)
        for k in range(N_KINGDOMS):
            r = Realm(self, k)
            h = k * 5
            king = self.noble(HOUSES[h], r, 30)
            for i in range(4):
                Estate(self, king, f"estate {i}")
            for l in range(1, 5):
                lord = self.noble(HOUSES[h + l], king, 25 + l * 3)
                for i in range(3):
                    Estate(self, lord, f"estate {i}")
                for i in range(2):
                    kn = self.noble(HOUSES[h + l], lord, 20 + i * 5)
                    Estate(self, kn, "estate 9")

    def noble(self, house, liege, age):
        return Noble(self, liege, house, age, 50 + self.random.randint(0, 25), gold=20)

    def step(self):
        for a in list(self.agents):
            if a.model is None or a not in self.agents:
                continue
            a.yearly()
            if isinstance(a, Noble) and a.gone():
                a.die()
        for n in list(self.agents_by_type[Noble]):
            if n in self.agents:
                n.deeds()
        for n in list(self.agents_by_type[Noble]):
            if n.gone():
                n.die()

    def report(self, year):
        print(f"year {year}:")
        for r in self.agents_by_type[Realm]:
            kings = r.held(Noble)
            king = f"{kings[-1].house} ({kings[-1].age})" if kings else "none"
            nobles = sum(1 for nb in self.agents_by_type[Noble] if kingdom(nb) == r.n)
            lands = sum(1 for e in self.agents_by_type[Estate] if kingdom(e) == r.n)
            c = r.counts
            print(f"  {r.name}: king {king}, {nobles} nobles, {lands} estates; "
                  f"crowned {c['crowned']}, inherited {c['inherited']}, escheated {c['escheated']}, "
                  f"granted {c['granted']}, sold {c['sold']}, seized {c['seized']}, "
                  f"defected {c['defected']}, arrived {c['arrived']}")
        estates_all = self.agents_by_type[Estate]
        lost = sum(1 for e in estates_all if realm_of(e) is None)
        print(f"  estates {len(estates_all)}, held by no realm {lost}")


if __name__ == "__main__":
    print(f"feudal kingdoms: {N_KINGDOMS} realms, {YEARS} years")
    model = Feudal()
    for year in range(1, YEARS + 1):
        model.step()
        if year % 25 == 0 or year == YEARS:
            model.report(year)
