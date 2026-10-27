"""An epidemic in Mesa: the same rules as sims/epidemic/plague.gbln.

A sickness spreads between people in the same town, lasts 8 days (1 health
a day), and either kills or leaves its host immune. People now and then
travel to the next town.
"""
import os
import mesa

N_TOWNS = int(os.environ.get("TOWNS", 4))
PEOPLE_PER_TOWN = int(os.environ.get("PEOPLE", 100))
DAYS = int(os.environ.get("DAYS", 120))
SICK_DAYS = 8
CATCH = 0.004


class Person(mesa.Agent):
    def __init__(self, model, town, health):
        super().__init__(model)
        self.town = town
        self.health = health
        self.immune = False
        self.sick_left = 0  # days of sickness left; 0 = not sick

    def travel(self):
        if N_TOWNS > 1 and self.random.randint(1, 30) == 1:
            self.town = (self.town + 1) % N_TOWNS


class Epidemic(mesa.Model):
    def __init__(self, seed=11):
        super().__init__(rng=seed)
        for t in range(N_TOWNS):
            for i in range(PEOPLE_PER_TOWN):
                p = Person(self, t, 6 + self.random.randint(0, 8))
                if t == 0 and i == 0:
                    p.sick_left = SICK_DAYS

    def step(self):
        people = self.agents
        # Spread: each healthy person in a sick person's town catches it.
        sick = people.select(lambda p: p.sick_left > 0)
        caught = set()
        for host in sick:
            for p in people:
                if (p is not host and p.town == host.town and not p.immune
                        and p.sick_left == 0 and p not in caught
                        and self.random.random() < CATCH):
                    caught.add(p)
        for p in caught:
            p.sick_left = SICK_DAYS
        # The sickness costs 1 health a day; at 0 the person dies.
        for p in people.select(lambda p: p.sick_left > 0):
            p.health -= 1
            p.sick_left -= 1
            if p.health <= 0:
                p.remove()
            elif p.sick_left == 0:
                p.immune = True
        self.agents.do("travel")

    def report(self, day):
        alive = len(self.agents)
        sick = len(self.agents.select(lambda p: p.sick_left > 0))
        immune = len(self.agents.select(lambda p: p.immune))
        print(f"day {day}: alive {alive}, sick {sick}, immune {immune}, "
              f"dead {N_TOWNS * PEOPLE_PER_TOWN - alive}")


if __name__ == "__main__":
    print(f"epidemic: {N_TOWNS} towns of {PEOPLE_PER_TOWN}, {DAYS} days")
    model = Epidemic()
    for day in range(1, DAYS + 1):
        model.step()
        if day % 5 == 0 or day == DAYS:
            model.report(day)
