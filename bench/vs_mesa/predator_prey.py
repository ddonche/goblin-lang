"""Predator and prey in Mesa: the same rules as sims/predator_prey/meadows.gbln.

Rabbits eat grass, foxes eat rabbits, both starve without food. Animals
now and then wander to the next meadow.
"""
import os
import mesa

N_MEADOWS = int(os.environ.get("MEADOWS", 4))
RABBITS = int(os.environ.get("RABBITS", 100))
FOXES = int(os.environ.get("FOXES", 5))
DAYS = int(os.environ.get("DAYS", 360))


class Grass(mesa.Agent):
    def __init__(self, model, meadow):
        super().__init__(model)
        self.meadow = meadow
        self.amount = 400

    def daily(self):
        self.amount = min(400, self.amount + 80)


class Animal(mesa.Agent):
    def __init__(self, model, meadow, energy):
        super().__init__(model)
        self.meadow = meadow
        self.energy = energy
        self.moved = False

    def wander(self):
        if N_MEADOWS > 1 and self.random.randint(1, 20) == 1:
            self.meadow = (self.meadow + 1) % N_MEADOWS
            self.moved = True  # arrives tomorrow: does nothing more today


class Rabbit(Animal):
    def daily(self):
        self.energy -= 1
        if self.energy >= 10 and self.random.randint(1, 4) == 1:
            self.energy -= 5
            Rabbit(self.model, self.meadow, 4)
        self.wander()

    def decide(self):
        grass = self.model.grass[self.meadow]
        if grass.amount >= 1:
            grass.amount -= 1
            self.energy = min(14, self.energy + 2)


class Fox(Animal):
    def daily(self):
        self.energy -= 1
        if self.energy >= 25 and self.random.randint(1, 4) == 1:
            self.energy -= 12
            Fox(self.model, self.meadow, 10)
        self.wander()

    def decide(self):
        if self.energy >= 30:
            return
        here = self.model.agents_by_type[Rabbit].select(lambda r: r.meadow == self.meadow)
        if len(here) == 0:
            return
        prey = self.random.choice(list(here))
        # The more rabbits in the meadow, the likelier the hunt succeeds.
        if self.random.randint(1, len(here) + 400) <= len(here):
            prey.remove()
            self.energy = min(40, self.energy + 8)


class Meadows(mesa.Model):
    def __init__(self, seed=7):
        super().__init__(rng=seed)
        self.grass = [Grass(self, m) for m in range(N_MEADOWS)]
        for m in range(N_MEADOWS):
            for _ in range(RABBITS):
                Rabbit(self, m, 4 + self.random.randint(0, 6))
            for _ in range(FOXES):
                Fox(self, m, 8 + self.random.randint(0, 8))

    def step(self):
        # Each living thing's daily changes, in the order they were made;
        # an animal that runs out of energy dies at once.
        for a in list(self.agents):
            a.daily()
            if isinstance(a, Animal) and a.energy <= 0:
                a.remove()
        # Then each animal still here (not caught, not wandered off) eats or hunts.
        for a in list(self.agents_by_type[Rabbit]) + list(self.agents_by_type[Fox]):
            if a in self.agents and not a.moved:
                a.decide()
        for a in self.agents:
            if isinstance(a, Animal):
                a.moved = False

    def report(self, day):
        grass = sum(g.amount for g in self.grass)
        rabbits = len(self.agents_by_type[Rabbit])
        foxes = len(self.agents_by_type[Fox])
        print(f"day {day}: rabbits {rabbits}, foxes {foxes}, grass {grass}")


if __name__ == "__main__":
    print(f"predator and prey: {N_MEADOWS} meadows, {DAYS} days")
    model = Meadows()
    for day in range(1, DAYS + 1):
        model.step()
        if day % 10 == 0 or day == DAYS:
            model.report(day)
