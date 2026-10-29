import sys

sys.path.insert(0, "../utils/py")
import re
from collections import Counter
from enum import StrEnum
from queue import Queue
import math
from functools import wraps

import utils

measure_time = utils.stopwatch()


class ResourceType(StrEnum):
    ORE = "ORE"
    CLY = "CLY"
    OBS = "OBS"
    GEO = "GEO"


RobotType = ResourceType
Cost = Counter[ResourceType]
BluePrint = dict[RobotType, Cost]


def ints(s: str) -> list[int]:
    return list(map(int, re.findall(r"\d+", s)))


@measure_time
def parse(raw_data: str) -> dict[int, BluePrint]:
    blueprints = {}
    for lineno, line in enumerate(raw_data.splitlines()):
        line = line[line.find(":") + 2 :]  # strip away the 'blueprint' part
        sentences = line.split(".")
        # all blueprints describe ore robots first, then clay then obsidian, then geode.
        # all ore robots cost ore, all clay cost ore, all obsidian robots cost ore and clay,
        # all geode robots cost ore and obsidian
        (ore_ore,) = ints(sentences[0])
        (cly_ore,) = ints(sentences[1])
        obs_ore, obs_cly = ints(sentences[2])
        geo_ore, geo_obs = ints(sentences[3])
        blueprint = {
            RobotType.ORE: Counter({ResourceType.ORE: ore_ore}),
            RobotType.CLY: Counter({ResourceType.ORE: cly_ore}),
            RobotType.OBS: Counter(
                {ResourceType.ORE: obs_ore, ResourceType.CLY: obs_cly}
            ),
            RobotType.GEO: Counter(
                {ResourceType.ORE: geo_ore, ResourceType.OBS: geo_obs}
            ),
        }
        blueprints[lineno + 1] = blueprint
    return blueprints


def min_steps(
    blueprint: BluePrint, resources: Counter[ResourceType], t: ResourceType
) -> int:
    # minimum number of steps needed to generate 1 resource of type t, given blueprint and resources
    ...


State = tuple[int, Counter[RobotType], Counter[ResourceType]]


def max_geodes(blueprint: BluePrint, steps: int) -> int:
    queue: Queue[State] = Queue()
    queue.put((1, Counter({RobotType.ORE: 1}), Counter()))

    current_best = Counter()
    while not queue.empty():
        step, robots, resources = queue.get()
        # print(step)
        if step == steps:
            if resources[ResourceType.GEO] > current_best[ResourceType.GEO]:
                current_best = resources
        # elif steps - step < min_steps(blueprint, resources, ResourceType.GEO):
        #     # it's not possible anymore to build a geode in the remaining time
        #     continue
        else:
            queue.put((step + 1, robots, resources + robots))
            for rt, cost in blueprint.items():
                if all(resources[t] >= cost[t] for t in cost):
                    queue.put(
                        (step + 1, robots + Counter({rt: 1}), resources - cost + robots)
                    )

    return current_best[ResourceType.GEO]


def max_geodes2(
    blueprint: BluePrint,
    resources: Counter[ResourceType],
    robots: Counter[RobotType],
    remaining_steps: int,
) -> int:
    # print(remaining_steps)
    # print(resources)
    if remaining_steps == 0:
        return resources[ResourceType.GEO]
    # if there is no time left to buy a geode robot
    if remaining_steps < max(
        blueprint[ResourceType.GEO][t] - resources[t] - remaining_steps * robots[t]
        for t in blueprint[ResourceType.GEO]
    ):
        return resources[ResourceType.GEO] + remaining_steps * robots[ResourceType.GEO]
    results = [max_geodes2(blueprint, resources + robots, robots, remaining_steps - 1)]
    for rt, cost in blueprint.items():
        missing = Counter({t: resources[t] - cost[t] for t in cost})   # don't use resources - cost because that discards negative values
        if all(missing[t] >= 0 for t in cost):
            # print(missing)
            results.append(max_geodes2(blueprint, resources - cost + robots, robots + Counter({rt: 1}), remaining_steps - 1))
        # else:
        #     if any(t not in robots for t in missing):
        #         break
        #     # number of steps in which we can afford to buy a robot of type rt
        #     future = 0
        #     for t, c in missing.items():
        #         _f = math.floor(c / robots[t])
        #         future = min(future, _f)

        #     assert future < 0, (missing, robots)

        #     if remaining_steps + future < 0:
        #         continue

        #     # this takes longer because there is double counting
        #     results.append(max_geodes2(
        #         blueprint,
        #         resources - cost + Counter({r: -future * v for r, v in robots.items()}),
        #         robots + Counter({rt: 1}),
        #         remaining_steps + future
        #     ))

        # if all(resources[t] >= cost[t] for t in cost):
        #     results.append(max_geodes2(blueprint, resources - cost + robots, robots + Counter({rt: 1}), remaining_steps - 1))
    return max(results)


# PART 1
@measure_time
def solve1(data: dict[int, BluePrint]):
    quality = 0
    for ID, blueprint in data.items():
        geodes = max_geodes2(blueprint, Counter(), Counter({RobotType.ORE: 1}), 19)
        print(f"geodes for {ID}: {geodes}")
        quality += ID * geodes
    return quality


# PART 2
@measure_time
def solve2(data):
    pass


if __name__ == "__main__":
    data = parse(open("input.txt").read().strip())
    print("Part 1: {}".format(solve1(data)))
    print("Part 2: {}".format(solve2(data)))

    print("\nTime taken:")
    for func, time in measure_time.times:
        print(f"{func:8}{time}s")
    print("----------------")
    print("total   {}s".format(sum(t for _, t in measure_time.times)))
