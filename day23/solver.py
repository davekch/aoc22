import sys

sys.path.insert(0, "../utils/py")
import utils
from collections import Counter
from copy import copy


measure_time = utils.stopwatch()


@measure_time
def parse(raw_data: str) -> dict:
    elves = {}
    for y, line in enumerate(raw_data.splitlines()):
        for x, c in enumerate(line):
            # the elves' current and considered next position
            if c == "#":
                elves[x,y] = None
    return elves

DIRS = {
    "N": (0, -1),
    "S": (0, 1),
    "W": (-1, 0),
    "E": (1, 0),
    "NW": (-1, -1),
    "NE": (1, -1),
    "SE": (1, 1),
    "SW": (-1, 1)
}

# the directions to consider in order
DIRGROUPS = ["N", "S", "W", "E"]

def neighbors(p: tuple, group: str=None) -> list[tuple]:
    x, y = p
    ns = []
    for dir, (dx, dy) in DIRS.items():
        if group is None or group in dir:
            ns.append((x+dx, y+dy))
    return ns

def round(elves: dict, dirindex: int) -> dict:
    # first half of round: decide where to go
    for elf in elves:
        # if no neighbors at all are present, do nothing
        if not any(n in elves for n in neighbors(elf)):
            elves[elf] = elf
            continue
        # otherwise look for direction to go to
        for i in range(4):
            # direction to consider
            dir = DIRGROUPS[(dirindex+i)%4]
            if not any(n in elves for n in neighbors(elf, dir)):
                # go there on the next step
                x, y = elf
                dx, dy = DIRS[dir]
                elves[elf] = (x+dx, y+dy)
                break
        else:
            # if no break
            # stay at the same spot
            elves[elf] = elf
    
    newelves = {}
    # count how many elves consider a point
    counter = Counter(elves.values())
    # next round: go to the proposed point if possible
    for elf, newelf in elves.items():
        if counter[newelf] == 1:
            # save elf at new position
            # what happens if a previus elf could not move and still sits here?
            assert newelf not in newelves
            newelves[newelf] = None
        else:
            # save elf at old position
            # what happens if a previus elf could not move and still sits here?
            assert elf not in newelves
            newelves[elf] = None
    
    return newelves

def printelves(elves):
    print(utils.dictgrid_to_str({e: "#" for e in elves}, empty="."))

# PART 1
@measure_time
def solve1(data):
    elves = copy(data)
    # print("initial:")
    # printelves(elves)
    for i in range(10):
        elves = round(elves, i)
        # printelves(elves)
    minx, maxx, miny, maxy = utils.corners(elves)
    # print(minx, maxx, miny, maxy)
    return (maxx-minx+1) * (maxy-miny+1) - len(elves)

# PART 2
@measure_time
def solve2(data):
    elves = data
    i = 0
    while elves != (newelves:=round(copy(elves), i)):
        elves = newelves
        i += 1
    return i+1


if __name__ == "__main__":
    data = parse(open("input.txt").read().strip())
    print("Part 1: {}".format(solve1(data)))
    print("Part 2: {}".format(solve2(data)))

    print("\nTime taken:")
    for func, time in measure_time.times:
        print(f"{func:8}{time}s")
    print("----------------")
    print("total   {}s".format(sum(t for _, t in measure_time.times)))

