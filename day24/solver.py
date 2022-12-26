import sys

sys.path.insert(0, "../utils/py")
import utils
from dataclasses import dataclass
from copy import copy
from rich import print


measure_time = utils.stopwatch()


@dataclass
class Blizzard:
    x: int
    y: int
    dx: int
    dy: int
    symbol: str
    
    def __hash__(self) -> int:
        return id(self)

@dataclass
class Point:
    x: int
    y: int
    t: int
    
    def __hash__(self) -> int:
        return hash(" ".join(map(str, [self.x, self.y, self.t])))

Position = tuple[int]


@measure_time
def parse(raw_data) -> tuple[dict[Position, str], list[Blizzard]]:
    walls: dict[Position, str] = {}
    blizzards: list[Blizzard] = []
    for y_, line in enumerate(raw_data.splitlines()):
        for x_, c in enumerate(line):
            # shift x and y by -1 so that the actual grid starts at 0,0
            x = x_ - 1
            y = y_ - 1
            if c == "#":
                walls[x, y] = c
            elif c in "<>v^":
                if c == ">":
                    blizzard = Blizzard(x, y, 1, 0, ">")
                elif c == "<":
                    blizzard = Blizzard(x, y, -1, 0, "<")
                elif c == "v":
                    blizzard = Blizzard(x, y, 0, 1, "v")
                elif c == "^":
                    blizzard = Blizzard(x, y, 0, -1, "^")
                blizzards.append(blizzard)
    return walls, blizzards


def blizzards_after(blizzards: list[Blizzard], t: int, maxx: int, maxy: int) -> dict[Blizzard, Position]:
    positions = {}
    for b in blizzards:
        x = (b.x + t * b.dx) % maxx
        y = (b.y + t * b.dy) % maxy
        positions[b] = (x, y)
    return positions


def neighbors(gridstate: tuple[dict, list[Position]], point: Point) -> list[Point]:
    """give a list of possible positions to move to (including staying) from pos at time t"""
    walls, blizzards = gridstate
    x, y, t = point.x, point.y, point.t
    movements = [(0,0), (1,0), (-1,0), (0,1), (0,-1)]
    minx, maxx, miny, maxy = utils.corners(walls)
    neighbors = []
    for dx, dy in movements:
        nx, ny = x+dx, y+dy
        conditions = [miny-1 < ny, ny < maxy+1, (nx, ny) not in walls, (nx, ny) not in blizzards]
        # print(f"see if {nx,ny} is a valid neighbor")
        # print(conditions)
        if all(conditions):
            neighbors.append(Point(nx, ny, t+1))
    # print(f"neighbors for {point}: {neighbors}")
    return neighbors


def cached_neighbors(grid: tuple[dict, list[Blizzard]]):
    # the blizzard positions at time t
    walls, blizzards = grid
    minx, maxx, miny, maxy = utils.corners(walls)
    blizzard_positions: dict[int, list[Position]] = {}

    def _neighbors(grid, point):
        walls, _ = grid
        if point.t+1 not in blizzard_positions:
            # print(f"filling cache for time {point.t}")
            blizzard_positions[point.t+1] = list(
                blizzards_after(blizzards, point.t+1, maxx, maxy).values()
            )

        return neighbors((walls, blizzard_positions[point.t+1]), point)
    
    return _neighbors


def visualize(grid: tuple[dict, list[Blizzard]], point: Point):
    # there's a lot of redundant computation in this function, so use it only for debugging
    walls, blizzards = grid
    minx, maxx, miny, maxy = utils.corners(walls)
    field = copy(walls)
    field[point.x, point.y] = "E"
    for b,pos in blizzards_after(blizzards, point.t, maxx, maxy).items():
        field[pos] = b.symbol
    print(point)
    print(utils.dictgrid_to_str(field, empty="."))


# PART 1
@measure_time
def solve1(data):
    walls, blizzards = data
    minx, maxx, miny, maxy = utils.corners(walls)
    start = Point(minx+1, miny, 0)
    finished = lambda p: p.x == maxx-1 and p.y == maxy
    paths = utils.BFS(
        data,
        start,
        cached_neighbors(data),
        finished=finished,
        # visualize=visualize
    )
    for p in paths:
        if finished(p):
            break

    # for p in utils.shortestpath(paths, start, p):
    #     visualize(data, p)
    # print(paths)

    return p.t


# PART 2
@measure_time
def solve2(data):
    walls, blizzards = data
    minx, maxx, miny, maxy = utils.corners(walls)
    neighbors_func = cached_neighbors(data)

    # round 1
    start = Point(minx+1, miny, 0)
    finished = lambda p: p.x == maxx-1 and p.y == maxy
    paths = utils.BFS(data, start, neighbors_func, finished=finished)
    for p in paths:
        if finished(p):
            break
    # print(f"round 1 done with goal {p}")

    # round 2
    start = Point(p.x, p.y, p.t)
    finished = lambda p: p.x == minx+1 and p.y == miny
    paths = utils.BFS(data, start, neighbors_func, finished=finished)
    for p in paths:
        if finished(p):
            break
    # print(f"round 1 done with goal {p}")
    
    # round 3
    start = Point(p.x, p.y, p.t)
    finished = lambda p: p.x == maxx-1 and p.y == maxy
    paths = utils.BFS(data, start, neighbors_func, finished=finished)
    for p in paths:
        if finished(p):
            break
    # print(f"round 1 done with goal {p}")
    
    return p.t



if __name__ == "__main__":
    data = parse(open("input.txt").read().strip())
    print("Part 1: {}".format(solve1(data)))
    print("Part 2: {}".format(solve2(data)))

    print("\nTime taken:")
    for func, time in measure_time.times:
        print(f"{func:8}{time}s")
    print("----------------")
    print("total   {}s".format(sum(t for _, t in measure_time.times)))

