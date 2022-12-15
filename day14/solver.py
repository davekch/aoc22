import sys

sys.path.insert(0, "../utils/py")
import utils
import copy


measure_time = utils.stopwatch()

class FallingForeverError(Exception):
    pass

ORIGIN = (500, 0)

@measure_time
def parse(raw_data):
    grid = {}
    for line in raw_data.splitlines():
        points = line.split(" -> ")
        for p1,p2 in zip(points, points[1:]):
            x1, y1 = p1.split(",")
            x2, y2 = p2.split(",")
            x1, y1, x2, y2 = int(x1), int(y1), int(x2), int(y2)
            for y in range(min(y1,y2), max(y1,y2)+1):
                for x in range(min(x1, x2), max(x1, x2)+1):
                    grid[(int(x), int(y))] = "#"
    grid[ORIGIN] = "+"
    return grid

def available(p, grid, bottom=None):
    if p in grid:
        return False
    elif bottom and p[1] == bottom:
        return False
    return True

def falling_sand(grid, bottom=None):
    x, y = ORIGIN
    minx, maxx, miny, maxy = utils.corners(grid)
    while True:
        dy = 1
        dx = 0
        if not available((x,y+dy), grid, bottom):
            dx = -1  # diagonally to the left
            if not available((x+dx,y+dy), grid, bottom):
                dx = 1
                if not available((x+dx,y+dy), grid, bottom):
                    grid[(x,y)] = "o"
                    return

        x, y = x+dx, y+dy
        # print(x,y)
        if bottom is None and(x<minx or x>maxx) and (y<miny or y>maxy):
            raise FallingForeverError()

# PART 1
@measure_time
def solve1(data):
    c = 0
    grid = copy.copy(data)
    try:
        while True:
            falling_sand(grid)
            c += 1
    except FallingForeverError:
        return c

# PART 2
@measure_time
def solve2(data):
    minx, maxx, miny, maxy = utils.corners(data)
    bottom = maxy + 2
    c = 0
    while data[ORIGIN] != "o":
        falling_sand(data, bottom)
        # print()
        # gridprint(data)
        # print()
        c += 1
    return c


if __name__ == "__main__":
    data = parse(open("input.txt").read().strip())
    print("Part 1: {}".format(solve1(data)))
    print("Part 2: {}".format(solve2(data)))

    print("\nTime taken:")
    for func, time in measure_time.times:
        print(f"{func:8}{time}s")
    print("----------------")
    print("total   {}s".format(sum(t for _, t in measure_time.times)))

