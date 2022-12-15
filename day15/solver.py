import sys

sys.path.insert(0, "../utils/py")
import utils
import re
from collections import Counter


def manhattan(p1, p2):
    x1,y1 = p1
    x2,y2 = p2
    return abs(x2-x1) + abs(y2-y1)

def ints(s: str) -> list[int]:
    return list(map(int, re.findall(r"[-+]?[0-9]+", s)))

measure_time = utils.stopwatch()


@measure_time
def parse(raw_data):
    closest_beacon = {}
    for line in raw_data.splitlines():
        ss, bb = line.split(":")
        sensor = tuple(ints(ss))
        beacon = tuple(ints(bb))
        closest_beacon[sensor] = beacon
    return closest_beacon

def intersection(d, y):
    """return points where the line at y=y intersects the diamond"""
    lines = [(d[0], d[1]), (d[1], d[2]), (d[2], d[3]), (d[3], d[0])]
    isecs = []
    for start, end in lines:
        if not min(start[1], end[1]) <= y <= max(start[1], end[1]):
            continue
        slope = (start[1] - end[1]) // (start[0] - end[0])
        b = start[1] - slope * start[0]
        ix = (y - b) // slope
        iy = slope * ix + b
        isecs.append((ix, iy))
    return isecs

def make_diamond(sensor, beacon):
    """return the (top, right, bottom, down) point of the diamond around the sensor"""
    xs, ys = sensor
    d = manhattan(sensor, beacon)
    return ((xs, ys-d), (xs+d, ys), (xs, ys+d), (xs-d, ys))

def intersections(d1, d2):
    """return list of points where d1 intersects d2 (d1 and d2 being diamond-shapes)"""
    lines1 = [(d1[0], d1[1]), (d1[1], d1[2]), (d1[2], d1[3]), (d1[3], d1[0])]
    lines2 = [(d2[0], d2[1]), (d2[1], d2[2]), (d2[2], d2[3]), (d2[3], d2[0])]
    # print(f"checking out if diamond1: {d1=}")
    # print(f"intersects with diamond2: {d2=}")
    isecs = []
    for start1, end1 in lines1:
        for start2, end2 in lines2:
            # print(f"    check intersection of {(start1,end1)} and {(start2,end2)}")
            # see if there's overlap in x
            xoverlaps = not max(start1[0], end1[0]) < min(start2[0], end2[0]) and not min(start1[0], end1[0]) > max(start2[0], end2[0])
            # overlap in y
            yoverlaps = not max(start1[1], end1[1]) < min(start2[1], end2[1]) and not min(start1[1], end1[1]) > max(start2[1], end2[1])
            # only if there's overlap in x and y there can be an intersection
            if xoverlaps and yoverlaps:
                # print(f"      they overlap in both x and y")
                slope1 = (start1[1] - end1[1]) // (start1[0] - end1[0])
                slope2 = (start2[1] - end2[1]) // (start2[0] - end2[0])
                # print(f"      got {slope1=} and {slope2=}")
                if slope1 == slope2:
                    # print("      same slope, skip")
                    continue
                b1 = start1[1] - slope1 * start1[0]
                b2 = start2[1] - slope2 * start2[0]
                x = (b2 - b1) // (slope1 - slope2)
                y = slope1 * x + b1
                # print(f"      got intersection {(x,y)}")
                isecs.append((x,y))
    return isecs


def in_diamond(p, diamond):
    top, right, bottom, left = diamond
    middlex = top[0]
    middley = right[1]
    d = (right[0] - left[0]) // 2
    return manhattan(p, (middlex, middley)) <= d


def neighbors(p):
    x, y = p
    return [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]


def find_undetected(isecs, shapes, searchspace):
    for sec in isecs:
        # print(f"checking intersection {sec}")
        for p in neighbors(sec):
            if searchspace[0] <= p[0] <= searchspace[1] and searchspace[0] <= p[1] <= searchspace[1]:
                # print(f"    neighbor {p} is in searchspace! check if in shapes")
                detected = False
                for d in shapes:
                    if in_diamond(p, d):
                        # print(f"    it's in a shape")
                        detected = True
                        break
                if not detected:
                    return p

# PART 1
@measure_time
def solve1(data, yaxis=2000000):
    shapes = [make_diamond(sensor, beacon) for sensor, beacon in data.items()]
    isecs = []
    for s in shapes:
        isecs.extend(intersection(s, yaxis))
    
    xs = [x for x, _ in isecs]
    return max(xs) - min(xs)


# PART 2
@measure_time
def solve2(data, searchspace=(0, 4000000)):
    visualize = False or True
    # if visualize:
    #     data = {k: v for k, v in data.items() if k in [(8,7), (14,3)]}
    shapes = [make_diamond(sensor, beacon) for sensor, beacon in data.items()]
    isecs = []
    for s1 in shapes:
        for s2 in shapes:
            if s1 == s2:
                continue
            isecs.extend(intersections(s1, s2))

    # only for visualization of the sample input
    if visualize:
        from string import ascii_letters    
        grid = {}
        for p in isecs:
            grid[p] = "x"
        for i,s in enumerate(shapes):
            for p in s:
                grid[p] = ascii_letters[i]
        print(utils.dictgrid_to_str(grid, empty="."))
        return

    x, y = find_undetected(isecs, shapes, searchspace)
    return x * 4000000 + y
    


if __name__ == "__main__":
    data = parse(open("input.txt").read().strip())
    print("Part 1: {}".format(solve1(data)))
    print("Part 2: {}".format(solve2(data)))

    print("\nTime taken:")
    for func, time in measure_time.times:
        print(f"{func:8}{time}s")
    print("----------------")
    print("total   {}s".format(sum(t for _, t in measure_time.times)))

