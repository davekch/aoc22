import sys

sys.path.insert(0, "../utils/py")
import utils
from collections import deque
from dataclasses import dataclass, field
from typing import Self
from enum import Enum, auto


measure_time = utils.stopwatch()

Parsed = dict[str, tuple[int, list[str]]]


@dataclass
class DecisionTree:
    at_valve: str
    timer: int
    release: int = 0
    children: list[Self] = field(default_factory=list)
    open_valves: list[str] = field(default_factory=list)
    parent: Self = None

    def __repr__(self) -> str:
        return f"DecisionTree(at_valve={self.at_valve}, timer={self.timer}, release={self.release}, children={[c.at_valve for c in self.children]}, open_valves={self.open_valves})"

    def __getitem__(self, child: str) -> Self:
        # for debugging only; can do tree['aa']['cc']['bb'] to follow a path
        for c in self.children:
            if c.at_valve == child.upper():
                found = c
                break
        else:
            raise KeyError(f"{child} is no child of this tree")
        return found


@measure_time
def parse(raw_data: str) -> Parsed:
    valves = {}
    for line in raw_data.splitlines():
        words = line.split()
        valve = words[1]
        rate = int(words[4].split("=")[1][:-1])
        connections = words[9:]
        connections = [c[:2] for c in connections]  # remove commas
        valves[valve] = (rate, connections)
    return valves


def build_decisiontree(valves: Parsed, timer: int, start: str) -> tuple[DecisionTree, int]:
    # valves we need to open; discard valves with release 0
    release_valves = [v for v in valves if valves[v][0] > 0]
    shortest_paths = all_shortest_paths(valves)
    # root
    decisiontree = DecisionTree(at_valve=start, timer=timer)
    q: deque[DecisionTree] = deque([decisiontree])  # keep a deque of things to try out
    # print(f"initial queue: {q}")
    current_max = 0
    count = 1
    while q:
        count += 1
        # if count % 1000 == 0:
        #     print(count)

        decisiontree = q.pop()
        current = decisiontree.at_valve
        # print(f"in the queue: {len(q)}")
        # print(f"popping {decisiontree} from queue")

        if decisiontree.release > current_max:
            current_max = decisiontree.release

        still_closed = [v for v in release_valves if v not in decisiontree.open_valves]
        # heuristic to kick out paths that cannot make it anyway
        overestimation = decisiontree.timer * sum(valves[v][0] for v in still_closed)
        if decisiontree.release + overestimation < current_max:
            continue

        # go to next closed valves
        for valve in still_closed:
            distance = len(shortest_paths[current][valve])
            if decisiontree.timer - distance >= 0:
                # no extra -1 for opening the valve because distance is 1 too large
                release = valves[valve][0] * (decisiontree.timer - distance)
                d = DecisionTree(
                    at_valve=valve,
                    timer=decisiontree.timer - distance,
                    release=decisiontree.release + release,
                    open_valves=decisiontree.open_valves + [valve],
                    parent=decisiontree,
                )
                # print(f"  up next: {d}")
                decisiontree.children.append(d)
                q.append(d)

    # print(f"done, now find the root (current tree is {decisiontree}")
    # print(f"iterations: {count}")
    # find root of decisiontree
    while decisiontree.parent is not None:
        decisiontree = decisiontree.parent
    return decisiontree, current_max


def build_decisiontree_p2(valves: Parsed, timer: int, start: str) -> int:
    # valves we need to open; discard valves with release 0
    release_valves = [v for v in valves if valves[v][0] > 0]
    shortest_paths = all_shortest_paths(valves)
    # root
    decisiontree1 = DecisionTree(at_valve=start, timer=timer)
    decisiontree2 = DecisionTree(at_valve=start, timer=timer)
    q: deque[DecisionTree] = deque(
        [decisiontree1, decisiontree2]
    )  # keep a deque of things to try out
    # print(f"initial queue: {q}")
    current_max = [0, 0]
    count = 1
    while q:
        count += 1
        # if count % 1000 == 0:
        #     print(count)

        decisiontrees = [q.pop(), q.pop()]
        """
        this is wrong because the timing between the two decisiontrees is off
        """

        for i in range(2):
            current = decisiontrees[i].at_valve

            if decisiontrees[i].release > current_max[i]:
                current_max[i] = decisiontrees[i].release

            still_closed = [
                v for v in release_valves
                if v not in decisiontrees[0].open_valves + decisiontrees[1].open_valves
            ]
            # heuristic to kick out paths that cannot make it anyway
            overestimation = decisiontrees[i].timer * sum(valves[v][0] for v in still_closed)
            if decisiontrees[i].release + decisiontrees[(i + 1) % 2].release + overestimation < sum(current_max):
                continue

            # go to next closed valves
            for valve in still_closed:
                distance = len(shortest_paths[current][valve])
                if decisiontrees[i].timer - distance >= 0:
                    # no extra -1 for opening the valve because distance is 1 too large
                    release = valves[valve][0] * (decisiontrees[i].timer - distance)
                    d = DecisionTree(
                        at_valve=valve,
                        timer=decisiontrees[i].timer - distance,
                        release=decisiontrees[i].release + release,
                        open_valves=decisiontrees[i].open_valves + decisiontrees[(i+1)%2].open_valves + [valve],
                        parent=decisiontrees[i],
                    )
                    # print(f"  up next: {d}")
                    decisiontrees[i].children.append(d)
                    q.append(d)

    return sum(current_max)


def get_leaves(decisiontree: DecisionTree) -> list[DecisionTree]:
    leaves = []
    if not decisiontree.children:
        return [decisiontree]
    for child in decisiontree.children:
        leaves.extend(get_leaves(child))
    return leaves


def all_shortest_paths(valves: Parsed) -> dict[str, dict[str, list[str]]]:
    """shortest paths from every point to every other point"""
    neighbors = lambda graph, p: graph[p][1]
    shortest_paths = {}
    # horribly inefficient but okay, graph is small
    for start in valves:
        paths = utils.BFS(valves, start, neighbors)
        shortest_paths[start] = {}
        for end in valves:
            if start == end:
                continue
            shortest_paths[start][end] = utils.shortestpath(paths, start, end)
    return shortest_paths


# PART 1
@measure_time
def solve1(data):
    tree, maxrelease = build_decisiontree(data, 30, "AA")

    # debugging ----------------------------------
    # leaves = get_leaves(tree)
    # best = max(leaves, key=lambda l: l.release)
    # print(f"best leaf: {best}")
    # path = [best.at_valve]
    # parent = best
    # while parent:=parent.parent:
    #     path.append(parent.at_valve)
    # print(f"path: {list(reversed(path))}")
    # assert best.release == maxrelease
    # ---------------------------------------------

    return maxrelease


# PART 2
@measure_time
def solve2(data):
    return 0
    return build_decisiontree_p2(data, 26, "AA")


if __name__ == "__main__":
    data = parse(open("input.txt").read().strip())
    print("Part 1: {}".format(solve1(data)))
    print("Part 2: {}".format(solve2(data)))

    print("\nTime taken:")
    for func, time in measure_time.times:
        print(f"{func:8}{time}s")
    print("----------------")
    print("total   {}s".format(sum(t for _, t in measure_time.times)))

