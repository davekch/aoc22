import sys

sys.path.insert(0, "../utils/py")
import utils
from collections import deque
from dataclasses import dataclass, field
from typing import Self
import itertools


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


def build_decisiontree(
        valves: Parsed,
        timer: int,
        start: str,
        release_valves: list,   # valves we need to open eventually
        shortest_paths: dict   # all the shortest paths from one valve to any other
    ) -> tuple[DecisionTree, int]:
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


def build_decisiontree_p2(valves: Parsed, timer: int, start: str) -> tuple[tuple[DecisionTree], int]:
    # valves we need to open; discard valves with release 0
    all_release_valves = [v for v in valves if valves[v][0] > 0]
    shortest_paths = all_shortest_paths(valves)
    counter = 0
    current_max = 0
    best_me = None
    best_elephant = None
    # iterate over all possibilities to split the valves between me and the elephant
    # n is the amount of valves I get; since there's no difference between me and the elephant
    # it's enough to take n up to total amount of valves // 2 + 1 and since the optimal
    # solution will be with us roughly opening the same amount of valves, let's not start at 0
    for n in range(len(all_release_valves) // 3, len(all_release_valves) // 2 + 1):
        for my_valves in itertools.combinations(all_release_valves, n):
            counter += 1
            if counter % 200 == 0:
                print(counter)

            elephant_valves = [v for v in all_release_valves if v not in my_valves]
            # this is horribly inefficient
            me, my_max = build_decisiontree(valves, timer, start, my_valves, shortest_paths)
            elephant, elephant_max = build_decisiontree(valves, timer, start, elephant_valves, shortest_paths)
            if my_max + elephant_max > current_max:
                current_max = my_max + elephant_max
                best_me = me
                best_elephant = elephant

    return (best_me, best_elephant), current_max


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
    valves = [v for v in data if data[v][0]>0]
    shortest_paths = all_shortest_paths(data)
    tree, maxrelease = build_decisiontree(data, 30, "AA", valves, shortest_paths)

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
    (me, elephant), maxrelease = build_decisiontree_p2(data, 26, "AA")

    # debugging ------------------------------------
    # leaves_me = get_leaves(me)
    # leaves_elephant = get_leaves(elephant)
    # best_me = max(leaves_me, key=lambda l: l.release)
    # best_elephant = max(leaves_elephant, key=lambda l: l.release)
    # path_me = [best_me.at_valve]
    # parent = best_me
    # while parent:=parent.parent:
    #     path_me.append(parent.at_valve)
    # path_elephant = [best_elephant.at_valve]
    # parent = best_elephant
    # while parent:=parent.parent:
    #     path_elephant.append(parent.at_valve)
    # print(f"my path: {list(reversed(path_me))}")
    # print(f"elephant path: {list(reversed(path_elephant))}")
    # ---------------------------------------------
    
    return maxrelease

if __name__ == "__main__":
    data = parse(open("input.txt").read().strip())
    print("Part 1: {}".format(solve1(data)))
    print("Part 2: {}".format(solve2(data)))

    print("\nTime taken:")
    for func, time in measure_time.times:
        print(f"{func:8}{time}s")
    print("----------------")
    print("total   {}s".format(sum(t for _, t in measure_time.times)))

