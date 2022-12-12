import sys

sys.path.insert(0, "../utils/py")
import utils
import rich
from queue import Queue


measure_time = utils.stopwatch()


@measure_time
def parse(raw_data):
    graph = {}
    for y, line in enumerate(raw_data.splitlines()):
        for x, c in enumerate(line):
            graph[(x, y)] = c
    start = [p for p in graph if graph[p] == "S"][0]
    end   = [p for p in graph if graph[p] == "E"][0]
    graph[start] = "a"
    graph[end] = "z"
    return graph, start, end

def neighbors(graph, p):
    x, y = p
    adjacent = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
    return [n for n in adjacent if (n[0]>=0 and n[1]>=0) and n in graph and ord(graph[n]) <= ord(graph[p])+1]

def breadthfirstsearch(graph, start, end):
    queue = Queue()
    queue.put(start)
    path = {start: None}
    while not queue.empty():
        v = queue.get()
        if v == end:
            return path
        for n in neighbors(graph, v):
            if n not in path:
                path[n] = v
                queue.put(n)
    return path


def shortestpath(graph, start, end):
    pathmap = breadthfirstsearch(graph, start, end)
    if end not in pathmap:
        return []
    path = [end]
    while parent:=pathmap[path[-1]]:
        path.append(parent)
    return list(reversed(path))

# PART 1
@measure_time
def solve1(data):
    return len(shortestpath(*data)) - 1


# PART 2
@measure_time
def solve2(data):
    graph, start, end = data
    starts = [p for p in graph if graph[p] == "a"]
    lengths = []
    for s in starts:
        # this is stupid
        path = shortestpath(graph, s, end)
        if path:
            lengths.append(len(path) - 1)
    return min(lengths)


if __name__ == "__main__":
    data = parse(open("input.txt").read().strip())
    print("Part 1: {}".format(solve1(data)))
    print("Part 2: {}".format(solve2(data)))

    print("\nTime taken:")
    for func, time in measure_time.times:
        print(f"{func:8}{time}s")
    print("----------------")
    print("total   {}s".format(sum(t for _, t in measure_time.times)))

