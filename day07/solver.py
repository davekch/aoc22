import sys

sys.path.insert(0, "../utils/py")
import utils
from collections import defaultdict
import rich


measure_time = utils.stopwatch()


@measure_time
def parse(raw_data):
    # assumes that no directory is called "files"
    tree = {"/": {"files": []}}
    pwd = []
    for line in raw_data.splitlines():
        # print(line)
        # rich.print(tree)
        if line[0] == "$":
            if "cd" in line:
                newdir = line.split()[-1]
                if newdir == "..":
                    pwd = pwd[:-1]
                else:
                    pwd.append(newdir)
            elif "ls" in line:
                # nothing to do
                continue
        else:
            # print(f"alter {'/'.join(pwd)}")
            # get the current directory and append contents to it
            dir = tree
            for d in pwd:
                dir = dir[d]
            if "dir" in line:
                dir[line.split()[-1]] = {"files": []}
            else:
                size, name = line.split()
                size = int(size)
                dir["files"].append((name, size))
    
    return calcsizes("/", tree)


def calcsizes(name, tree):
    if name == "files":
        size = sum([f[1] for f in tree["files"]])
        return size, {"total": size}
    sizes = {"total": 0}
    for nname, item in tree[name].items():
        subsize, subsizes = calcsizes(nname, tree[name])
        sizes["total"] += subsize
        sizes[nname] = subsizes
    return sizes["total"], sizes


def filtersizes(sizes, conditional):
    filtered = []
    for name, ssizes in sizes.items():
        if name == "total":
            if conditional(ssizes):
                filtered.append(ssizes)
            continue
        if name == "files":
            continue
        filtered += filtersizes(ssizes, conditional)
    return filtered

# PART 1
@measure_time
def solve1(data):
    _, sizes = data
    return sum(filtersizes(sizes, lambda s: s<=100000))


# PART 2
@measure_time
def solve2(data):
    total, sizes = data
    required = 30000000
    available = 70000000
    mustdelete = required - (available - total)
    candidates = filtersizes(sizes, lambda s: s>=mustdelete)
    return min(candidates)


if __name__ == "__main__":
    data = parse(open("input.txt").read().strip())
    print("Part 1: {}".format(solve1(data)))
    print("Part 2: {}".format(solve2(data)))

    print("\nTime taken:")
    for func, time in measure_time.times:
        print(f"{func:8}{time}s")
    print("----------------")
    print("total   {}s".format(sum(t for _, t in measure_time.times)))

