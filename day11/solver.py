import sys

sys.path.insert(0, "../utils/py")
import utils
import rich
from collections import Counter


measure_time = utils.stopwatch()


@measure_time
def parse(raw_data: str):
    monkeys = []
    for monks in raw_data.split("\n\n"):
        monks = monks.splitlines()
        index = int(monks[0].split()[-1][:-1])
        items = list(map(int, monks[1].split(":")[1].split(",")))
        operation = monks[2].split("=")[1].strip()
        condition = (int(monks[3].split()[-1]), int(monks[4].split()[-1]), int(monks[5].split()[-1]))
        monkeys.append({
            "index": index,
            "items": items,
            "operation": operation,
            "condition": condition
        })
    return monkeys


# PART 1
@measure_time
def solve1(data):
    monkeys = data[:]
    monkeycount = Counter()
    for round in range(20):
        for i, monkey in enumerate(monkeys):
            while monkey["items"]:
                monkeycount[i] += 1
                item = monkey["items"].pop(0)
                newitem = eval(monkey["operation"], {}, {"old": item}) // 3
                divby, then, otherwise = monkey["condition"]
                if newitem % divby == 0:
                    newmonkey = then
                else:
                    newmonkey = otherwise
                monkeys[newmonkey]["items"].append(newitem)
                # rich.print(monkeys)
    m1, m2, *_ = sorted(monkeycount.values(), reverse=True)
    return m1 * m2


# PART 2
@measure_time
def solve2(data):
    monkeys = data[:]
    monkeycount = Counter()
    items = []
    modnumbers = [m["condition"][0] for m in monkeys]
    print(modnumbers)
    n = 1
    for m in modnumbers:
        n *= m
    # print(n)
    for monkey in monkeys:
        for item in monkey["items"]:
            items.append({"worry": item, "monkey": monkey["index"]})

    for round in range(10000):
        # print(f"{round=}")
        for item in items:
            # print(f"  processing {item=}")
            monkey = monkeys[item["monkey"]]
            while item["monkey"] >= monkey["index"]:
                monkey = monkeys[item["monkey"]]
                monkeycount[item["monkey"]] += 1
                item["worry"] = eval(monkey["operation"], {}, {"old": item["worry"]}) % n
                # print(f"    operated on worry level, got {item['worry']=}")
                divby, then, otherwise = monkey["condition"]
                if item["worry"] % divby == 0:
                    # print(f"    was divisible by {divby}, throw to monkey {then}")
                    item["monkey"] = then
                else:
                    # print(f"    was not divisible by {divby}, throw to monkey {otherwise}")
                    item["monkey"] = otherwise
            # print(f"  {item=}")

    # print(monkeycount)
    m1, m2, *_ = sorted(monkeycount.values(), reverse=True)
    # return 0
    return m1 * m2


if __name__ == "__main__":
    data = parse(open("input.txt").read().strip())
    print("Part 1: {}".format(solve1(data)))
    print("Part 2: {}".format(solve2(data)))

    print("\nTime taken:")
    for func, time in measure_time.times:
        print(f"{func:8}{time}s")
    print("----------------")
    print("total   {}s".format(sum(t for _, t in measure_time.times)))

