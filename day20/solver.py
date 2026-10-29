import sys
import logging

sys.path.insert(0, "../utils/py")
import utils


measure_time = utils.stopwatch()
logger = logging.getLogger(__name__)


@measure_time
def parse(raw_data):
    return [int(x) for x in raw_data.splitlines()]


# PART 1
@measure_time
def solve1(data):
    # decorate all elements with their initial index to make them unique
    data = list(enumerate(data))
    l = len(data)
    for item in data[:]:
        logger.debug("current:")
        logger.debug([v for _, v in data])
        logger.debug(f"  {item[1]} moves")
        index = data.index(item)
        logger.debug(f"  current index: {index}")
        data.pop(index)
        logger.debug(f"  popped it")
        _, jump = item
        if jump >= l:
            jump = jump % (l)
        if jump >= 0:
            newindex = index + jump + 1
            logger.debug(f"  calculated new index {newindex}")
            if newindex > l:
                newindex = newindex % l
                logger.debug(f"  it's beyond the list so i wrapped it to {newindex}")
        else:
            newindex = index + jump
            logger.debug(f"  calculated new index {newindex}")
            if newindex < 0:
                newindex = newindex % l
                logger.debug(f"  it's beyond the list so i wrapped it to {newindex}")

        if newindex >= index:
            newindex -= 1
            logger.debug(f"  account for shift in popped list: {newindex=}")
        data.insert(newindex, item)
        logger.debug(f"  > {item[1]} moves between {data[newindex-1][1]} and {data[(newindex+1)%l][1]}")
        logger.debug(f"  {[v for _, v in data]}")
        logger.debug("")
    
    index0 = [i for i, (_,v) in enumerate(data) if v == 0][0]
    logger.debug(f"0 is at {index0}")
    s1, s2, s3 = data[(index0+1000)%l][1], data[(index0+2000)%l][1], data[(index0+3000)%l][1]
    logger.debug(f"numbers are {s1, s2, s3}")
    return s1 + s2 + s3


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

