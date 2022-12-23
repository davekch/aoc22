import pytest
from solver import parse, solve1, solve2

TESTDATA = """....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..
"""
TESTDATA2 = """.....
..##.
..#..
.....
..##.
.....
"""

@pytest.fixture
def parsed_data():
    return parse(TESTDATA), parse(TESTDATA2)


def test_parse():
    data = parse(TESTDATA)
    # asserts go here


# PART 1
def test_solve1(parsed_data):
    parsed1, parsed2 = parsed_data
    solution1 = solve1(parsed1)
    assert solution1 == 110
    # solution2 = solve1(parsed2)
    # assert False


# PART 2
def test_solve2(parsed_data):
    parsed1, _ = parsed_data
    solution = solve2(parsed1)
    assert solution == 20
