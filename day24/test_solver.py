import pytest
from solver import parse, solve1, solve2, blizzards_after

import sys
sys.path.insert(0, "../utils/py")
import utils


TESTDATA = """#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#
"""
TESTDATA_SIMPLE = """#.#####
#.....#
#>....#
#.....#
#...v.#
#.....#
#####.#
"""

@pytest.fixture
def parsed_data():
    return parse(TESTDATA)


def test_parse():
    data = parse(TESTDATA)
    # asserts go here


# PART 1
def test_solve1(parsed_data):
    solution = solve1(parsed_data)
    assert solution == 18


# PART 2
def test_solve2(parsed_data):
    solution = solve2(parsed_data)
    assert solution == 54
