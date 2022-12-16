import pytest
from solver import parse, solve1, solve2

TESTDATA = """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
"""


@pytest.fixture
def parsed_data():
    return parse(TESTDATA)


def test_parse():
    data = parse(TESTDATA)
    assert data["AA"] == (0, ["DD", "II", "BB"])
    assert data["HH"] == (22, ["GG"])


# PART 1
def test_solve1(parsed_data):
    solution = solve1(parsed_data)
    assert solution == 1651


# PART 2
def test_solve2(parsed_data):
    solution = solve2(parsed_data)
    assert solution == 1707
