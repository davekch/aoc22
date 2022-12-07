import pytest
from solver import parse, solve1, solve2

TESTDATA = """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
"""

@pytest.fixture
def parsed_data():
    return parse(TESTDATA)


def test_parse():
    data = parse(TESTDATA)
    


# PART 1
def test_solve1(parsed_data):
    solution = solve1(parsed_data)
    assert solution == 95437

# PART 2
def test_solve2(parsed_data):
    solution = solve2(parsed_data)
    assert solution == 24933642