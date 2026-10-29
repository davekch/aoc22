module Solver

include("../utils/jl/Utils.jl")
using .Utils

parse_input(raw_data) = map.(x -> parse(Int, x), split.(split(raw_data, "\n\n")))
export parse_input

function solve1(parsed)
    map(sum, parsed) |> maximum
end
export solve1

function solve2(parsed)
    top = map(sum, parsed) |> sort
    sum(reverse(top)[1:3])
end
export solve2


testinput = """1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"""
testanswer_1 = 24000
testanswer_2 = 45000
export testinput, testanswer_1, testanswer_2

include("../utils/jl/AoC.jl")

end # module Solver
