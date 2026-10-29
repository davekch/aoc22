module Solver

include("../utils/jl/Utils.jl")
using .Utils

function parse_input(raw_data)
    map.(parse_shape, split.(split(strip(raw_data), "\n"), " "))
end
export parse_input

@enum Shape rock paper scissors

function parse_shape(s)
    if s == "A" || s == "X"
        return rock
    elseif s == "B" || s == "Y"
        return paper
    else
        return scissors
    end
end

function gamescore(first::Shape, second::Shape)
    if first == rock
        if second == rock
            return 3
        elseif second == paper
            return 0
        else
            return 6
        end
    elseif first == paper
        if second == rock
            return 6
        elseif second == paper
            return 3
        else
            return 0
        end
    elseif first == scissors
        if second == rock
            return 0
        elseif second == paper
            return 6
        else
            return 3
        end
    end
end

function solve1(parsed)
    score = 0
    for (other, me) ∈ parsed
        if me == rock
            score += 1
        elseif me == paper
            score += 2
        else
            score += 3
        end
        score += gamescore(other, me)
    end
    score
end
export solve1

function solve2(parsed)
end
export solve2


testinput = """A Y
B X
C Z
"""
testanswer_1 = 15
testanswer_2 = 0
export testinput, testanswer_1, testanswer_2

include("../utils/jl/AoC.jl")

end # module Solver
