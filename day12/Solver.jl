module Solver

include("../utils/jl/Utils.jl")
using .Utils

Point = Tuple{Int,Int}
struct Graph <: Graphs.AbstractGraph{Point}
    nodes::Dict{Point,Char}
end

function parse_input(raw_data::String)
    graph = Graph(Dict())
    for (y, line) ∈ enumerate(raw_data |> split)
        for (x, c) ∈ enumerate(line)
            graph.nodes[(x, y)] = c
        end
    end
    startnode = [p for (p, c) ∈ graph.nodes if c == 'S'][1]
    endnode = [p for (p, c) ∈ graph.nodes if c == 'E'][1]
    graph.nodes[startnode] = 'a'
    graph.nodes[endnode] = 'z'
    graph, startnode, endnode
end
export parse_input

function Graphs.neighbors(graph::Graph, p::Point)
    (x, y) = p
    adjacent = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
    return [n for n ∈ adjacent if (n[1] >= 0 && n[2] >= 0) && n ∈ keys(graph.nodes) && Int(graph.nodes[n]) <= Int(graph.nodes[p]) + 1]
end

function solve1(parsed)
    bfsmap = Graphs.BFS(parsed...)
    length(Graphs.shortestpath(bfsmap, parsed[2:end]...)) - 1
end
export solve1

function solve2(parsed)
end
export solve2


testinput = """Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
"""
testanswer_1 = 31
testanswer_2 = 0
export testinput, testanswer_1, testanswer_2

include("../utils/jl/AoC.jl")

end # module Solver
