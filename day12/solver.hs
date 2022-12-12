{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ 
import Data.List
import Utils
import AoC
import Geometry
import Data.Vector (Vector)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Char (ord)

getInput :: IO String
getInput = readFile "input.txt"

--------------------------------------------------------------------

type Parsed = Grid Int Char
type Node = (Vector Int, Char)
type Sol1 = Int
type Sol2 = Int

parse :: String -> Parsed
parse = gridFromString

neighbors :: Parsed -> Node -> [Node]
neighbors grid (x,a) = [(xn, grid Map.! xn)
    | xn <- [left x, right x, up x, down x],
    xn `Map.member` grid, ord (grid Map.! xn) <= (ord a) + 1
    ]

breadthFirstSearch :: Parsed -> Node -> Node -> Map Node (Maybe Node)
breadthFirstSearch graph start end = bfs (Map.singleton start Nothing) [start] end
    where
        bfs path [] end = path
        bfs path (q:queue) end
            | q == end = path
            | otherwise = bfs (Map.union newparents path) (ns ++ queue) end
            where
                ns = [n | n <- neighbors graph q, not (n `Map.member` path)]
                newparents = Map.fromList $ zip ns (repeat (Just q))

shortestFromBFS :: Node -> Node -> Map Node (Maybe Node) -> [Node]
shortestFromBFS start end parents
    | not $ end `Map.member` parents = []
    | otherwise = shortest' [] parents start end
    where
        shortest' path parents start end = case parents Map.! end of
            Nothing -> path
            Just p -> shortest' (p:path) parents start p


solve1 :: Parsed -> Sol1
solve1 parsed = length . shortestFromBFS start end $ breadthFirstSearch grid start end
    where
        start = (fromJust $ lookupValue 'S' parsed, 'a')
        end = (fromJust $ lookupValue 'E' parsed, 'z')
        grid = uncurry Map.insert start $ uncurry Map.insert end parsed

solve2 :: Parsed -> Sol2
solve2 parsed = undefined


testdata = [r|Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi|]
testresult1 = 31
testresult2 = 0

--------------------------------------------------------------------

test1 = test (solve1 . parse) testresult1 testdata
test2 = test (solve2 . parse) testresult2 testdata

printPart :: (Show a) => Int -> a -> IO ()
printPart part solution = do
    putStr $ "Part " ++ (show part) ++ ": "
    print solution

main' :: CLIOptions -> IO ()
main' (CLIOptions p False) = do
    parsed <- parse <$> getInput
    case p of
        1 -> printPart 1 (solve1 parsed)
        2 -> printPart 2 (solve2 parsed)
        _ -> (do
            printPart 1 (solve1 parsed)
            printPart 2 (solve2 parsed))
main' (CLIOptions p True) = 
    case p of
        1 -> printPart 1 test1
        2 -> printPart 2 test2
        _ -> (do
            printPart 1 test1
            printPart 2 test2)

main = main' =<< clioptions
