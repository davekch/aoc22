{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ 
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Char (ord)
import Utils
import AoC

getInput :: IO String
getInput = readFile "input.txt"

--------------------------------------------------------------------

type Point = (Int, Int)

type Parsed = Map Point Int
type Sol1 = Int
type Sol2 = Int

parse :: String -> Parsed
parse input = Map.fromList (toPointList enumerated)
    where
        enumerated = enumerate . map enumerate . lines $ input

toPointListLine :: (Int, [(Int, Char)]) -> [(Point, Int)]
toPointListLine ((_, [])) = []
toPointListLine ((y, (x,t):ts)) = ((x, y), ord t - 48) : toPointListLine (y, ts)
toPointList = concat . map toPointListLine

visible :: Map Point Int -> Point -> Bool
visible grid p = (all smaller left) || (all smaller right) || (all smaller up) || (all smaller down) || (any (==[]) [left, right, up, down])
    where
        smaller = (< grid Map.! p)
        left  = goFrom p (mapp pred id) grid
        right = goFrom p (mapp succ id) grid
        down  = goFrom p (mapp id succ) grid
        up    = goFrom p (mapp id pred) grid

goFrom :: Point -> (Point -> Point) -> Map Point Int -> [Int]
goFrom p f grid = case Map.lookup p' grid of
    Nothing -> []
    Just h' -> h' : goFrom p' f grid
    where
        p' = f p
    
mapp :: (Int -> Int) -> (Int -> Int) -> Point -> Point
mapp fx fy (x, y) = (fx x, fy y)

solve1 :: Parsed -> Sol1
solve1 parsed = length . filter (visible parsed) . Map.keys $ parsed

view :: Map Point Int -> Point -> Int
view grid p = product $ map countVisible [left, right, up, down]
    where
        left  = lookLeft grid p
        right = lookRight grid p
        down  = lookDown grid p
        up    = lookUp grid p
        countVisible = length . takeWhileInclusive (< grid Map.! p)

lookLeft  grid p = goFrom p (mapp pred id) grid
lookRight grid p = goFrom p (mapp succ id) grid
lookDown  grid p = goFrom p (mapp id succ) grid
lookUp    grid p = goFrom p (mapp id pred) grid

solve2 :: Parsed -> Sol2
solve2 parsed = maximum . map (view parsed) . Map.keys $ parsed


testdata = [r|30373
25512
65332
33549
35390|]
testresult1 = 21
testresult2 = 8

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
