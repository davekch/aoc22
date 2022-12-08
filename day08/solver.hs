{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ 
import Data.List
import qualified Data.Map as Map
import Data.Vector (Vector)
import Data.Char (ord)
import Utils
import AoC
import Geometry

getInput :: IO String
getInput = readFile "input.txt"

--------------------------------------------------------------------

type Point = Vector Int
type Forest = Grid Int Int

type Parsed = Forest
type Sol1 = Int
type Sol2 = Int

parse :: String -> Parsed
parse = Map.map ((flip (-) 48) . ord) . gridFromString

visible :: Forest -> Point -> Bool
visible grid p = (all smaller left) || (all smaller right) || (all smaller up) || (all smaller down) || (any (==[]) [left, right, up, down])
    where
        smaller = (< grid Map.! p)
        left  = map snd $ walkGrid grid (vadd (mkVec [-1,0])) p
        right = map snd $ walkGrid grid (vadd (mkVec [1,0])) p
        down  = map snd $ walkGrid grid (vadd (mkVec [0,1])) p
        up    = map snd $ walkGrid grid (vadd (mkVec [0,-1])) p

solve1 :: Parsed -> Sol1
solve1 parsed = length . filter (visible parsed) . Map.keys $ parsed

view :: Forest -> Point -> Int
view grid p = product $ map countVisible [left, right, up, down]
    where
        left  = map snd $ walkGrid grid (vadd (mkVec [-1,0])) p
        right = map snd $ walkGrid grid (vadd (mkVec [1,0])) p
        down  = map snd $ walkGrid grid (vadd (mkVec [0,1])) p
        up    = map snd $ walkGrid grid (vadd (mkVec [0,-1])) p
        countVisible = length . takeWhileInclusive (< grid Map.! p)

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
