{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ 
import Data.List
import Data.List.Split (splitOn)
import Utils as U
import Utils (CLIOptions, clioptions)

getInput :: IO String
getInput = readFile "input.txt"

--------------------------------------------------------------------

type Parsed = [[[Int]]]
type Sol1 = Int
type Sol2 = Int

parse :: String -> Parsed
parse = map parse' . lines
    where
        parse' =  map (map read . splitOn "-") . splitOn ","

fulloverlaps :: [[Int]] -> Bool
fulloverlaps ([[min1,max1], [min2,max2]]) = (min1>=min2 && max1<=max2) || (min2>=min1 && max2<=max1)

solve1 :: Parsed -> Sol1
solve1 = length . filter fulloverlaps

overlaps :: [[Int]] -> Bool
overlaps ([[min1,max1], [min2,max2]]) =  not (max1<min2) && not (min1>max2)

solve2 :: Parsed -> Sol2
solve2 = length . filter overlaps


testdata = [r|2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8|]
testresult1 = 2
testresult2 = 4

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
