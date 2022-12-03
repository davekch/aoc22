{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ 
import Data.List
import Data.List.Split (chunksOf)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Char
import Utils as U
import Utils (CLIOptions, clioptions)

getInput :: IO String
getInput = readFile "input.txt"

--------------------------------------------------------------------

type Parsed = [String]
type Sol1 = Int
type Sol2 = Int

parse :: String -> Parsed
parse = lines

splitCompartments :: String -> [Set Char]
splitCompartments s = setify . splitAt (flip div 2 . length $ s) $ s
    where setify (c1, c2) = [Set.fromList c1, Set.fromList c2] 

commonChar :: [Set Char] -> Char
commonChar = head . Set.toList . U.intersections

priority :: Char -> Int
priority a
    | isLower a = ord a - ord 'a' + 1
    | isUpper a = ord a - ord 'A' + priority 'z' + 1

solve1 :: Parsed -> Sol1
solve1 = sum . map (priority . commonChar . splitCompartments)

solve2 :: Parsed -> Sol2
solve2 = sum . map (priority . commonChar) . chunksOf 3 . map Set.fromList


testdata = [r|vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw|]
testresult1 = 157
testresult2 = 70

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
