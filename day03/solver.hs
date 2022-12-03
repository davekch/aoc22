{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ 
import Data.List
import Data.List.Split (chunksOf)
import Data.Char
import Utils as U
import Utils (CLIOptions, clioptions)

getInput :: IO String
getInput = readFile "input.txt"

--------------------------------------------------------------------

type Parsed = [(String, String)]
type Sol1 = Int
type Sol2 = Int

parse :: String -> Parsed
parse input = [splitAt (flip div 2 $ length l) l | l <- lines input]

priority :: Char -> Int
priority a
    | isLower a = ord a - ord 'a' + 1
    | isUpper a = ord a - ord 'A' + priority 'z' + 1

-- get the char that is in both strings
inBoth :: (String, String) -> Char
inBoth ((c:s1), s2)
    | c `elem` s2 = c
    | otherwise = inBoth (s1, s2)

solve1 :: Parsed -> Sol1
solve1 = sum . map (priority . inBoth)

--- split rucksacks into groups of 3 and also join back together the compartments
splitGroups :: Parsed -> [[String]]
splitGroups = chunksOf 3 . map (\(s1, s2) -> s1 ++ s2)

inAll :: [String] -> Char
inAll ((c:s1):s2:s3:[])
    | (c `elem` s2) && (c `elem` s3) = c
    | otherwise = inAll (s1:s2:s3:[])

solve2 :: Parsed -> Sol2
solve2 = sum . map (priority . inAll) . splitGroups


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
