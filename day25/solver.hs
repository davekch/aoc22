{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ 
import Data.List
import Data.Bimap (Bimap, (!), (!>))
import qualified Data.Bimap as Bimap
import Utils
import AoC

getInput :: IO String
getInput = readFile "input.txt"

--------------------------------------------------------------------

type Parsed = [String]
type Sol1 = String
type Sol2 = Int


-- convert [1,2,3] to [3, 2, 1] and then
-- 3:2:1:[] -> -2:3:1:[] -> -2:-2:2:[] -> reverse -> [2,2,-2]
snafuCoefficients :: Int -> [Int]
snafuCoefficients = reverse . snafuCoefficients' . reverse . baseCoefficients 5
    where
        snafuCoefficients' [] = []
        snafuCoefficients' [a]
            | a == 3 = [-2, 1]
            | a == 4 = [-1, 1]
            | otherwise = [a]
        snafuCoefficients' (a1:a2:rest)
            | a1 == 3 = (-2) : snafuCoefficients' (a2+1:rest)
            | a1 == 4 = (-1) : snafuCoefficients' (a2+1:rest)
            | a1 == 5 = 0 : snafuCoefficients' (a2+1:rest)
            | otherwise = a1 : snafuCoefficients' (a2:rest)


snafuDigit :: Bimap Int String
snafuDigit = Bimap.fromList
    [ (-2, "=")
    , (-1, "-")
    , (0, "0")
    , (1, "1")
    , (2, "2")
    ]

snafuDigits :: Int -> [String]
snafuDigits = map (snafuDigit !) . snafuCoefficients

toSnafu :: Int -> String
toSnafu = concat . snafuDigits

fromSnafu :: String -> Int
fromSnafu = fromSnafu' . enumerate . reverse
    where
        fromSnafu' :: [(Int, Char)] -> Int
        fromSnafu' [] = 0
        fromSnafu' ((i,c):rest) = (snafuDigit !> [c]) * 5 ^ i + fromSnafu' rest


parse :: String -> Parsed
parse = lines

solve1 :: Parsed -> Sol1
solve1 = toSnafu . sum . map fromSnafu

solve2 :: Parsed -> Sol2
solve2 parsed = undefined


testdata = [r|1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122|]
testresult1 = "2=-1=0"
testresult2 = 0

-- extra testcases
toSnafuTestcases = [
    (1, "1"),
    (2, "2"),
    (3, "1="),
    (4, "1-"),
    (5, "10"),
    (6, "11"),
    (7, "12"),
    (8, "2="),
    (9, "2-"),
    (10, "20"),
    (15, "1=0"),
    (20, "1-0"),
    (2022, "1=11-2"),
    (12345, "1-0---0"),
    (314159265, "1121-1110-1=0")
    ]
toSnafuTest = mconcat $ zipWith (test toSnafu) (map snd toSnafuTestcases) (map fst toSnafuTestcases)

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
