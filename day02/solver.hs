{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ 
import Data.List
import Data.Char
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Utils as U
import Utils (CLIOptions, clioptions)

getInput :: IO String
getInput = readFile "input.txt"

--------------------------------------------------------------------

type Parsed = [(Char, Char)]
type Sol1 = Int
type Sol2 = Int

data RPS = Rock | Paper | Scissors deriving (Eq, Show, Ord, Enum)

convert :: Char -> RPS
convert x
    | x `elem` "ABC" = toEnum (ord x - ord 'A')
    | x `elem` "XYZ" = toEnum (ord x - ord 'X')
convert _ = undefined

parse :: String -> Parsed
parse = map (\(a:_:b:[]) -> (a, b)) . lines

beatsmap :: Bimap RPS RPS
beatsmap = Bimap.fromList [(Rock, Scissors), (Paper, Rock), (Scissors, Paper)]

winscore :: (RPS, RPS) -> Int
winscore (x, y)
    | x == y = 3
    | otherwise = if (x, y) `Bimap.pairMember` beatsmap then 6 else 0

score :: (RPS, RPS) -> Int
score (x, y) = (winscore (x, y)) + (fromEnum y + 1)

solve1 :: Parsed -> Sol1
solve1 = sum . map score . map (\(x, y) -> (convert x, convert y))

strategy :: (Char, Char) -> RPS
strategy (xx, y)
    | y == 'X' = beatsmap Bimap.! x   -- loose
    | y == 'Y' = x                     -- draw
    | y == 'Z' = beatsmap Bimap.!> x    -- win
    where
        x = convert xx

scorep2 :: (Char, Char) -> Int
scorep2 (x, y) = score (convert x, strategy (x, y))


solve2 :: Parsed -> Sol2
solve2 = sum . map scorep2


testdata = [r|A Y
B X
C Z|]
testresult1 = 15
testresult2 = 12

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
