{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ 
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Utils as U
import Utils (CLIOptions, clioptions)

getInput :: IO String
getInput = readFile "input.txt"

--------------------------------------------------------------------

type Parsed = (Map Int String, [Instruction])
type Sol1 = String
type Sol2 = String

data Instruction = Instruction Int Int Int deriving (Show)

parse :: String -> Parsed
parse input = (parseStack . reverse $ stack, parseInstructions moves)
    where
        [stack, moves] = U.groupByEmptyLines input

-- create a stack-map for each line and then fold the resulting list
parseStack :: [String] -> Map Int String
parseStack (index:stack) = foldl (Map.unionWith (++)) emptystack (reverse $ parseStack' stack)
    where
        emptystack = Map.fromList [(i, "") | i<-(U.parseInts index)]
        parseStack' (s:ss) = parseStackLine s : parseStack' ss
        parseStack' [] = []

parseStackLine :: String -> Map Int String
parseStackLine line = Map.filter (/=" ") $ Map.fromList (zip [1..n] cs)
    where
        -- every forth element is a crate in a stack; map (:[]) just converts from [Char] to [String]
        cs = map (:[]) . takeEvery 4 $ drop 1 line
        n = length cs

parseInstructions :: [String] -> [Instruction]
parseInstructions (l:lines) = Instruction n a b : parseInstructions lines
    where [n, a, b] = U.parseInts l
parseInstructions [] = []

move :: Map Int String -> Instruction -> Map Int String
move map (Instruction 1 a b) = Map.adjust (c:) b poppedmap
    where
        c = (head $ map Map.! a)
        poppedmap = Map.adjust tail a map
move map (Instruction n a b) = move (move map (Instruction 1 a b)) (Instruction (n-1) a b)

-- get the first element of each string in the map's elements and fold them to a string
topcrates :: Map Int String -> String
topcrates = Map.foldr ((:) . head) ""

solve1 :: Parsed -> Sol1
solve1 ((map, moves)) = topcrates $ foldl move map moves

movep2 :: Map Int String -> Instruction -> Map Int String
movep2 map (Instruction n a b) = Map.adjust (c++) b poppedmap
    where
        c = take n $ map Map.! a
        poppedmap = Map.adjust (drop n) a map

solve2 :: Parsed -> Sol2
solve2 ((map, moves)) = topcrates $ foldl movep2 map moves

testdata = [r|    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2|]
testresult1 = "CMZ"
testresult2 = "MCD"

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
