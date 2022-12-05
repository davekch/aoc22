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
        ([stack, moves]) = U.groupByEmptyLines input

parseStack :: [String] -> Map Int String
parseStack (index:stack) = parseStack' stack map
    where
        map = Map.fromList [(i, "") | i<-(U.parseInts index)]
        parseStack' (l:lines) map = parseStack' lines (parseStackLine l 1 map)
        parseStack' ([]) map = map

parseStackLine :: String -> Int -> Map Int String -> Map Int String
parseStackLine ('[':x:']':line) i map = parseStackLine (if line=="" then line else tail line) (i+1) (Map.insertWith (++) i (x:[]) map)
parseStackLine (' ':' ':' ':line) i map = parseStackLine (if line=="" then line else tail line) (i+1) map
parseStackLine ([]) i map = map

parseInstructions :: [String] -> [Instruction]
parseInstructions (l:lines) = Instruction n a b : parseInstructions lines
    where [n, a, b] = U.parseInts l
parseInstructions ([]) = []

move :: Map Int String -> Instruction -> Map Int String
move map (Instruction 1 a b) = Map.adjust (c++) b poppedmap
    where
        c = (head $ map Map.! a) : []
        poppedmap = Map.adjust tail a map
move map (Instruction n a b) = move (move map (Instruction 1 a b)) (Instruction (n-1) a b)

topcrates map = [head $ (map Map.! i) | i<-Map.keys map]

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
