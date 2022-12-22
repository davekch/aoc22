{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ 
import Data.List
import Text.Parsec
    ( Parsec
    , digit
    , oneOf
    , many1
    , (<|>)
    )
import qualified Text.Parsec as P
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Utils
import AoC
import Geometry
import Debug.Trace (trace)

getInput :: IO String
getInput = readFile "input.txt"

--------------------------------------------------------------------

type Parsed = (Grid Int Char, [Instruction])
type Sol1 = Int
type Sol2 = Int

data Turn = L | R deriving (Eq, Show, Read)
data Instruction = Forward Int | Turn Turn deriving (Eq, Show)
type Position = Vector Int
type Direction = Vector Int

parse :: String -> Parsed
parse input = (grid, instructions)
    where
        inputlines = lines input
        griddata = init inputlines
        instructiondata = last inputlines
        grid = Map.filter (/=' ') $ gridFromString (unlines griddata)
        instructions = case P.parse instructionParser "" instructiondata of
            Right ins -> ins
            Left err -> error $ show err

instructionParser :: Parsec String st [Instruction]
instructionParser = many1 (
        (Forward . read <$> many1 digit)
        <|> (Turn . read . (:[]) <$> oneOf "RL")
    )

step :: Grid Int Char -> Position -> Direction -> Position
step grid pos dir = 
    if grid ! newpos == '#' then
        pos
    else
        newpos
    where
        newpos =
            if (vadd pos dir) `Map.member` grid then
                vadd pos dir
            else
                -- get the current row/column
                let line = filter (\p -> ((dir Vec.! 1)/=0 && p Vec.! 0 == pos Vec.! 0) || ((dir Vec.! 0)/=0) && p Vec.! 1 == pos Vec.! 1) (Map.keys grid) in
                    if dir Vec.! 0 > 0 || dir Vec.! 1 > 0 then
                        minimum line
                    else
                        maximum line

steps :: Grid Int Char -> Position -> Direction -> Int -> Position
steps _ pos _ 0 = pos
steps grid pos dir n = steps grid (step grid pos dir) dir (n-1)

followInstructions :: Grid Int Char -> [Instruction] -> Position -> Direction -> (Position, Direction)
followInstructions _ [] pos dir = (pos, dir)
followInstructions grid (i:is) pos dir = -- trace (show pos ++ show dir ++ show i) $
    case i of
        Forward n -> followInstructions grid is (steps grid pos dir n) dir
        Turn L -> followInstructions grid is pos (matmul rot2D90 dir)
        Turn R -> followInstructions grid is pos (matmul rot2D90' dir)

password :: (Position, Direction) -> Int
password (pos, dir) = 1000 * ((pos Vec.! 1) + 1) + 4 * ((pos Vec.! 0) + 1) + facing
    where
        facing =
            if dir == mkVec [1,0] then 0
            else if dir == mkVec [0,1] then 1
            else if dir == mkVec [-1,0] then 2
            else 3

solve1 :: Parsed -> Sol1
solve1 (grid, instructions) = password $ followInstructions grid instructions start (mkVec [1, 0])
    where
        start = minimum $ filter (\p -> p Vec.! 1 == 0) (Map.keys grid)

solve2 :: Parsed -> Sol2
solve2 parsed = undefined


testdata = [r|        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5|]
testresult1 = 6032
testresult2 = 5031

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
