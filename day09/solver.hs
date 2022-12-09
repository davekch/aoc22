{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ 
import Data.List
import Utils
import Geometry
import qualified Data.Vector as Vec
import Data.Vector (Vector)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import AoC

getInput :: IO String
getInput = readFile "input.txt"

--------------------------------------------------------------------

type Parsed = [Direction]
type Sol1 = Int
type Sol2 = Int

type Direction = Vector Int
data Move = Move Direction Int deriving (Show)
type Position = Vector Int
-- represents one segment of a rope
data Segment = Segment { headPos :: Position, tailPos :: Position } deriving (Show)

steps :: [Move] -> [Direction]
steps = concat . map stepify
    where stepify (Move d n) = take n $ repeat d

parse :: String -> Parsed
parse = steps . linesWith parseLine
    where
        parseLine line = let [dir,dist] = words line in Move (translate dir) (read dist)
        translate d = case d of
            "D" -> mkVec [0, -1]
            "U" -> mkVec [0, 1]
            "R" -> mkVec [1, 0]
            "L" -> mkVec [-1, 0]

-- how one segment of the rope moves one step in a direction
-- one step is important because we need to keep track
-- of the position for each step (do not do R 4 at once but rather RRRR)
moveSegment :: Direction -> Segment -> Segment
moveSegment dir segment = Segment headpos' tailpos'
    where
        headpos = headPos segment
        -- the head just moves according to the direction
        headpos' = vadd dir headpos
        tailpos' = follow headpos' (tailPos segment)

follow :: Position -> Position -> Position
follow newhead tail = 
    -- if neither x nor y position differ more than 1, tail does not move
    if Vec.all (<=1) (Vec.map abs direction) then
        tail
    else
        -- the tail moves in the direction of the head, but only 1 x and y
        vadd tail (Vec.map signum direction)
    where
        direction = vsub newhead tail

startsegment :: Segment
startsegment = Segment (mkVec [0,0]) (mkVec [0,0])

-- keep track of where the tail was
tailtrail :: Set Position
tailtrail = Set.singleton (mkVec [0,0])

type Rope = [Segment]

moveRope :: Direction -> Rope -> Rope
moveRope d (h:tls) = h' : follows h' tls
    where h' = moveSegment d h

-- make the rope follow the rope segment
follows :: Segment -> Rope -> Rope
follows _ [] = []
follows h (t:ts) = t' : follows t' ts
    where
        t' = moveSegment dir t
        dir = Vec.map signum $ vsub (tailPos h) (headPos t)

type RopeState = (Set Position, Rope)

update :: RopeState -> Direction -> RopeState
update (trail, rope) d = (Set.insert (tailPos (last rope')) trail, rope')
    where
        rope' = moveRope d rope

solve :: Rope -> Parsed -> RopeState
solve startrope = foldl update (tailtrail, startrope)

solve1 :: Parsed -> Sol1
solve1 = Set.size . fst . solve [startsegment]

solve2 :: Parsed -> Sol2
-- take 9 $ repeat startrope because a ropelist of 9 segments has 10 knots
solve2 = Set.size . fst . solve (take 9 $ repeat startsegment)

-- in ghci, do
-- > parsed <- parse <$> getInput
-- > let (trail, ropes) = solve (take 9 $ repeat startsegment) parsed
-- > visualize trail
visualize :: Set Position -> IO ()
visualize trail = putStr . Geometry.prettyShow "." . Map.fromList $ zipWith (,) (Set.toList trail) (repeat "#")

testdata = [r|R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2|]
testdata2 = [r|R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20|]
testresult1 = 13
testresult2 = 36

--------------------------------------------------------------------

test1 = test (solve1 . parse) testresult1 testdata
test2 = test (solve2 . parse) testresult2 testdata2

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
