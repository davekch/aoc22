{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ 
import Data.List
import Utils
import Geometry
import qualified Data.Vector as Vec
import Data.Vector (Vector)
import qualified Data.Map as Map
import AoC

getInput :: IO String
getInput = readFile "input.txt"

--------------------------------------------------------------------

type Parsed = [Move]
type Sol1 = Int
type Sol2 = Int

data Direction = D | U | R | L deriving (Read, Show)
data Move = Move Direction Int deriving (Show)
type Position = Vector Int
data Rope = Rope { headPos :: Position, tailPos :: Position } deriving (Show)

steps :: [Move] -> [Direction]
steps = concat . map stepify
    where stepify (Move d n) = take n $ repeat d

parse :: String -> Parsed
parse = linesWith parseLine
    where
        parseLine line = let [dir,dist] = words line in Move (read dir) (read dist)

left, right, up, down :: Position -> Position
left = vadd (mkVec [-1, 0])
right = vadd (mkVec [1, 0])
up = vadd (mkVec [0, 1])
down = vadd (mkVec [0, -1])

-- how the rope moves one step in a direction
-- one step is important because we need to keep track
-- of the position for each step
moveRope :: Direction -> Rope -> Rope
moveRope dir rope = Rope headpos' tailpos'
    where
        headpos = headPos rope
        -- the head just moves according to the direction
        headpos' = case dir of
            D -> down headpos
            U -> up headpos
            L -> left headpos
            R -> right headpos
        tailpos' = follow headpos' (tailPos rope)

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

startrope :: Rope
startrope = Rope (mkVec [0,0]) (mkVec [0,0])

-- keep track of where the tail was
tailtrail :: Grid Int Int
tailtrail = Map.singleton (mkVec [0,0]) 1

keeptrack :: Position -> Grid Int Int -> Grid Int Int
keeptrack p trail = 
    if p `Map.member` trail then
        Map.adjust (+1) p trail
    else
        Map.insert p 1 trail

type RopeState = (Grid Int Int, Rope)

update :: RopeState -> Direction -> RopeState
update (trail, rope) d = (keeptrack (tailPos rope') trail, rope')
    where
        rope' = moveRope d rope

solve1 :: Parsed -> Sol1
solve1 = Map.size . fst . foldl update (tailtrail, startrope) . steps

type Longrope = [Rope]

moveLongrope :: Direction -> Longrope -> Longrope
moveLongrope d (h:tls) = h' : follows h' tls

follows :: Rope -> [Rope] -> [Rope]
follows _ [] = []
follows h (t:ts) = t' : follows t' ts
    where t' = follow (tailPos h) (headPos t) -- does not work, tail does not follow

solve2 :: Parsed -> Sol2
solve2 parsed = undefined


testdata = [r|R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2|]
testresult1 = 13
testresult2 = 0

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
