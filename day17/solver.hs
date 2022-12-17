{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ 
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Data.Map as Map
import Data.Function (on)
import Debug.Trace
import Utils
import AoC
import Geometry

getInput :: IO String
getInput = readFile "input.txt"

--------------------------------------------------------------------

type Parsed = [Direction]
type Sol1 = Int
type Sol2 = Int

data Direction = R | L | D deriving (Eq, Show)
type Shape = [Vector Int]
type Cave = Grid Int String
data Blocker = Wall | Ground | Rock deriving (Eq, Show)

parse :: String -> Parsed
-- alternate between left/right and down, do so indefinitely (cycle)
-- make sure to also have a D between cycles (D:) but start with left/right (drop 1)
parse = drop 1 . cycle . (D:) . intersperse D . map parsePush
    where
        parsePush '<' = L
        parsePush '>' = R

-- hardcode the shapes
shapes :: [Shape]
shapes = [
        map mkVec [[0,0], [1,0], [2,0], [3,0]],
        map mkVec [[1,0], [0,1], [1,1], [2,1], [1,2]],
        map mkVec [[0,0], [1,0], [2,0], [2,1], [2,2]],
        map mkVec [[0,0], [0,1], [0,2], [0,3]],
        map mkVec [[0,0], [1,0], [0,1], [1,1]]
    ]

shift :: Direction -> Shape -> Shape
shift R = map right
shift L = map left
shift D = map down

shapeEdges :: Shape -> (Int, Int, Int, Int)
shapeEdges shape = (minx, maxx, miny, maxy)
    where
        minx = (Vec.! 0) . minimumBy (compare `on` (Vec.! 0)) $ shape
        maxx = (Vec.! 0) . maximumBy (compare `on` (Vec.! 0)) $ shape
        miny = (Vec.! 1) . minimumBy (compare `on` (Vec.! 1)) $ shape
        maxy = (Vec.! 1) . maximumBy (compare `on` (Vec.! 1)) $ shape

-- move according to direction but within the cave
moveConstrained :: Cave -> Direction -> Shape -> Either Blocker Shape
moveConstrained grid d shape = 
    if redge' > 6 || ledge' < 0 then
        Left Wall
    else if dedge' < 0 then
        Left Ground
    else if any (flip Map.member grid) shape' then
        Left Rock
    else
        Right shape'
    where
        shape' = shift d shape
        (ledge', redge', dedge', _) = shapeEdges shape'

-- returns the shape and the leftover directions
moveUntilStop :: Cave -> Shape -> [Direction] -> (Shape, [Direction])
moveUntilStop grid shape [] = (shape, [])   -- does never happen but handy for debugging
moveUntilStop grid shape (d:dirs) = case moveConstrained grid d shape of
-- moveUntilStop grid shape (d:dirs) = trace debugmsg $ case moveConstrained grid d shape of
    Left  Wall   -> moveUntilStop grid shape dirs
    -- we only stop if we hit ground/rock on a downwards movement
    Left  _      -> if d == D
                    then
                        (shape, dirs)
                    else
                        moveUntilStop grid shape dirs
    Right shape' -> moveUntilStop grid shape' dirs
    where
        debugmsg = Geometry.prettyShow "." (solidify' "@" grid shape) ++ "move " ++ show d

-- position the shape 2 from the left wall and 3 above the highest point in the cave
initShape :: Cave -> Shape -> Shape
initShape grid shape = map (vadd offset) shape
    where
        offset = mkVec [x0, y0]
        x0 = 2
        y0 = maxy + 4
        (_, _, _, maxy) = gridEdges grid

-- integrate the shape into the grid represented by string
solidify' :: String -> Cave -> Shape -> Cave
solidify' a grid shape = Map.union grid $ Map.fromList (map (,a) shape)
solidify :: Cave -> Shape -> Cave
solidify = solidify' "#"

cave :: Cave
cave = Map.fromList $ zip [mkVec [x,-1] | x<-[-1..7]] (map (:[]) "+-------+")

-- let all the shapes fall into the cave
-- uncomment the `trace` line and comment out the line below it for debugging
fallingRocks :: Cave -> [Shape] -> [Direction] -> Cave
fallingRocks grid [] _ = grid
-- fallingRocks grid (s:shapes) dirs = trace debugmsg $ fallingRocks grid' shapes dirs'
fallingRocks grid (s:shapes) dirs = fallingRocks grid' shapes dirs'
    where
        grid' = solidify grid s'
        (s', dirs') = moveUntilStop grid (initShape grid s) dirs
        debugmsg = "solidify\n" ++ Geometry.prettyShow "." grid

prettyPrint :: Cave -> IO ()
prettyPrint = putStr . Geometry.prettyShow "."

solve1 :: Parsed -> Sol1
solve1 directions = maxy + 1
    where
        (_, _, _, maxy) = gridEdges cave'
        cave' = fallingRocks cave (take 2022 (cycle shapes)) directions

-- takes about 11 years to run
solve2 :: Parsed -> Sol2
solve2 directions = maxy + 1
    where
        (_, _, _, maxy) = gridEdges cave'
        cave' = fallingRocks cave (take 1000000000000 (cycle shapes)) directions


testdata = [r|>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>|]
testresult1 = 3068
testresult2 = 1514285714288

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
