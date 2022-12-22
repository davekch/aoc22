{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ 
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Function (on)
import Utils
import AoC
import Geometry

getInput :: IO String
getInput = readFile "input.txt"

--------------------------------------------------------------------

type Parsed = [Cube]
type Sol1 = Int
type Sol2 = Int

type Point = Vector Int
type Surface = Set Point
type Shape = Set Surface
type Cube = Shape

cube :: Cube   -- Set (Set (Vector Int))
cube = Set.fromList $ map (Set.fromList . (map mkVec)) [
    [[0,0,0], [1,0,0], [1,0,1], [0,0,1]],  -- bottom
    [[0,0,0], [1,0,0], [1,1,0], [0,1,0]],  -- front
    [[1,0,0], [1,0,1], [1,1,1], [1,1,0]],  -- right
    [[0,0,1], [1,0,1], [1,1,1], [0,1,1]],  -- back
    [[0,0,0], [0,0,1], [0,1,1], [0,1,0]],  -- left
    [[0,1,0], [1,1,0], [1,1,1], [0,1,1]]   -- top
    ]

shift :: Point -> Cube -> Cube
shift p = Set.map $ Set.map (vadd p)

-- a shape is a set of surfaces; if we add one cube to a shape we add the cube's surfaces
-- which are not yet present and remove the surfaces which are in both the shape
-- and the cube (union (difference A B) (difference B A))
buildShape :: [Cube] -> Shape
buildShape = foldl buildShape' Set.empty
    where
        buildShape' :: Shape -> Cube -> Shape
        buildShape' shape cube = Set.union (shape Set.\\ cube) (cube Set.\\ shape)

parse :: String -> Parsed
parse input = [shift offset cube | offset <- offsets]
    where offsets = linesWith (mkVec . parseInts) input

solve1 :: Parsed -> Sol1
solve1 = length . buildShape

-- take a shape and filter out shapes that are not connected to the outer
-- surface (removing cavities)
outerSurface :: Shape -> Shape
outerSurface shape = foldl connect start shape
    where
        start = Set.singleton $ Set.toAscList shape !! 0
        connect shape surface =
            if any (hasCommonPoints surface) shape
            then Set.insert surface shape
            else shape
        hasCommonPoints s1 s2 = not $ Set.disjoint s1 s2

xy = mkVec [
        mkVec [1, 0, 0],
        mkVec [0, 1, 0],
        mkVec [0, 0, 0]
    ]
xz = mkVec [
        mkVec [1, 0, 0],
        mkVec [0, 0, 0],
        mkVec [0, 0, 1]
    ]
zy = mkVec [
        mkVec [0, 0, 0],
        mkVec [0, 1, 0],
        mkVec [0, 0, 1]
    ]

project :: Vector (Vector Int) -> Surface -> Surface
project mat surface = Set.map (matmul mat) surface

solve2 :: Parsed -> Sol2
solve2 = length . outerSurface . buildShape

-- for debugging
showShape :: Shape -> String
showShape shape = show $ map Set.toList $ Set.toList shape

generateJSON :: String -> Parsed -> IO ()
generateJSON filename parsed = writeFile filename $ [r|{
    "fullshape": |] ++ showShape fullshape ++ [r|,
    "innershape": |] ++ showShape innershape ++ [r|,
    "outershape": |] ++ showShape outershape ++ [r|,
    "shadow": |] ++ show shadow ++ "\n}\n"
        where
            fullshape = buildShape parsed
            outershape = outerSurface fullshape
            innershape = fullshape Set.\\ outershape
            shadow = map Set.toList $ filter (\s -> length s == 4) (
                        (map (project xy) $ Set.toList fullshape)
                        ++
                        (map (project xz) $ Set.toList fullshape)
                        ++
                        (map (project zy) $ Set.toList fullshape)
                    )


testdata = [r|2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5
|]
testresult1 = 64
testresult2 = 58

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
