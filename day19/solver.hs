{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ 
import Data.List
import Text.Parsec
    ( Parsec
    , string
    , many
    , digit
    , char
    , letter
    , sepBy
    , endBy
    , (<|>)
    , try
    )
import qualified Text.Parsec as P
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Utils
import AoC

getInput :: IO String
getInput = readFile "input.txt"

--------------------------------------------------------------------

type Parsed = [Blueprint]
type Sol1 = Int
type Sol2 = Int

data ResourceType = Ore | Clay | Obsidian | Geode deriving (Eq, Ord, Show, Read)
data Resource = Resource
    { amount:: Int
    , rtype :: ResourceType
    } deriving (Eq, Show)

instance Ord Resource where
    compare (Resource _ t1) (Resource _ t2) = compare t1 t2

toMap :: [Resource] -> Map ResourceType Int
toMap rs = Map.fromList [(rtype r, amount r) | r <- rs]

type ResourceCount = Map ResourceType Int

type RoboType = ResourceType
type RoboCount = Map RoboType Int

data Blueprint = Blueprint
    { bpID :: Int
    , cost :: Map RoboType [Resource]
    } deriving (Show)

blueprintParser :: Parsec String st Blueprint
blueprintParser = Blueprint
    <$> idParser
    <*> (Map.fromList
        <$> ( (,) <$ string " Each " <*> typeparser <* string " robot costs " <*> resourcesParser
            ) `endBy` (char '.')
        )

typeparser = read . capitalize <$> many letter

idParser :: Parsec String st Int
idParser = read <$ string "Blueprint " <*> many digit <* string ":"

resourcesParser :: Parsec String st [Resource]
resourcesParser = toResources
    <$> ( (,) <$> many digit <* char ' ' <*> typeparser
        ) `sepBy` (string " and ")
    where
        toResources [] = []
        toResources ((n,t):rs) = Resource (read n) t : toResources rs

parse :: String -> Parsed
parse = rights' . linesWith (P.parse blueprintParser "")
    where
        rights' [] = []
        rights' (eb:bs) = case eb of
            Right b -> b : rights' bs
            Left msg -> error $ show msg

suffice :: [Resource] -> ResourceCount -> Bool
suffice rs count = all (\r -> amount r <= count ! (rtype r)) rs

buildRobot :: RoboType -> Blueprint -> (ResourceCount, RoboCount) -> (ResourceCount, RoboCount)
buildRobot rt blueprint (resourcecount, robocount) =
    if suffice costs resourcecount then
        (Map.unionWith (-) resourcecount (toMap costs), Map.insertWith (+) rt 1 robocount)
    else
        (resourcecount, robocount)
    where costs = cost blueprint ! rt


solve1 :: Parsed -> Sol1
solve1 parsed = undefined

solve2 :: Parsed -> Sol2
solve2 parsed = undefined


testdata = [r|Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
|]
testresult1 = 0
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
