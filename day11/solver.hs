{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ 
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Map (Map)
import Utils
import AoC
import qualified Text.Parsec as P
import Text.Parsec (
    Parsec,
    char,
    string,
    many1,
    many,
    digit,
    oneOf,
    newline,
    sepBy1,
    (<|>)
    )

getInput :: IO String
getInput = readFile "input.txt"

--------------------------------------------------------------------

type Parsed = Map Int Monkey
type Sol1 = Int
type Sol2 = Int

data Monkey = Monkey {
    items :: [Int],
    -- operation on the worry level
    operation :: MonkeyOperation,
    -- to which monkey to throw depending on worry level
    target :: MonkeyCondition
} deriving (Show)

data Operator = Add | Mult deriving (Show)
data Value = Const Int | Var deriving (Show)
data MonkeyOperation = MOp Value Operator Value deriving (Show)
--                devisible by, then, else
data MonkeyCondition = MCon Int Int Int deriving (Show)

intparser :: Parsec String st Int
intparser = read <$> many1 digit

itemsparser :: Parsec String st [Int]
itemsparser = string "  Starting items: " *> (
    (:) <$> intparser <*> many (string ", " *> intparser)
    )

operatorparser :: Parsec String st Operator
operatorparser = chooseop <$> oneOf "+*"
    where 
        chooseop '+' = Add
        chooseop '*' = Mult

-- valueparser = undefined
valueparser :: P.Parsec String st Value
valueparser = (Var <$ string "old") <|> (Const <$> intparser)

operationparser :: Parsec String st MonkeyOperation
operationparser = MOp
    <$ string "  Operation: new = "
    <*> valueparser
    <*> (char ' ' *> operatorparser)
    <*> (char ' ' *> valueparser)

conditionparser :: Parsec String st MonkeyCondition
conditionparser = MCon
    <$> (string "  Test: divisible by " *> intparser <* newline)
    <*> (string "    If true: throw to monkey " *> intparser <* newline)
    <*> (string "    If false: throw to monkey " *> intparser)

monkeyparser :: Parsec String st (Int, Monkey)
-- monkeyparser :: Parsec String st Monkey
monkeyparser = (,)
    <$> (string "Monkey " *> intparser <* char ':' <* newline)
    <*> (Monkey
        <$> (itemsparser <* newline)
        <*> (operationparser <* newline)
        <*> conditionparser
    )

parse :: String -> Parsed
parse = Map.fromList . map parsemonkey . splitOn "\n\n"
    where
        parsemonkey monkeystr = case P.parse monkeyparser "" monkeystr of
            Right monkey -> monkey
            Left err -> undefined

solve1 :: Parsed -> Sol1
solve1 parsed = undefined

solve2 :: Parsed -> Sol2
solve2 parsed = undefined


testdata = [r|Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1|]
testresult1 = 10605
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
