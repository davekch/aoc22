{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ 
import Data.List
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.Maybe (fromJust)
import Text.Parsec
    ( Parsec
    , string
    , char
    , many1
    , letter
    , digit
    , oneOf
    , try
    , (<|>)
    )
import qualified Text.Parsec as P
import Debug.Trace (trace)
import Utils
import AoC

getInput :: IO String
getInput = readFile "input.txt"

--------------------------------------------------------------------

type Parsed = Map String Expression
type Sol1 = Int
type Sol2 = Int

data Operator = Add | Sub | Mult | Div deriving (Eq, Show)
data Expression = Var String
                | Literal Int
                | Term Expression Operator Expression
                deriving (Show)

litParser = Literal . read <$> many1 digit
varParser = Var <$> many1 letter
opParser = chooseOp <$> oneOf "+-/*"
    where chooseOp o = case o of
            '+' -> Add
            '-' -> Sub
            '*' -> Mult
            '/' -> Div
termparser = Term <$> varParser <* char ' ' <*> opParser <* char ' ' <*> varParser

expressionParser :: Parsec String st Expression
expressionParser = litParser <|> termparser

assignmentParser :: Parsec String st (String, Expression)
assignmentParser = (,) <$> many1 letter <* string ": " <*> expressionParser

parse :: String -> Parsed
parse = Map.fromList . linesWith (right . (P.parse assignmentParser ""))
    where
        right (Right e) = e
        right (Left e) = error $ show e

eval :: Parsed -> Expression -> Int
eval _     (Literal x) = x
eval stack (Var x) = eval stack (stack ! x)
eval stack (Term (Var x1) o (Var x2)) = a `op` b
    where
        a = eval stack (stack ! x1)
        b = eval stack (stack ! x2)
        op = case o of
            Add -> (+)
            Sub -> (-)
            Mult -> (*)
            -- Div -> (\a b -> if a `mod` b /= 0 then error "shit" else a `div` b)
            Div -> div
eval stack other = trace (show other) $ undefined

solve1 :: Parsed -> Sol1
solve1 parsed = eval parsed (parsed ! "root")

findZero :: Int -> Int -> (Int -> Int) -> Int
findZero x1 x2 update
    | x1' == 0 = x1
    | x2' == 0 = x2
    | otherwise = -- trace (show x1 ++ " " ++ show x2 ++ " " ++ show m) $
        if signum x1' == signum x2' then
            -- no zero passing between x1 and x2
            error "bad interval"
        else if signum m' /= signum x1' then
            findZero x1 m update
        else
            findZero x2 m update
    where
        x1' = update x1
        x2' = update x2
        m = (x1 + x2) `div` 2
        m' = update m

solve2 :: Parsed -> Sol2
solve2 parsed = findZero 0 1000000000000000 f
    where
        f x =   let stack = Map.insert "humn" (Literal x) parsed
                    (Term (Var t1) _ (Var t2)) = parsed ! "root"
                in
                    (eval stack (stack ! t1)) - (eval stack (stack ! t2))


testdata = [r|root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32|]
testresult1 = 152
testresult2 = 301

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
