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
                | FTerm Expression Operator Expression
                | Unknown
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
            Div -> div
eval stack other = trace (show other) $ undefined

reduce :: Parsed -> Expression -> Expression
reduce _     Unknown = Unknown
reduce _     (Literal x) = Literal x
reduce _     ft@(FTerm _ _ _) = ft
reduce stack (Var x) = reduce stack (stack ! x)
reduce stack (Term (Literal x1) o (Literal x2)) = Literal $ x1 `op` x2
    where
        op = case o of
            Add -> (+)
            Sub -> (-)
            Mult -> (*)
            Div -> div
reduce stack (Term Unknown o subterm) = FTerm Unknown o (reduce stack subterm)
reduce stack (Term subterm o Unknown) = FTerm (reduce stack subterm) o Unknown
reduce stack (Term ft1@(FTerm _ _ _) o ft2@(FTerm _ _ _)) = FTerm ft1 o ft2
reduce stack (Term ft1@(FTerm _ _ _) o t2) = FTerm ft1 o (reduce stack t2)
reduce stack (Term t1 o ft2@(FTerm _ _ _)) = FTerm (reduce stack t1) o ft2
reduce stack (Term sub1 o sub2) = reduce stack $ Term (reduce stack sub1) o (reduce stack sub2)


-- reduce stack (Term (Literal x1) o (Var x2)) = reduce stack $
--     Term (Literal x1) o (reduce stack (stack ! x2))
-- reduce stack (Term (Var x1) o (Literal x2)) = reduce stack $ 
--     Term (reduce stack (stack ! x1)) o (Literal x2)
-- reduce stack (Term (Var x1) o (Var x2)) = reduce stack $
--     Term (reduce stack (stack ! x1)) o (reduce stack (stack ! x2))
-- reduce stack term = term

solve1 :: Parsed -> Sol1
solve1 parsed = eval parsed (parsed ! "root")

solve2 :: Parsed -> Sol2
solve2 parsed = getval . fst . fromJust $ find snd [(stack'!"humn", eval stack' t1 == eval stack' t2) | stack' <- tries]
    where
        (Term t1 _ t2) = parsed ! "root"
        tries = [Map.insert "humn" (Literal x) parsed | x <- [30000..50000]]
        getval (Literal a) = a


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
