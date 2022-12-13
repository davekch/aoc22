{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ 
import Data.List
import Utils
import AoC
import qualified Text.Parsec as P
import Text.Parsec (
    Parsec
    , ParseError
    , between
    , char
    , string
    , sepBy
    , digit
    , many
    , try
    , (<|>)
    )
import Data.Either (rights)

getInput :: IO String
getInput = readFile "input.txt"

--------------------------------------------------------------------

type Parsed = [[Packet]]
type Sol1 = Int
type Sol2 = Int

data Packet = I Int | L [Packet] deriving (Show, Eq)

instance Ord Packet where
    compare (I x) (I y) = compare x y
    compare (L (x:xs)) (L (y:ys)) = if x == y then compare (L xs) (L ys) else compare x y
    compare (L []) _ = LT
    compare _ (L []) = GT
    compare i@(I x) l@(L ys) = compare (L [i]) l
    compare l@(L xs) i@(I y) = compare l (L [i])

intParser :: Parsec String st Int
intParser = read <$> many digit

packetParser :: Parsec String st Packet
packetParser = (try $ L [] <$ string "[]")
    <|> (between (char '[') (char ']') packetContentparser)
    <|> (I <$> intParser)

packetContentparser :: Parsec String st Packet
packetContentparser = L <$> (packetParser `sepBy` (char ','))

parse' :: String -> [[Either ParseError Packet]]
parse' = map (map (P.parse packetParser "")) . groupByEmptyLines

parse :: String -> Parsed
parse = map rights . parse'

comparePair :: [Packet] -> Ordering
comparePair ([x, y]) = compare x y

solve1 :: Parsed -> Sol1
-- solve1 = undefined
solve1 = sum . map fst . filter ((==LT) . snd) . zip [1..] . map comparePair

divider1 = L [L [I 2]]
divider2 = L [L [I 6]]

solve2 :: Parsed -> Sol2
solve2 = product . map succ . findIndices (`elem` [divider1, divider2]) . sort . (++ [divider1, divider2]) . concat


testdata = [r|[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]|]
testresult1 = 13
testresult2 = 140

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
