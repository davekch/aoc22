{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ 
import Data.List
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Debug.Trace (trace)
import Utils
import AoC

getInput :: IO String
getInput = readFile "input.txt"

--------------------------------------------------------------------

type Parsed = ([Element], Cycle)
type Sol1 = Int
type Sol2 = Int

data Element = E {eindex :: Int, evalue:: Int } deriving (Ord, Eq)

instance Show Element where
    show (E i a) = "E " ++ show a -- ++ " at " ++ show i

-- maps elements to their left and right neighbors
-- an element is its original index and its value
type Cycle = Map Element (Element, Element)

toList :: Cycle -> [Element]
toList cycle = toListFrom (Map.keys cycle !! 0) cycle

toListFrom :: Element -> Cycle -> [Element]
toListFrom start cycle = toList' start [] cycle
    where
        toList' e ls cycle = let next = fst $ cycle ! e in
            if next `elem` ls
            then
                ls
            else
                toList' next (next:ls) cycle

findValue :: Int -> [Element] -> Element
findValue v (el@(E _ e):es) = if e == v then el else findValue v es

buildCycle :: [Element] -> Cycle
buildCycle ls = buildCycle' Map.empty (take (length ls + 2) $ cycle ls)
    where
        buildCycle' cycle [] = cycle
        buildCycle' cycle (_:_:[]) = cycle
        buildCycle' cycle (prev:a:next:ls) = buildCycle'
            (Map.insert a (prev, next) cycle)
            (a:next:ls)

-- removes an element from the cycle, and reconnects the hole in the cycle
-- assumes the element is present
cpop :: Element -> Cycle -> Cycle
cpop a cycle = Map.union (Map.fromList [(prev, prev'), (next, next')]) poppedcycle
    where
        poppedcycle = Map.delete a cycle
        (prev,next) = cycle ! a
        (ll, _) = cycle ! prev
        (_, rr) = cycle ! next
        prev' = (ll, next)
        next' = (prev, rr)

-- assumes that a and b are adjacent (and present)
cinsertAfter :: Element -> Element -> Cycle -> Cycle
cinsertAfter a new cycle = -- trace ("cinsert " ++ show new ++ " after " ++ show a ++ "; locals = " ++ show [ll, b, rr]) $
    Map.union patchedcycle cycle
    where
        (ll, b) = cycle ! a
        (_, rr) = cycle ! b
        patchedcycle = Map.fromList [(a, (ll, new)), (new, (a,b)), (b, (new,rr))]

move :: Element -> Cycle -> Cycle
move x cycle = -- trace ("move " ++ show x ++ " after " ++ show a) $ 
    if a == x then cycle else cinsertAfter a x $ cpop x cycle
    where
        j = evalue x
        a = jump ((if j>=0 then j else j-1) `mod` Map.size cycle) x cycle

-- returns the element n away
jump :: Int -> Element -> Cycle -> Element
jump 0 e _ = e
jump j e circle
    | j > 0 = -- trace ("seeking right " ++ show e ++ " -> " ++ show (snd $ circle ! e)) $
        jump (j-1) (snd $ circle ! e) circle
    -- | j < 0 = -- trace ("seeking left " ++ show e ++ " -> " ++ show (fst $ circle ! e)) $
    --     jump (j+1) (fst $ circle ! e) circle

mixFile :: Cycle -> [Element] -> Cycle
mixFile = foldl (flip move)

parse :: String -> Parsed
parse input = (elements, buildCycle elements)
    where elements = zipWith E [0..] $ linesWith read input

solve1 :: Parsed -> Sol1
solve1 (elements, cycle) = sum . map evalue $ map (mixed !!) [i1, i2, i3]
    where
        mixed = toListFrom (findValue 0 elements) $ mixFile cycle elements
        i1 = 1000 `mod` l
        i2 = 2000 `mod` l
        i3 = 3000 `mod` l
        l = length mixed


solve2 :: Parsed -> Sol2
solve2 parsed = undefined


testdata = [r|1
2
-3
3
-2
0
4|]
testresult1 = 3
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
