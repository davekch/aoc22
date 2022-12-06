{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ 
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Utils as U
import Utils (CLIOptions, clioptions)

getInput :: IO String
getInput = readFile "input.txt"

--------------------------------------------------------------------

type Parsed = String
type Sol1 = Int
type Sol2 = Int

parse :: String -> Parsed
parse = id

startOfPacket :: Int -> String -> Int
startOfPacket markersize = (+markersize) . length . takeWhile not . slideWith unique markersize

solve1 :: Parsed -> Sol1
solve1 = startOfPacket 4

solve2 :: Parsed -> Sol2
solve2 = startOfPacket 14


testdata = [r|mjqjpqmgbljsphdztnvjfqwrcgsmlb|]
testresult1 = 7
testresult2 = 19

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
