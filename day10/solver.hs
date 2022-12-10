{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ 
import Data.List
import Data.List.Split (chunksOf)
import Utils
import AoC

getInput :: IO String
getInput = readFile "input.txt"

--------------------------------------------------------------------

type Parsed = [(Instruction, Int)]
type Sol1 = Int
type Sol2 = String

data Instruction = Noop | Addx Int deriving (Show)
type Registry = Int
data VM = VM {
    registry :: Registry,
    clock :: Int
    }

vminit :: VM
vminit = VM 1 1

parse :: String -> Parsed
parse input = zip instructions (map cycleLength instructions)
    where
        instructions = linesWith parseInstruction input
        parseInstruction line = case words line of
            ["noop"] -> Noop
            ["addx", x] -> Addx (read x)

-- get how long an instruction takes
cycleLength :: Instruction -> Int
cycleLength Noop = 1
cycleLength (Addx _) = 2

performInstruction :: Instruction -> Registry -> Registry
performInstruction Noop r = r
performInstruction (Addx x) r = r + x

clockTick :: VM -> Parsed -> (VM, Parsed)
clockTick (VM reg clk) ((instruction, t):is)
    | t == 1 = (VM (performInstruction instruction reg) (clk + 1), is)
    | otherwise = (VM reg (clk + 1), (instruction, t-1):is)

runProgram :: VM -> Parsed -> [Registry]
runProgram vm = reverse . runProgram' [registry vm] vm
    where
        runProgram' history vm [] = history
        runProgram' history vm instructionq = runProgram' ((registry vm'):history) vm' instructionq'
            where
                (vm', instructionq') = clockTick vm instructionq

times = [19, 59, 99, 139, 179, 219]

solve1 :: Parsed -> Sol1
solve1 parsed = sum $ zipWith (*) (map (+1) times) [reghistory !! t | t<-times]
    where
        reghistory = runProgram vminit parsed

drawPixel :: Int -> Registry -> Char
drawPixel clock reg = if clock >= reg-1 && clock <= reg+1 then '#' else '.'

solve2 :: Parsed -> Sol2
solve2 parsed = intercalate "\n" . chunksOf 40 $ zipWith drawPixel (cycle [0..39]) reghistory
    where
        -- for some reason there's one value too much at the end, so let's take init
        reghistory = init $ runProgram vminit parsed


testdata = [r|addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop|]
testresult1 = 13140
testresult2 = [r|##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....|]

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
