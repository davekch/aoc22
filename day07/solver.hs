{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ 
import Data.List
import Data.Function (on)
import Data.Foldable (toList)
import Utils
import AoC

getInput :: IO String
getInput = readFile "input.txt"

--------------------------------------------------------------------

type Parsed = Directory
type Sol1 = Int
type Sol2 = Int

type Size = Int
type Directory = Tree (String, Size)

calcSizes :: Directory -> Tree Size
calcSizes (Node (_, s)) = Node s
calcSizes (Tree (_, _) children) = Tree subsize subsizes
    where
        subsizes = map calcSizes children
        subsize = sum $ map getValue subsizes

-- filter for directory sizes, removing single files from the tree
hideFiles :: Tree Size -> Tree Size
hideFiles (Tree s subsizes) = Tree s (map hideFiles . filter (not . isNode) $ subsizes)

parse :: String -> Parsed
parse input = testtree

-- group commands and outputs together
groupCommands :: String -> [[String]]
groupCommands = groupBy ((==) `on` ((=='$') . head)) . lines

solve1 :: Parsed -> Sol1
solve1 = sum . filter (<=100000) . toList . hideFiles . calcSizes

solve2 :: Parsed -> Sol2
solve2 parsed = minimum . filter (>needed) $ sizes
    where
        sizes = toList . hideFiles . calcSizes $ parsed
        total = maximum sizes
        needed = 30000000 - (70000000 - total)

testdata = [r|$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k|]
testresult1 = 95437
testresult2 = 24933642

testtree :: Directory
testtree = Tree ("/", 0) [
                Tree ("a", 0) [
                    Tree ("e", 0) [
                        Node ("i", 584)
                    ],
                    Node ("f", 29116),
                    Node ("g", 2557),
                    Node ("h.list", 62596)
                ],
                Node ("b.txt", 14848514),
                Node ("c.dat", 8504156),
                Tree ("d", 0) [
                    Node ("j", 4060174),
                    Node ("d.log", 8033020),
                    Node ("d.ext", 5626152),
                    Node ("k", 7214296)
                ]
            ]

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
