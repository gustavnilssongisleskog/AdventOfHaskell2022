module Day07 where
import Utils(trim, split)
import Data.Char(isDigit)
import Data.List(concatMap)

data Dir = Dir String Int [Dir] deriving Show
data Command = Up | Down String | Size Int deriving Show

path = "input/07.in"

func = do
    content <- readFile path

    let commands = tail $ map (stringToCommand . trim) $ split '$' content
    print commands

    let tree = buildTree commands

    print tree
    print $ sumMax100k tree

    let size = treeSize tree
    let unused = 70000000 - size
    let needed = 30000000 - unused

    print $ minAboveX tree needed

buildTree :: [Command] -> Dir
buildTree cs = Dir name size children where
    
    name = commandToName $ head cs
    
    size = subTreeSum children + commandToSize (cs !! 1)

    children = map buildTree $ splitToRelevant $ drop 2 $ init cs 

commandToName :: Command -> String
commandToName Up = ""
commandToName (Down s) = s
commandToName (Size x) = ""

commandToSize :: Command -> Int
commandToSize Up = 0
commandToSize (Down s) = 0
commandToSize (Size x) = x

subTreeSum :: [Dir] -> Int
subTreeSum ((Dir s x ds) : dirs) = x + subTreeSum dirs
subTreeSum [] = 0

treeSize :: Dir -> Int
treeSize (Dir s x ds) = x

sumMax100k :: Dir -> Int
sumMax100k (Dir s x ds) = (if x < 100000 then x else 0) + sum (map sumMax100k ds)

minAboveX :: Dir -> Int -> Int
minAboveX dir limit = minimum $ allAboveX dir limit where
    allAboveX (Dir s x ds) limit = [x | x > limit] ++ concatMap (`allAboveX` limit) ds 

relevant :: [Command] -> [Command]
relevant = flip relevant' 0 where
    relevant' :: [Command] -> Int -> [Command]
    relevant' (Up : cs) depth = if depth == 1 then [Up] else Up : relevant' cs (depth - 1)
    relevant' ((Down s) : cs) depth = Down s : relevant' cs (depth + 1)
    relevant' ((Size x) : cs) depth = Size x : relevant' cs depth
    relevant' [] _ = []

splitToRelevant :: [Command] -> [[Command]]
splitToRelevant [] = []
splitToRelevant cs = xs : splitToRelevant (drop (length xs) cs) where
    xs = relevant cs

stringToCommand :: String -> Command
stringToCommand s
    | s == "cd .." = Up
    | take 2 s == "cd" = Down (drop 3 s)
    | otherwise = Size $ sum $ map (read . head . words) $ filter (isDigit . head) $ lines s