module Day03 where
import Data.Char(ord)
import Utils(partitionPieceLength)

path = "input/3.in"

func = do
    content <- readFile path
    let groups = partitionPieceLength 3 $ map (map priority) $ lines content

    let works = map inAll groups

    print works

    print $ sum $ map head works

part1 = do 
    content <- readFile path
    let comps = map (\s -> splitAt (length s `div` 2) $ map priority s) $ lines content
    let commons = map (uncurry inBoth) comps
    print commons
    let packsums = map sum commons
    print packsums
    print $ sum packsums

inBoth :: [Int] -> [Int] -> [Int]
inBoth xs ys = filter (\x -> elem x xs && elem x ys) [1..52] 

inAll :: [[Int]] -> [Int]
inAll xss = filter (flip all xss . elem) [1..52]

priority :: Char -> Int
priority x = if x <= 'Z' then ord x - 38 else ord x - 96