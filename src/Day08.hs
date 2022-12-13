module Day08 where
import Data.List(singleton) 
import Utils(transpose, matrixMax)

path = "input/08.in"

func = do
    content <- readFile path
    let xss = map (map (read . singleton)) $ lines content
    let sees = seeFromAnyWhere xss

    print $ countBools sees
    print $ matrixMax $ matrixScenics xss

seeFromLeft :: [Int] -> [Bool]
seeFromLeft = seeFromLeft' (-1) where
    seeFromLeft' :: Int -> [Int] -> [Bool]
    seeFromLeft' maxi (x:xs) = (x > maxi) : seeFromLeft' (max maxi x) xs
    seeFromLeft' _ [] = []

seeFromAnyWhere :: [[Int]] -> [[Bool]]
seeFromAnyWhere = foldl1 (zipWith $ zipWith (||)) . zipWith ($) [id, map reverse, transpose, transpose . map reverse] . map (map seeFromLeft) . zipWith ($) [id, map reverse, transpose, map reverse . transpose] . replicate 4

countBools :: [[Bool]] -> Int
countBools = sum . map (length . filter id)

numCanSee :: Int -> [Int] -> Int
numCanSee n (x:xs) = if n <= x then 1 else 1 + numCanSee n xs
numCanSee _ [] = 0

scenicsLeftRight :: [Int] -> [Int]
scenicsLeftRight xs = map ((\(as, b:bs) -> numCanSee b (reverse as) * numCanSee b bs) . (`splitAt` xs)) [0..length xs - 1]

matrixScenics :: [[Int]] -> [[Int]]
matrixScenics xss = (zipWith $ zipWith (*)) (map scenicsLeftRight xss) (transpose $ map scenicsLeftRight $ transpose xss)