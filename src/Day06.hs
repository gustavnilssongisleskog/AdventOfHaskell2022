module Day06 where
import Data.List(sort)

path = "input/06.in"

func = do
    content <- readFile path
    print $ markerPos content

markerPos :: String -> Int
markerPos xs
    | isUnique $ take 14 xs = 14
    | otherwise = 1 + markerPos (tail xs)

isUnique :: Ord a => [a] -> Bool
isUnique = isUnique' . sort where
    isUnique' (a:b:xs) = a /= b && isUnique' (b:xs)
    isUnique' _ = True