module Utils where
import Data.List(singleton, sort)

split :: Eq a => a -> [a] -> [[a]]
split x xs = split' x xs [] where
    split' :: Eq a => a -> [a] -> [a] -> [[a]]
    split' s (x:xs) cur 
        | s == x = cur : split' s xs []
        | otherwise = split' s xs (cur ++ [x])
    split' _ [] cur = [cur]

partitionPieceLength :: Int -> [a] -> [[a]]
partitionPieceLength _ [] = []
partitionPieceLength n xs = take n xs : partitionPieceLength n (drop n xs)

trim :: String -> String
trim = reverse . dropWhile (or . zipWith (==) " \n" . replicate 2) . reverse . dropWhile (or . zipWith (==) " \n" . replicate 2)

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose xss = map head xss : transpose (map tail xss)

matrixMax :: Ord a => [[a]] -> a
matrixMax = maximum . map maximum

listToTuple :: [a] -> (a,a)
listToTuple = head . uncurry zip . splitAt 1

tupleToList :: (a,a) -> [a]
tupleToList = uncurry (++) . unzip . singleton

numUnique :: Ord a => [a] -> Int
numUnique = length . unique

minimumBy :: Ord b => (a -> b) -> [a] -> a
minimumBy f xs = minimumBy' f (tail xs) (head xs, f $ head xs) where
    minimumBy' ::  Ord b => (a -> b) -> [a] -> (a,b) -> a
    minimumBy' f (x:xs) (minKey,minVal) = if f x < minVal then minimumBy' f xs (x, f x) else minimumBy' f xs (minKey, minVal)
    minimumBy' _ [] (minKey,minVal) = minKey

unique :: Ord a => [a] -> [a]
unique = unique' . sort where
    unique' :: Eq a => [a] -> [a]
    unique' (a:b:xs) = if a /= b then a:unique' (b:xs) else unique' (b:xs)
    unique' xs = xs

notIn :: Ord a => [a] -> [a] -> [a]
notIn xs ys = notIn' (sort xs) (sort ys) where
    notIn' (x:xs) (y:ys)
        | x < y = x : notIn' xs (y:ys)
        | x == y = notIn' xs (y:ys)
        | x > y = notIn' (x:xs) ys
    notIn' xs _ = xs

index :: Eq a => a -> [a] -> Int
index x (y:ys) = if x == y then 0 else 1 + index x ys