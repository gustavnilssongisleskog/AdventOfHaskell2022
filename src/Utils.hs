module Utils where
import Data.List(singleton)

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