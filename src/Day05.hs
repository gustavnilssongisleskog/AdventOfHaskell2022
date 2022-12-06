module Day05 where

path = "input/5.in"
cols = 9
rows = 8

func = do
    content <- readFile path
    let l = lines content
    let s = stacks $ take rows l
    print s
    let instructions = map (\x -> map ((read :: String -> Int) . (words x !!)) [1, 3, 5]) $ drop (rows + 2) l
    let result = execute s instructions

    print $ map head result

execute :: [String] -> [[Int]] -> [String]
execute xs ([amount,from,to]:is) = execute (map (\ind -> if ind == from - 1 then drop amount (xs !! ind) else if ind == to - 1 then take amount (xs !! (from - 1)) ++ (xs !! ind) else xs !! ind)  [0..cols - 1]) is
execute xs [] = xs

stacks :: [String] -> [String]
stacks xs = map (dropWhile (==' ') . reverse) $ stacks' $ reverse $ map fixIndices xs where

    stacks' :: [String] -> [String]
    stacks' (x:xs) = zipWith (:) x $ stacks' xs
    stacks' [] = ["","","","","","","","",""]
    
    fixIndices :: String -> String
    fixIndices xs = map (xs!!) [1,5..(cols * 4 - 3)]