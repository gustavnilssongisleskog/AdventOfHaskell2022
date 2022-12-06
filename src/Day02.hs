module Day02 where


path = "input/2.in"

func = do
    content <- readFile path
    print $ sum $ map (\s -> gamePoints (head s) (s!!2)) $ lines content


gamePoints :: Char -> Char -> Int
gamePoints 'A' 'X' = 3 + 0
gamePoints 'A' 'Y' = 1 + 3
gamePoints 'A' 'Z' = 2 + 6
gamePoints 'B' 'X' = 1 + 0
gamePoints 'B' 'Y' = 2 + 3
gamePoints 'B' 'Z' = 3 + 6
gamePoints 'C' 'X' = 2 + 0
gamePoints 'C' 'Y' = 3 + 3
gamePoints 'C' 'Z' = 1 + 6
gamePoints _ _ = -1000000