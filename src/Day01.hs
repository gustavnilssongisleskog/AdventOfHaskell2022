module Day01 where
import Data.List(sort)
import Utils(split)

path = "input/01.in"

func = do
    content <- readFile path
    -- print $ split "" (lines content)
    
    let elves = map (map read) $ split "" (lines content)
    let elvesum = map sum elves
    
    print $ sum $ take 3 $ reverse $ sort elvesum