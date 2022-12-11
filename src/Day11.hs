module Day11 where
import Data.Char(isDigit)
import Data.List(sort)
import Utils(partitionPieceLength)

data Monkey = Monkey [Integer] (Integer -> Integer) (Integer -> Int) Int
instance Show Monkey where
    show (Monkey items _ _ n) = "Monkey " ++ show items ++ " " ++ show n

path = "input/11.in"

numMonkeys = 8
allDiv = 5 * 2 * 13 * 7 * 19 * 11 * 3 * 17

func = do
    content <- readFile path
    let monkeyParagraphs = partitionPieceLength 7 $ lines content
    let monkeys = map (\p -> Monkey (readStartingItems $ p !! 1) (readOperation $ p !! 2) (readTest $ take 3 $ drop 3 p) 0) monkeyParagraphs

    let rounds = iterate (simulateOneRound 0) monkeys
    let round20 = rounds !! 20
    let mostInspectionMonkeys20 = take 2 $ reverse $ sort $ map (\(Monkey _ _ _ n) -> n) round20

    let round10k = rounds !! 10000
    let mostInspectionMonkeys10k = take 2 $ reverse $ sort $ map (\(Monkey _ _ _ n) -> n) round10k

    --print $ product mostInspectionMonkeys20
    print $ product mostInspectionMonkeys10k


readStartingItems :: String -> [Integer]
readStartingItems = map (read . filter isDigit) . words . drop 18

readOperation :: String -> Integer -> Integer
readOperation s x = f where
    w = words $ drop 19 s
    op = if w !! 1 == "+" then (+) else (*)
    f = if w !! 2 == "old" then mod (op x x) allDiv else mod (op x (read $ w !! 2)) allDiv

readTest :: [String] -> Integer -> Int
readTest s x = monkey where
    modNum = read $ drop 21 $ head s
    optionTrue = read $ drop 29 $ s !! 1
    optionFalse = read $ drop 30 $ s !! 2
    monkey = if x `mod` modNum == 0 then optionTrue else optionFalse

simulateOneRound :: Int -> [Monkey] -> [Monkey]
simulateOneRound cur monkeys 
    | cur == numMonkeys = monkeys
    | otherwise = simulateOneRound (cur + 1) newMonkeys where
        (Monkey items op test n) = monkeys !! cur
        newMonkey = Monkey [] op test (n + length items)

        sentItems = monkeyThrowTo (Monkey items op test n)

        newMonkeys = zipWith monkeyAddItems (take cur monkeys ++ [newMonkey] ++ drop (cur + 1) monkeys) sentItems

monkeyThrowTo :: Monkey -> [[Integer]]
monkeyThrowTo (Monkey curItems op test _) = foldl (zipWith (++)) (replicate numMonkeys []) $ map ((\x -> replicate (test x) [] ++ [[x]] ++ replicate (numMonkeys - 1 - test x) []) {-. (`div` 3)-} . op) curItems 

monkeyAddItems :: Monkey -> [Integer] -> Monkey
monkeyAddItems (Monkey curItems op test n) add = Monkey (curItems ++ add) op test n