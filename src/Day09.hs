module Day09 where
import Utils(tupleToList, listToTuple, minimumBy, numUnique)
import Data.List(scanl)

path = "input/09.in"

func = do
    content <- readFile path
    let instructions = parseInstructions $ lines content

    let ropeHead = instructionsToHeadPoss instructions

    let ropeposs = iterate follows ropeHead
    
    print $ numUnique $ ropeposs !! 1
    print $ numUnique $ ropeposs !! 9

parseInstructions :: [String] -> [(Int, Int)]
parseInstructions = concatMap (expandInstruction . listToTuple . words)

expandInstruction :: (String, String) -> [(Int, Int)]
expandInstruction = uncurry $ flip (replicate . read) . charToMove . head

charToMove :: Char -> (Int, Int)
charToMove 'R' = (1,0)
charToMove 'U' = (0,1)
charToMove 'L' = (-1,0)
charToMove 'D' = (0,-1)

tupleAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
tupleAdd = curry $ listToTuple . uncurry (zipWith (+)) . listToTuple . map tupleToList . tupleToList

tupleSubtract :: (Int, Int) -> (Int, Int) -> (Int, Int)
tupleSubtract = curry $ listToTuple . uncurry (zipWith (-)) . listToTuple . map tupleToList . tupleToList

distanceSquared :: (Int, Int) -> (Int, Int) -> Int
distanceSquared = ((sum . map (^2) . tupleToList) .) . tupleSubtract

neighbors :: (Int,Int) -> [(Int,Int)]
neighbors = flip map (zip [-1,-1,-1,0,0,0,1,1,1] [-1,0,1,-1,0,1,-1,0,1]) . tupleAdd

moveTail :: (Int,Int) -> (Int,Int) -> (Int,Int)
moveTail h t = if distanceSquared h t <= 2 then t else minimumBy (distanceSquared h) (neighbors t)

instructionsToHeadPoss :: [(Int, Int)] -> [(Int, Int)]
instructionsToHeadPoss = scanl tupleAdd (0,0)

follows :: [(Int, Int)] -> [(Int, Int)]
follows = follows' (0,0) where
    follows' start (p:poss) = move : follows' move poss where
        move = moveTail p start
    follows' _ [] = []