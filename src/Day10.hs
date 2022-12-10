module Day10 where
import Utils(partitionPieceLength)
import Data.List(intercalate)

path = "input/10.in"

func = do
    content <- readFile path

    let valsAfterCycle = regHistory (lines content) 1

    let valsDuringCycle = 1 : init valsAfterCycle

    print $ sum $ signalStrengths valsDuringCycle
    putStrLn $ drawing valsAfterCycle

regHistory :: [String] -> Int -> [Int]
regHistory (i:instructions) val = if i == "noop" then val:regHistory instructions val else val:val:regHistory instructions (val + read (last $ words i))
regHistory [] val = [val]

signalStrengths :: [Int] -> [Int]
signalStrengths rs = map (\ind -> ind * rs !! ind) [20,60..length rs - 1]

drawing :: [Int] -> String
drawing = intercalate "\n" . partitionPieceLength 40 . map ((".#"!!) . fromEnum) . zipWith crtMatchesCPU [0..239]

crtMatchesCPU :: Int -> Int -> Bool
crtMatchesCPU = (((<=1) . abs) .) . (-) . (`mod` 40)