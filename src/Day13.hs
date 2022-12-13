module Day13 where
import Data.Char(isDigit)
import Data.List(sort)
import Utils(partitionPieceLength, listToTuple)

data Packet = List [Packet] | Number Int deriving (Read, Show)

path = "input/13.in"

func = readFile path >>= (print . sum . map fst . filter (uncurry (<) . snd) . zip [1..] . map (listToTuple . map readPacket . take 2) . partitionPieceLength 3 . lines)
--func = readFile path >>= (print . product . map fst . filter (\(_,num) -> num == List [List [Number 2]] || num == List [List [Number 6]]) . zip [1..] . sort . concatMap (map readPacket . take 2) . partitionPieceLength 3 . lines . (++ "\n\n[[2]]\n[[6]]"))

instance Eq Packet where
    (==) = ((==EQ) .) . comparePackets

instance Ord Packet where
    compare = comparePackets

comparePackets :: Packet -> Packet -> Ordering
comparePackets (List as) (List bs) = compareLists as bs
comparePackets (List as) (Number b) = compareLists as [Number b]
comparePackets (Number a) (List bs) = compareLists [Number a] bs
comparePackets (Number a) (Number b) = compare a b

compareLists :: [Packet] -> [Packet] -> Ordering
compareLists [] [] = EQ
compareLists [] _ = LT
compareLists _ [] = GT
compareLists (a:as) (b:bs) = if a == b then compareLists as bs else compare a b

readPacket :: String -> Packet
readPacket = read . addNumber . concatMap ((\x -> if x == "[" then "List [" else x) . return) where
    addNumber (a:b:xs) = if (a == '[' || a == ',') && isDigit b then (a:"Number ") ++ (b:addNumber xs) else a:addNumber (b:xs)
    addNumber xs = xs