module Day04 where
import Utils(split, listToTuple)

path = "input/4.in"

func = do
    content <- readFile path
    let intervals = map (listToTuple . map (listToTuple . map (read :: String -> Int) . split '-') . split ',') $ lines content
    print intervals

    print $ length $ filter (uncurry overlap) intervals


covers :: (Int,Int) -> (Int,Int) -> Bool
covers (a,b) (c,d) = covers' (a,b) (c,d) || covers' (c,d) (a,b) where
    covers' (a,b) (c,d) = a <= c && b >= d


overlap :: (Int,Int) -> (Int,Int) -> Bool
overlap (a,b) (c,d) = not $ b < c || d < a