module Day12 where
import Data.Char(ord)
import Data.List(sort, elemIndex)
import Utils(unique, notIn, index, partitionPieceLength)

inf = 1000000

path = "input/12.in"

func = do
    content <- readFile path
    let grid = lines content 
    let rows = length grid
    let cols = length $ head grid
    let len = rows * cols

    let newGrid = map (map (\c -> if c == 'S' then 'a' else if c == 'E' then 'z' else c)) grid

    -- part 1
    --let graph = buildGraph newGrid
    -- let start = index 'S' (concat grid)
    -- let end = index 'E' (concat grid)
    -- let myBFS = bfs graph (replicate start inf ++ [0] ++ replicate (len - start - 1) inf) [start] 0
    -- print $ partitionPieceLength 8 myBFS
    -- print $ myBFS !! end

    --part 2
    let graph = buildDownwardsGraph newGrid
    let start = index 'E' (concat grid)
    let myBFS = bfs graph (replicate start inf ++ [0] ++ replicate (len - start - 1) inf) [start] 0

    print $ snd $ minimum $ filter ((=='a') . fst) $ zip (concat newGrid) myBFS

buildGraph :: [String] -> [[Int]]
buildGraph xss = concat [[map (\(i,j) -> cols * i + j) $ filter (\(i,j) -> 0 <= i && i < rows && 0 <= j && j < cols && ord (xss !! r !! c) + 1 >= ord (xss !! i !! j)) [(r+1,c),(r-1,c),(r,c+1),(r,c-1)] | c <- [0..cols - 1]] | r <- [0..rows - 1]] where
    rows = length xss
    cols = length $ head xss

buildDownwardsGraph :: [String] -> [[Int]]
buildDownwardsGraph xss = concat [[map (\(i,j) -> cols * i + j) $ filter (\(i,j) -> 0 <= i && i < rows && 0 <= j && j < cols && ord (xss !! r !! c) <= ord (xss !! i !! j) + 1) [(r+1,c),(r-1,c),(r,c+1),(r,c-1)] | c <- [0..cols - 1]] | r <- [0..rows - 1]] where
    rows = length xss
    cols = length $ head xss

nodesToNeighbors :: [[Int]] -> [Int] -> [Int]
nodesToNeighbors = (unique .) . concatMap . (!!)

updateDists :: [Int] -> [Int] -> Int -> [Int]
updateDists oldDists nodes newDis = updateDists' oldDists (sort nodes) newDis 0 where
    updateDists' (d:oldDists) (n:nodes) newDis ind
        | ind == n = newDis : updateDists' oldDists nodes newDis (ind + 1)
        | otherwise = d : updateDists' oldDists (n:nodes) newDis (ind + 1)
    updateDists' oldDists _ _ _ = oldDists

notVisited :: [Int] -> [Int] -> [Int]
notVisited dists nodes = notVisited' dists nodes 0 where
    notVisited' (d:dists) (n:nodes) ind
        | ind /= n = notVisited' dists (n:nodes) (ind + 1)
        | d == inf = n : notVisited' dists nodes (ind + 1)
        | otherwise = notVisited' dists nodes (ind + 1)
    notVisited' _ _ _ = []

bfs :: [[Int]] -> [Int] -> [Int] -> Int -> [Int]
bfs _ dists [] _ = dists
bfs graph dists queue curDis = bfs graph newDists newQueue (curDis + 1) where
    newQueue = notVisited dists (nodesToNeighbors graph queue)
    newDists = updateDists dists newQueue (curDis + 1)