module Toboggan where

-- Custom data types are cool! I guess this could just be a
-- bool, but I want to get used to how data types work.
data Cell = Tree | Empty
    deriving (Show, Eq)

-- Given a char, return a Cell.
parseChar :: Char -> Cell
parseChar '.' = Empty
parseChar '#' = Tree

-- Parse a whole line.
parseLine :: String -> [Cell]
parseLine = map parseChar

parseString :: String -> [[Cell]]
parseString = map parseLine . lines

isCrash :: [Cell] -> Int -> Bool
isCrash arr i = (concat . repeat) arr !! i == Tree

columns :: Int -> [Int]
columns i = map (*i) [0..]

coords :: Int -> Int -> Int -> [(Int, Int)]
coords i j limit
    = takeWhile f (zip (columns i) (columns j))
    where
        f (_, j) = j < limit

-- Do the thing. i is the number of cols and j the number of rows
treeCount :: Int -> Int -> [[Cell]] -> Int
treeCount i j arr
    = length (filter f g)
    where
        g = coords i j (length arr)
        f (col, row) = isCrash (arr !! row) col

inputs :: [(Int, Int)]
inputs = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

process :: [(Int, Int)] -> String -> [Int]
process ins content
    = map f ins
    where
        f (a, b) = treeCount a b (parseString content)
    
main :: IO()
main = do
    content <- readFile "input"
    let thedata = process inputs content
    let total = product thedata
    print thedata
    print total 