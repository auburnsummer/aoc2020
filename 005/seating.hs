module Seating where

data Divider
    = First
    | Last
    deriving (Show)

-- Parse a string into a list of dividers.
parseChar :: Char -> Divider
parseChar 'F' = First
parseChar 'B' = Last
parseChar 'L' = First
parseChar 'R' = Last

parseString :: [Char] -> [Divider]
parseString = map parseChar

-- get a number
getNum :: Int -> Int -> [Divider] -> Int
getNum _ accum [] = accum
getNum i accum (First:xs) = getNum (i `div` 2) accum xs
getNum i accum (Last:xs) = getNum (i `div` 2) (accum + i `div` 2) xs

seatId :: String -> Int
seatId s
    = getNum 128 0 a * 8 + getNum 8 0 b
    where
        a = parseString $ take 7 s
        b = parseString $ drop 7 s

allSeatIds = map seatId . lines

maxSeatId :: String -> Int
maxSeatId s = maximum (allSeatIds s)

mySeat :: [Int] -> Int
mySeat i = head [a - 1 | a <- i, b <- i, a - b == 2 && notElem (a - 1) i]

main :: IO()
main = do
    content <- readFile "input"
    (print . mySeat . allSeatIds) content