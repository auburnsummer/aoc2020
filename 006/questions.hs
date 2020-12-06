module Questions where

import qualified Data.Text as T
import Data.List
import Data.Char (isAlpha)

groups :: T.Text -> [T.Text]
groups = T.splitOn (T.pack "\n\n")

uniqueChars = T.foldl f []
    where
        f :: [Char] -> Char -> [Char]
        f arr c
            | notElem c arr && isAlpha c = c : arr
            | otherwise     = arr

processGroup :: T.Text -> Int
processGroup t = (length . uniqueChars) t

process :: T.Text -> Int
process t = sum (map processGroup (groups t))

main :: IO()
main = do
    content <- readFile "input"
    (print . process . T.pack) content