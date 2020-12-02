{-# OPTIONS_GHC -Wall #-}

module Passwords2 where

import Data.List
import Data.Maybe

data Range
    = Range Int Int
    deriving Show

data PasswordPolicy
    = PasswordPolicy Range Char
    deriving Show

data PasswordLine
    = PasswordLine PasswordPolicy String
    deriving Show

-- Split a string on an index, excluding index.
splitIndex :: Int -> String -> (String, String)
splitIndex idx s = (take idx s, drop (idx + 1) s)

-- Split a string once along a delimiter.
splitOnce :: Char -> String -> (String, String)
splitOnce delim s = splitIndex (fromMaybe 0 (elemIndex delim s)) s

-- Parse a range.
parseRange :: String -> Range
parseRange s
    = Range ((read . fst) t) ((read . snd) t)
    where t = splitOnce '-' s

parsePolicy :: String -> PasswordPolicy
parsePolicy s
    = PasswordPolicy ((parseRange . fst) t) ((head . snd) t)
    where t = splitOnce ' ' s

parseLine :: String -> PasswordLine
parseLine s
    = PasswordLine ((parsePolicy . fst) t) (snd t)
    where t = splitOnce ':' s

-- Determine if the index 'i' of a string equals the character.
indexEquals :: Int -> Char -> String -> Bool
indexEquals i c s
    = (s !! i) == c

-- Determine if a password line is correct or not.
validate :: PasswordLine -> Bool
validate (PasswordLine (PasswordPolicy (Range a b) c) s)
    = indexEquals a c s /= indexEquals b c s

validateString :: String -> Bool
validateString = validate . parseLine

process :: String -> Int
process s =
    length (filter id (map validateString (lines s)))

-- Open the file.
main :: IO()
main = do
    filecontent <- readFile "input"
    (print . process) filecontent
