module Passports where

import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Char

type Passport = Map.Map T.Text T.Text

splitOnce :: T.Text -> T.Text -> (T.Text, T.Text)
splitOnce delim t
    = (head arr, last arr)
    where
        arr = T.splitOn delim t

parseLine :: T.Text -> Passport
parseLine s =
    Map.fromList (map (splitOnce delim) (T.words s))
    where
        delim = T.pack ":"

parseText :: T.Text -> [Passport]
parseText s
    = map parseLine (T.splitOn (T.pack "\n\n") s)

-- Run a predicate on a value in a passport. If the value doesn't exist, it's false no matter what.
passportTest :: T.Text -> (T.Text -> Bool) -> Passport -> Bool
passportTest field pred pass
    = f val
    where
        val = Map.lookup field pass
        f :: Maybe T.Text -> Bool
        f Nothing  = False
        f (Just t) = pred t

numberValidator :: Int -> Int -> T.Text -> Bool
numberValidator from to text
    = n >= from && n <= to
    where n = read (T.unpack text)


hgtValid :: T.Text -> Bool
hgtValid text
    | T.pack "in" `T.isSuffixOf` text = numberValidator 59 76 t
    | T.pack "cm" `T.isSuffixOf` text = numberValidator 150 193 t
    | otherwise                       = False
    where
        t = T.dropEnd 2 text

hclValid :: T.Text -> Bool
hclValid
    = f . T.unpack
    where
        f :: String -> Bool
        f ('#':xs)
            = all f3 xs
            where
                f3 c = isDigit c || (c >= 'a' && c <= 'f')
        f _
            = False

eclValid :: T.Text -> Bool
eclValid
    = f . T.unpack
    where
        f :: String -> Bool
        f "amb" = True
        f "blu" = True
        f "brn" = True
        f "gry" = True
        f "grn" = True
        f "hzl" = True
        f "oth" = True
        f _     = False

pidValid :: T.Text -> Bool
pidValid t
    = (length s == 9) && all isDigit s
    where s = T.unpack t

validate :: Passport -> Bool
validate pass
    = all ($ pass) [
        passportTest (T.pack "byr") (numberValidator 1920 2002),
        passportTest (T.pack "iyr") (numberValidator 2010 2020),
        passportTest (T.pack "eyr") (numberValidator 2020 2030),
        passportTest (T.pack "hgt") hgtValid,
        passportTest (T.pack "hcl") hclValid,
        passportTest (T.pack "ecl") eclValid,
        passportTest (T.pack "pid") pidValid
    ]

process :: String -> Int
process s
    = length (filter id (map validate (parseText (T.pack s))))

main :: IO()
main = do
    content <- readFile "input"
    let result = process content
    print result