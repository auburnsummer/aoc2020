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

numberValidator :: Int -> Int -> Maybe T.Text -> Bool
numberValidator _ _ Nothing
    = False
numberValidator from to (Just text)
    = n >= from && n <= to
    where n = read (T.unpack text)

textValidator :: Int -> Int -> T.Text -> Passport -> Bool
textValidator lower upper field pass
    = numberValidator lower upper (Map.lookup field pass)

byrValid = textValidator 1920 2002 (T.pack "byr") 

iyrValid = textValidator 2010 2020 (T.pack "iyr")

eyrValid = textValidator 2020 2030 (T.pack "eyr")

hgtValid_1 :: Maybe T.Text -> Bool
hgtValid_1 Nothing
    = False
hgtValid_1 (Just text)
    | T.pack "in" `T.isSuffixOf` text = numberValidator 59 76 (Just t)
    | T.pack "cm" `T.isSuffixOf` text = numberValidator 150 193 (Just t)
    | otherwise                       = False
    where
        t = T.dropEnd 2 text

hgtValid :: Passport -> Bool
hgtValid pass
    = hgtValid_1 (Map.lookup (T.pack "hgt") pass)


hclValid :: Passport -> Bool
hclValid pass
    = f t
    where
        t = Map.lookup (T.pack "hcl") pass
        f :: Maybe T.Text -> Bool
        f Nothing = False
        f (Just text) = f2 (T.unpack text)
        f2 :: String -> Bool
        f2 ('#':xs)
            = all f3 xs
            where
                f3 c = isDigit c || (c >= 'a' && c <= 'f')
        f2 _
            = False

eclValid :: Passport -> Bool
eclValid pass
    = f t
    where
        t = Map.lookup (T.pack "ecl") pass
        f :: Maybe T.Text -> Bool
        f Nothing  = False
        f (Just t) = f2 (T.unpack t)
        f2 :: String -> Bool
        f2 "amb" = True
        f2 "blu" = True
        f2 "brn" = True
        f2 "gry" = True
        f2 "grn" = True
        f2 "hzl" = True
        f2 "oth" = True
        f2 _     = False

pidValid :: Passport -> Bool
pidValid pass
    = pidValid_1 (Map.lookup (T.pack "pid") pass)

pidValid_1 :: Maybe T.Text -> Bool
pidValid_1 Nothing = False
pidValid_1 (Just t)
    = (length s == 9) && all isDigit s
    where s = T.unpack t

validate :: Passport -> Bool
validate pass
    = all ($ pass) [
        Map.member byr,
        Map.member iyr,
        Map.member eyr,
        Map.member hgt,
        Map.member hcl,
        Map.member ecl,
        Map.member pid,
        byrValid,
        iyrValid,
        eyrValid,
        hgtValid,
        hclValid,
        eclValid,
        pidValid
    ]
    where
        byr = T.pack "byr"
        iyr = T.pack "iyr"
        eyr = T.pack "eyr"
        hgt = T.pack "hgt"
        hcl = T.pack "hcl"
        ecl = T.pack "ecl"
        pid = T.pack "pid"

process :: String -> Int
process s
    = length (filter id (map validate (parseText (T.pack s))))

main :: IO()
main = do
    content <- readFile "input"
    let result = process content
    print result