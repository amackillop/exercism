module Acronym (abbreviate) where

import Data.Char (toUpper, isUpper, isAlpha)
import Data.List (partition)
import Data.List.Split (splitOn)

parseSnake :: String -> String
parseSnake = map (toUpper . head) . splitOn "-"

parseCamel :: String -> String
parseCamel = fst . partition isUpper

parseWords :: [String] -> [String]
parseWords = map parseWord . filter (isAlpha . head)
    where
        parseWord word
            | elem '-' word    = parseSnake word
            | isCamelCase word = parseCamel word
            | otherwise        = pure . toUpper . head $ word
        isCamelCase word = let nCaps = length . filter isUpper $ word
                           in  nCaps > 1 && nCaps < length word

abbreviate :: String -> String
abbreviate = concat . parseWords . words