module Anagram (anagramsFor) where

import qualified Data.List as List
import qualified Data.Char as Char

lowerStr :: String -> String
lowerStr = map Char.toLower

anagramsFor :: String -> [String] -> [String]
anagramsFor word grams = filter (\x -> lowerStr x /= lowerStr word) candidates 
    where
        sWord      = List.sort $ lowerStr word
        sWords     = zip [0..] $ map (List.sort . lowerStr) grams
        candidates = [grams !! fst i | i <- filter ((== sWord) . snd) sWords]