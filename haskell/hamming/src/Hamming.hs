module Hamming (distance) where

distance :: Eq a => [a] -> [a] -> Maybe Integer
distance xs ys = if (length xs == length ys) then Just $ distance' xs ys else Nothing
    where
        distance' xs ys  = foldl addIfNotEq 0 $ zipWith (==) xs ys
        addIfNotEq acc bool = if (bool) then acc else acc + 1
