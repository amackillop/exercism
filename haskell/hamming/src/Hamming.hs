module Hamming (distance) where

distance :: Eq a => [a] -> [a] -> Maybe Integer
distance xs ys = if (length xs == length ys) then Just $ distance' xs ys else Nothing
    where
        distance' xs ys  = foldl addIfEq 0 $ zip xs ys
        addIfEq acc (x, y) = if (x == y) then acc else acc + 1
