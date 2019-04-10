module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

isFactorOf :: Int -> Int -> Bool
isFactorOf x y = y `rem` x == 0

intSqrt :: Int -> Int
intSqrt = floor . sqrt . fromIntegral

factors :: Int -> [Int]
factors num = if num < 2 then [] else 1 : concat factors'
    where
        factors' = [if n /= num `div` n then [n, num `div` n] 
                    else [n] | n <- [2..intSqrt num], n `isFactorOf` num]

classify :: Int -> Maybe Classification
classify num
    | num == 1       = Just Deficient
    | aliquot == 0   = Nothing
    | aliquot > num  = Just Abundant
    | aliquot == num = Just Perfect
    | aliquot < num  = Just Deficient
    where
        aliquot = sum $ factors num