module Prime (nth) where

isFactorOf :: Int -> Int -> Bool
isFactorOf x y = x `rem` y == 0

intSqrt :: Int -> Int
intSqrt = floor . sqrt . fromIntegral

factors :: Int -> [Int]
factors num
 | num < 1   = []
 | num == 1  = [1] 
 | otherwise =  1 : num : concat factors'
    where
        factors' = [if n ^ 2 /= num then [n, num `div` n] 
                    else [n] | n <- filter (isFactorOf num) [2..intSqrt num]]

isPrime :: Int -> Bool
isPrime n = case factors n of
    [1, _] -> True
    _      -> False

primes :: [Int]
primes = 2 : filter isPrime [3, 5 ..]

nth :: Int -> Maybe Integer
nth n
    | n < 1     = Nothing
    | otherwise = Just $ toInteger $ last $ take n primes
    