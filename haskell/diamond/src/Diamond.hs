module Diamond (diamond) where

repeatChar :: Char -> Int -> String
repeatChar char n = take n $ repeat char

buildRow :: Char -> Int -> Int -> String
buildRow char padding pos = 
    let firstHalf = pad pos ++ [char] ++ pad (padding - pos) in 
        firstHalf ++ (tail $ reverse firstHalf)
     where
        pad = repeatChar ' '

buildPyramid :: String -> Int -> Int -> [String]
buildPyramid [] _ _ = []
buildPyramid (c:cs) pad pos = buildRow c pad pos : buildPyramid cs pad (pos + 1)

buildDiamond :: Char -> [String]
buildDiamond char = 
    let letters = [char, pred char .. 'A'] 
        firstHalf = buildPyramid letters (length letters - 1) 0
    in  (reverse firstHalf) ++ (tail firstHalf)

diamond :: Char -> Maybe [String]
diamond char
    | char `elem` ['A'..'Z'] = Just $ buildDiamond char
    | otherwise              = Nothing