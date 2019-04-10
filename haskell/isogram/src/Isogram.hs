module Isogram (isIsogram) where

import qualified Data.List as L
import qualified Data.Char as C

isIsogram :: String -> Bool
isIsogram str = L.nub cleanStr == cleanStr
    where
        cleanStr = map C.toLower $ filter C.isAlpha str