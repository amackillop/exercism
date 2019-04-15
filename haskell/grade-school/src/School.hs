module School (School, add, empty, grade, sorted) where

import           Data.Map (Map)
import qualified Data.Map as Map

type School = Map Int [String]

add :: Int -> String -> School -> School
add gradeNum student = Map.insertWith (insert) gradeNum [student]
    where 
        insert [e] [] = [e]
        insert [e] (x:xs)
            | e < x     = e : x : xs
            | otherwise = x : (insert [e] xs)

empty :: School
empty = Map.empty

grade :: Int -> School -> [String]
grade = Map.findWithDefault []

sorted :: School -> [(Int, [String])]
sorted = Map.assocs
