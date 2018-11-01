module Chapter2.Neven where
import Data.List
neven :: Int -> [Int]
neven n = map (\n -> 2*n) [1..n]
neven2 :: Int -> [Int]
neven2 n = foldl (\evenList n -> (evenList ++ [n])) [] [1..n]