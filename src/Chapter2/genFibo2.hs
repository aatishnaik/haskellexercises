module Chapter2.GenFibo2 where
import Data.Char
import Data.List

genFibo :: (Int -> Bool) -> Int -> Int -> [Int] -> Int -> Int -> [Int]
genFibo fn n1 n2 ttl n s
    |n<s = if (fn n2)
        then genFibo fn n2 (n1+n2) (ttl++[n2]) (n+1) s
        else genFibo fn n2 (n1+n2) ttl n s
    |n>=s = ttl


neven :: Int -> [Int]
neven n = genFibo (\x -> (x `mod` 2) == 0) 0 1 [] 0 n

ntri :: Int -> [Int]
ntri n = genFibo (\x -> (x `mod` 3) == 0) 0 1 [] 0 n
