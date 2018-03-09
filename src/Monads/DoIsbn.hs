module Monads.DoIsbn where
import Data.Char
import Data.List

rmdash :: String -> String
rmdash str = filter (/='-') str
checkisbn :: String -> Bool
checkisbn str = 
    (cformat str) && (((sum (zipWith (\ n1 n2 -> n1*n2) [1..10] (map (\c -> (digitToInt c)) (rmdash str)))) `mod` 11) == 0)
    where cformat strval = (not) (False `elem` (map (\c -> (isDigit c)) (rmdash strval))) || (length (rmdash strval) /= 10)

checkIsbnList :: [String] -> [(String,Bool)]
checkIsbnList isbnList = map (\isbnval -> (isbnval,checkisbn isbnval)) isbnList

isbnVerifier :: IO()
isbnVerifier = 
    do 
    putStrLn "Enter a delimiter char:"
    delimiter <-getLine
    putStrLn "Enter Isbn list seperated by delimiter:"
    isbnList<-getLine
    let 
        list = map (\c-> if c == (head delimiter) then ' ' else c) isbnList
        isbnSet = words list
    putStrLn (foldl' (\opStr (i,f)-> opStr ++ i ++ " " ++ (show f) ++ "\n") "" (checkIsbnList isbnSet))