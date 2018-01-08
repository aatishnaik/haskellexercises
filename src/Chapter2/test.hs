module Chapter2.Test where
import Data.Char
import Data.List

type StudentName = String
type SubjectName = String
type SubjectMarks = Int
type MarkSheet = [(String,[(String,Int)])]
subAvg :: MarkSheet -> SubjectName -> Int
subAvg mksheet subname = let subarr = (filter (\x -> (fst x == subname)) (foldl' (\arr x -> arr ++ (snd x)) [] mksheet))
                in (foldl' (\s x -> (s+(snd x))) 0 subarr) `div` length subarr
main :: IO()
main = print (subAvg [("Saurabh Nanda", [("English", 84), ("Chemisty", 80), ("Physics", 95), ("Geography", 75)]), ("John Doe", [("Chemisty", 80), ("Physics", 95), ("Geography", 75)]), ("Jane Doe", [("Chemisty", 66), ("Phsyics", 33), ("Geography", 56)]), ("John Doe", [("Chemisty", 90), ("Economics", 45), ("Geography", 56)]), ("Bahubali", [("Hindi", 45), ("Biology", -90), ("Geography", -75)]), ("Rajnikant", [("Tamil", 110), ("Biology", 100), ("Geography", 100)])] "Geography")
