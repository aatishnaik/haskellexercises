module Chapter2.ExamScore where
import Data.Char
import Data.List

showRec = let records = [ ("Saurabh Nanda", [("English", 84), ("Chemisty", 80), ("Physics", 95), ("Geography", 75)]), ("John Doe", [("Chemisty", 80), ("Physics", 95), ("Geography", 75)]), ("Jane Doe", [("Chemisty", 66), ("Phsyics", 33), ("Geography", 56)]), ("John Doe", [("Chemisty", 90), ("Economics", 45), ("Geography", 56)]), ("Bahubali", [("Hindi", 45), ("Biology", -90), ("Geography", -75)]), ("Rajnikant", [("Tamil", 110), ("Biology", 100), ("Geography", 100)])]
    in map (\r -> r ) records

main :: IO()
main = print showRec