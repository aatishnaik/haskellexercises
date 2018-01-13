module Chapter3.Rail where
import Data.Char
import Data.List

data Rail = MkRail{
    rail1 :: String,
    rail2 :: String,
    rail3 :: String
}deriving Show

railfence :: String -> Rail
railfence str =
    MkRail{
        rail1=skipchar (rmspace str) 0 3,
        rail2=skipchar (rmspace str) 1 1,
        rail3=skipchar (rmspace str) 2 3
    }

skipchar :: String -> Int -> Int -> String
skipchar str x n
    |((length str) > x) = [str!!x] ++ (skipchar str (x+n+1) n)
    |otherwise = ""

rmspace :: String -> String
rmspace str = if (length (delete ' ' str)) < (length str) 
    then rmspace (delete ' ' str)
    else delete ' ' str
main :: IO()
main = print (railfence "WE ARE DISCOVERED FLEE AT ONCE")