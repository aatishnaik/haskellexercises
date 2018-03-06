module Monad.Test where
import Data.List
import Data.Char
displayLine :: IO()
displayLine = fmap (\str -> map (toUpper) str) getLine >>= putStrLn

getL :: IO()
getL = fmap (\str -> if str == "yes" then "Yes" else "No"
        ) getLine >>= putStrLn

testEither :: Int -> Either String Int
testEither num = if num > 18 then Right 18 else Left "enter proper age"

--getLn :: IO()
getLn = getLine >>= \code -> code