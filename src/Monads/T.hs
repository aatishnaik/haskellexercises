module Monad.Test where
import Data.List
import Data.Char
displayLine :: IO()
displayLine = fmap (\str -> map (toUpper) str) getLine >>= putStrLn

getL :: IO()
getL = fmap (\str -> if str == "yes" then "Yes" else "No"
        ) getLine >>= putStrLn