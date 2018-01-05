module Chapter2.Upper where
import Data.Char
import Data.List
upper :: String -> String
upper list = map (\x -> toUpper x) list