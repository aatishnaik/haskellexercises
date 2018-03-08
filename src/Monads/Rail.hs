module Monads.Rail where
import Data.List
import Data.Ord
import Data.Char
import Text.Read

encode :: Int -> String -> String
encode n str =  let numL = [1..n] ++ (reverse [2..(n-1)])
                    cycList = cycle numL
                    pairList = zip cycList str
                    sortL = sortBy (comparing fst) pairList
                in map (\x -> snd x) sortL

decode :: Int -> String -> String
decode n str =  let numL = [1..(length str)]
                    charNumL = map (\c -> intToDigit c) numL
                    charPos = zip (encode n charNumL) str
                    sortL = sortBy (comparing fst) charPos
                in map (\x -> snd x) sortL

railfence :: IO()
railfence = putStrLn "Enter 1 to Encode\nEnter 2 to Decode" >>
    getLine >>= \ch->
        case ((readMaybe ch)::Maybe Int) of
            Just 1->  putStrLn "Enter String to encode:" >>
                getLine >>= \str->
                    putStrLn "Enter Key value:" >>
                        getLine >>= \n -> case ((readMaybe n) :: Maybe Int) of
                            Just k -> putStrLn (encode k str) >> railfence
                            _-> putStrLn "Invalid: Key has to be a number" >> railfence
            Just 2-> putStrLn "Enter String to decode:" >>
                getLine >>= \str->
                    putStrLn "Enter Key value:" >>
                        getLine >>= \n -> 
                            case ((readMaybe n) :: Maybe Int) of
                                Just k -> putStrLn (decode k str) >> railfence
                                _-> putStrLn "Invalid: Key has to be a number" >> railfence
            _-> putStrLn "Invalid Choice" >> railfence