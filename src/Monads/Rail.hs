module Monads.Isbn where
import Data.List
import Text.Read

encode :: Int -> String -> String
encode n s = map (s !!) $ railsPath n s
decode :: Int -> String -> String
decode n s = map snd . sort $ zip (railsPath n s) s
railsPath :: Int -> String -> [Int]
railsPath n = map snd . sort . zip (cycle $ [1..n] ++ [n-1,n-2..2]) . zipWith const [0..]

railfence :: IO()
railfence = putStrLn "Enter 1 to Encode\nEnter 2 to Decode" >>
    getLine >>= \ch->
        case ((readMaybe ch)::Maybe Int) of
            Just 1->  putStrLn "Enter String to encode:" >>
                getLine >>= \str->
                    putStrLn "Enter Key value:" >>
                        getLine >>= \n -> case ((readMaybe n) :: Maybe Int) of
                            Just k -> putStrLn (encode k str)
                            _-> putStrLn "Invalid: Key has to be a number" >> railfence
            Just 2-> putStrLn "Enter String to decode:" >>
                getLine >>= \str->
                    putStrLn "Enter Key value:" >>
                        getLine >>= \n -> 
                            case ((readMaybe n) :: Maybe Int) of
                                Just k -> putStrLn (decode k str)
                                _-> putStrLn "Invalid: Key has to be a number" >> railfence
            _-> putStrLn "Invalid Choice" >> railfence