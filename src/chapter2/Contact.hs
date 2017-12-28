
module Contact where
import Data.Char (toLower)
import Data.List (foldl')
    
groupNamesByAlphabet :: Char -> [String] -> (Char, [String])
groupNamesByAlphabet alpha names =
    foldl' (\ (alphabet, collectedNames) name ->
    if (length name > 0) && (toLower (head name) == (toLower alphabet))
            then (alphabet, collectedNames ++ [name])
            else (alphabet, collectedNames)) (alpha, []) names
    
groupNamesByAllAlphabets :: [String] -> [(Char, [String])]
groupNamesByAllAlphabets names = map (\alphabet -> groupNamesByAlphabet alphabet names) ['a'..'z']