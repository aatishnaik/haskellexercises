module Chapter2.ContactList where
import Data.Char
import Data.List

{-groupNamesByAlphabet :: [String] -> (Char, [String])
groupNamesByAlphabet names = foldl' (\ (alphabet, collectedNames) name ->
        if (length name > 0) && (toLower (head name) == (toLower alphabet))
        then (alphabet, collectedNames ++ [name])
        else (alphabet, collectedNames)
    ) ('a', []) names-}

groupNamesByAllAlphabets :: [String] -> [(Char, [String])]
groupNamesByAllAlphabets names = filter (\ (alpha, collectedName) -> length (collectedName) > 0) (map (\alphabet -> groupNamesByAllAlphabets names alphabet) "abcdefghijklmnopqwxyz")
