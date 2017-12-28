module Chapter2.ContactList where
import Data.Char
import Data.List

groupNamesByAlphabet :: [String] -> (Char, [String])
groupNamesByAlphabet names = foldl' (\ (alphabet, collectedNames) name ->
        if (length name > 0) && (toLower (head name) == (toLower alphabet))
        then (alphabet, collectedNames ++ [name])
        else (alphabet, collectedNames)
    ) ('a', []) names