
module Chapter2.Contact where
import Data.Char
import Data.List
{-groupNamesByAlphabet :: Char -> [String] -> (Char, [String])
groupNamesByAlphabet alpha names =
    foldl' (\ (alphabet, collectedNames) name ->
    if (length name > 0) && (toLower (head name) == (toLower alphabet))
            then (alphabet, collectedNames ++ [name])
            else (alphabet, collectedNames)) (alpha, []) names
    
groupNamesByAllAlphabets :: [String] -> [(Char, [String])]
groupNamesByAllAlphabets names = map (\alphabet -> groupNamesByAlphabet alphabet names) ['a'..'z']-}
--groupAll :: [String] -> [String]

groupAll str = groupBy (\ a b -> ((head a) == (head b))) (sort str)

main :: IO()
main = print (groupAll ["abc","bbc","abab","aaaa"])