module Chapter3.Pagination where
import Data.Char
import Data.List

data Pages = MkPage {
    prev :: String,
    current :: String,
    next :: String
}deriving Show

displayPagination :: Int -> Int -> Int -> Int -> Pages
displayPagination ip ti cp np =
    let totalPage = if (ti `mod` ip == 0) then (ti `div` ip) else (ti `div` ip)+1
    in MkPage { 
        prev = if (cp > 4)
            then "<< Prev ... "++ show([(cp-4)..(cp-1)])
        else "<< Prev "++ show([1..(cp-1)]),
        current = (show (cp))++"*",
        next = if ((cp+4) > totalPage)
            then show([(cp+1)..totalPage])++ " Next >>"
        else show([(cp+1)..(cp+4)])++ "... Next >>"
    }
main :: IO()
main = print (displayPagination 9 100 3 4)