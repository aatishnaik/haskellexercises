module Chapter3.Pagination where
import Data.Char
import Data.List

data Pages = MkPage {
    prev :: String,
    current :: String,
    next :: String
}deriving Show

displayPagination :: Int -> Int -> Int -> Int -> Pages
displayPagination initialPg totalItems currentPg numberOfPages =
    let totalPage = if (totalItems `mod` initialPg == 0) then (totalItems `div` initialPg) else (totalItems `div` initialPg)+1
    in MkPage {
        prev = if (currentPg > 4)
            then "<< Prev ... "++ show([(currentPg-4)..(currentPg-1)])
        else "<< Prev "++ show([1..(currentPg-1)]),
        current = (show (currentPg))++"*",
        next = if ((currentPg+4) > totalPage)
            then show([(currentPg+1)..totalPage])++ " Next >>"
        else show([(currentPg+1)..(currentPg+4)])++ "... Next >>"
    }