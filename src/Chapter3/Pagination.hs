module Chapter3.Pagination where
import Data.Char
import Data.List

data Pages = MkPage {
    prev :: [Int],
    current :: [Int],
    next :: [Int]
}deriving Show

pagination :: Int -> Int -> Int -> Int -> Pages
pagination initialPg totalItems currentPg numberOfPages =
    let totalPage = if (totalItems `mod` initialPg == 0) then (totalItems `div` initialPg) else (totalItems `div` initialPg)+1
    in MkPage {
        prev = if (currentPg > 4)
                then if (currentPg+4) > totalPage
                    then [(currentPg-(4+((currentPg+4)-totalPage)))..(currentPg-1)]
                    else [(currentPg-4)..(currentPg-1)]
                else [1..(currentPg-1)],
        current = [currentPg],
        next = if ((currentPg+4) > totalPage)
                then [(currentPg+1)..totalPage]
                else [(currentPg+1)..(currentPg+4)]
    }

--displayPagination :: Int -> Int -> Int -> Int -> String
displayPagination initialPg totalItems currentPg numberOfPages =
            let pages = (pagination initialPg totalItems currentPg numberOfPages)
                totalPage = if (totalItems `mod` initialPg == 0) then (totalItems `div` initialPg) else (totalItems `div` initialPg)+1
                pgstr = toStr pages
                p = if (head pgstr) == '1' then "<< Prev " else "<< Prev..."
                n = if (currentPg+4) >= totalPage then " Next >>" else "...Next >>"
            in p++pgstr++n

toStr :: Pages -> String
toStr pgs =
    let (MkPage p c n) = pgs
        prev = concat (map (\x -> " | " ++ (show x)) p)
        cp = " | "++(show (c!!0)) ++ "* | "
        next = concat (map (\x -> (show x) ++ " | ") n)
    in prev ++ cp ++ next