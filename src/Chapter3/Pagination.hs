module Chapter3.Pagination where
import Data.Char
import Data.List

data Pages = MkPage{
    ip :: Int,
    ti :: Int,
    cp :: Int,
    np :: Int
}

displayPagination MkPage { ip = i,ti = t,cp = c,np = n} =
    MkPage { 
        ip = i,
        ti = t,
        cp = c,
        np = n
    }