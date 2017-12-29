module Chapter2.Pagination where
import Data.Char
import Data.List

type ItemsPerPage = Int
type TotalItems = Int
type CurrentPage = Int
displayPagination :: TotalItems -> ItemsPerPage -> CurrentPage -> String