module Typeclass.Test where

import qualified Data.Map as Mp

setData :: (Mp.Map Int String)
setData = Mp.fromList [(1,"abc"),(2,"bbc"),(3,"ccc")]

getData :: Int -> Maybe String
getData key = Mp.lookup key setData