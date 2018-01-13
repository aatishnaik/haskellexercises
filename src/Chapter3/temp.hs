module Chapter2.ExamScore2 where

import Data.List
import Data.Char

--data Customer = MCustomer
--    {   cid :: Int,
--        cname :: String,
--        csal :: Int
--    }
data ImperialWeight = MkImperialWeight Int Int Int Int deriving (Eq, Show, Ord)

addWeights :: ImperialWeight -> ImperialWeight -> ImperialWeight
addWeights (MkImperialWeight p1 o1 d1 g1) (MkImperialWeight p2 o2 d2 g2) =
  let (addDrachm, finalGrain) = divMod (g1 + g2) 7000
      (addOunce, finalDrachm) = divMod (d1 + d2 + addDrachm) 256
      (addPound, finalOunce) = divMod (o1 + o2 + addOunce) 16
  in (MkImperialWeight (p1 + p2 + addPound) finalOunce finalDrachm finalGrain)
