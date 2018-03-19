{-# LANGUAGE TemplateHaskell #-}

import Control.Lens as CL

ix :: Int -> Lens [a] a
ix index f list
  | index < 0        = error "ix: negative index"
  | null list        = error "ix: index too large"
  | old:rest <- list = if index == 0
                         then (old, liftM (: rest) (f old))
                         else second (liftM (old :)) $ ix (index-1) f rest