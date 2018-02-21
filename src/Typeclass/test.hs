module Typeclass.Test where

import qualified Data.Map as Mp

data Node a = Element a (Node a) | Empty deriving (Eq,Show)
x :: Num a => Node a
x = Element 1 (Element 2 (Element 3 (Element 4 Empty)))
y :: Num a => Node a
y = Element 1 (Element 2 (Element 3 (Element 4 Empty)))
z :: Num a => Node a
z = Element 2 (Element 5 (Element 6 (Element 8 Empty)))

instance Monoid (Node a) where
    mempty = Empty
    mappend (Element val nxt) (Element val2 nxt2) = case nxt of
        Empty -> Element val nxt2
        Element v n -> Element val (Element v (n `mappend` (Element val2 nxt2)))
    mappend (Element val nxt) Empty = Element val nxt
    mappend Empty (Element val nxt) = Element val nxt
    mappend Empty Empty= Empty

instance Functor Node where
    fmap func (Element val nxt) = Element (func val) (fmap func nxt)
    fmap _ Empty = Empty

instance Foldable Node where
    --foldMap func (Element val nxt) = mconcat (func val) (foldMap func nxt)
    --foldMap _ Empty = mempty
    --foldMap _ Empty = mempty
    --foldMap func (Element val nxt) = mconcat (fmap (\x -> [func x]) (Element val nxt))
    Node x ll2 -> (fn x) mappend (foldMap fn ll2)

    Empty -> mempty – TODO – think about this? LLNode x ll2 -> (fn x) mappend (foldMap fn ll2)




foldMap fn ll = case ll of
    LLEmpty -> mempty 
    LLNode x ll2 -> (fn x) mappend (foldMap fn ll2)

—foldl :: (b -> a -> b) -> b -> LinkedList a -> b foldl fn iv ll = Prelude.foldl fn iv $ foldMap (x -> [x]) ll

case ll of
    LLEmpty -> iv – TODO – is this correct? LLNode x ll2 ->
        let newIv = fn iv x
        in Prelude.foldl fn newIv ll2 null ll = Prelude.null $ foldMap (x -> [x]) ll


setData :: (Mp.Map Int String)
setData = Mp.fromList [(1,"abc"),(2,"bbc"),(3,"ccc")]

getData :: Int -> Maybe String
getData key = Mp.lookup key setData