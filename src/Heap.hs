module Heap where

-- Leftist min heaps. Heap-ordered binary trees where the ranks of left siblings are greater than or equal to ranks of right siblings.

data Heap a =
    Heap Int a (Heap a) (Heap a)
  | Empty
  deriving (Show, Eq)

empty :: Heap a
empty = Empty

isEmpty :: Heap a -> Bool
isEmpty Empty = True
isEmpty _ = False

rank :: Heap a -> Int
rank Empty = 0
rank (Heap r _ _ _) = r

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h Empty = h
merge Empty h = h
merge h1@(Heap _ x a1 b1) h2@(Heap _ y a2 b2)
  | x < y = makeT x a1 (merge b1 h2)
  | otherwise = makeT y a2 (merge h1 b2)
  where
    makeT x a b
      | rank a >= rank b = Heap (rank b + 1) x a b
      | otherwise = Heap (rank a + 1) x b a

insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge (Heap 1 x Empty Empty) h

findMin :: Heap a -> Maybe a
findMin Empty = Nothing
findMin (Heap _ x _ _) = Just x

deleteMin :: Ord a => Heap a -> Heap a
deleteMin Empty = Empty
deleteMin (Heap _ _ a b) = merge a b
