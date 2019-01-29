module BinomialHeap where

-- Binomial heaps.

data BinomialTree a =
  Node Int a [BinomialTree a]
  deriving (Show, Eq)

type BinomialHeap a = [BinomialTree a]

empty :: BinomialHeap a
empty = []

isEmpty :: BinomialHeap a -> Bool
isEmpty = null

rank :: BinomialTree a -> Int
rank (Node r _ _) = r

root :: BinomialTree a -> a
root (Node _ x _) = x

link :: Ord a => BinomialTree a -> BinomialTree a -> BinomialTree a
link t1@(Node r x1 c1) t2@(Node _ x2 c2)
  | x1 < x2 = Node (r+1) x1 (t2:c1)
  | otherwise = Node (r+1) x2 (t1:c2)

insTree :: Ord a => BinomialTree a -> BinomialHeap a -> BinomialHeap a
insTree t [] = [t]
insTree t (t2:ts)
  | rank t < rank t2 = t : ts
  | otherwise = insTree (link t t2) ts

insert :: Ord a => a -> BinomialHeap a -> BinomialHeap a
insert x ts = insTree (Node 0 x []) ts

merge :: Ord a => BinomialHeap a -> BinomialHeap a -> BinomialHeap a
merge ts [] = ts
merge [] ts = ts
merge all1@(t1:t1s) all2@(t2:t2s)
  | rank t1 < rank t2 = t1 : merge t1s all2
  | rank t2 < rank t1 = t2 : merge all1 t2s
  | otherwise = insTree (link t1 t2) (merge t1s t2s)

removeMinTree :: Ord a => BinomialHeap a -> (BinomialTree a, BinomialHeap a)
removeMinTree [t] = (t, [])
removeMinTree (t:ts)
  | root t < root t2 = (t, ts)
  | otherwise = (t2, t:t2s)
  where
    (t2, t2s) = removeMinTree ts

findMin :: Ord a => BinomialHeap a -> a
findMin ts = root . fst $ removeMinTree ts

deleteMin :: Ord a => BinomialHeap a -> BinomialHeap a
deleteMin ts =
  merge (reverse t1s) t2s
  where 
    (Node _ x t1s, t2s) = removeMinTree ts
