module BTree where

-- 2-3 B-trees. Nodes have either one value and two children, or two values and three children.

data BTree a
  = Node2 a (BTree a) (BTree a)
  | Leaf2 a
  | Node3 a a (BTree a) (BTree a) (BTree a)
  | Leaf3 a a
  | Empty

member :: Ord a => a -> BTree a -> Bool
member _ Empty = False
member a (Leaf2 x) = a == x
member a (Leaf3 x y) = a == x || a == y
member a (Node2 x l r)
  | a < x = member a l
  | a > x = member a r
  | otherwise = True
member a (Node3 x y l m r)
  | a < x = member a l
  | x < a && a < y = member a m
  | y < a = member a r
  | otherwise = True
