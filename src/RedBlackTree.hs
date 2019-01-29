module RedBlackTree where

-- Red-black trees.

data Color = Red | Black
  deriving (Eq, Show)

data RedBlackTree a =
    RedBlackTree Color a (RedBlackTree a) (RedBlackTree a)
  | Empty
  deriving (Eq, Show)

empty :: RedBlackTree a
empty = Empty

member :: Ord a => a -> RedBlackTree a -> Bool
member _ Empty = False
member x (RedBlackTree _ y l r)
  | x < y = member x l
  | x > y = member x r
  | otherwise = True

balance :: RedBlackTree a -> RedBlackTree a
balance (RedBlackTree Black z (RedBlackTree Red y (RedBlackTree Red x a b) c) d) =
  RedBlackTree Red y (RedBlackTree Black x a b) (RedBlackTree Black z c d)
balance (RedBlackTree Black z (RedBlackTree Red x a (RedBlackTree Red y b c)) d) =
  RedBlackTree Red y (RedBlackTree Black x a b) (RedBlackTree Black z c d)
balance (RedBlackTree Black x a (RedBlackTree Red z (RedBlackTree Red y b c) d)) =
  RedBlackTree Red y (RedBlackTree Black x a b) (RedBlackTree Black z c d)
balance (RedBlackTree Black x a (RedBlackTree Red y b (RedBlackTree Red z c d))) =
  RedBlackTree Red y (RedBlackTree Black x a b) (RedBlackTree Black z c d)
balance x = x

insert :: Ord a => a -> RedBlackTree a -> RedBlackTree a
insert x s = 
  RedBlackTree Black y l r
  where
    ins Empty = RedBlackTree Red x Empty Empty
    ins s@(RedBlackTree color y l r)
      | x < y = balance (RedBlackTree color y (ins l) r)
      | y < x = balance (RedBlackTree color y l (ins r))
      | otherwise = s
    RedBlackTree _ y l r = ins s

size :: Ord a => RedBlackTree a -> Int
size Empty = 0
size (RedBlackTree _ _ l r) = 1 + size l + size r
