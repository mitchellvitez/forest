module BinarySearchTree where

-- Binary search trees. Each left child is less than the root and each right child is greater.

data BinarySearchTree a =
    BinarySearchTree a (BinarySearchTree a) (BinarySearchTree a)
  | Empty
  deriving (Show, Eq)

insert :: Ord a => a -> BinarySearchTree a -> BinarySearchTree a
insert x Empty = BinarySearchTree x Empty Empty
insert x cur@(BinarySearchTree y l r)
  | x < y = BinarySearchTree y (insert x l) r
  | x > y = BinarySearchTree y l (insert x r)
  | otherwise = cur

member :: Ord a => a -> BinarySearchTree a -> Bool
member _ Empty = False
member x (BinarySearchTree y l r)
  | x < y = member x l
  | x > y = member x r
  | otherwise = True

findMin :: Ord a => BinarySearchTree a -> Maybe a
findMin Empty = Nothing
findMin (BinarySearchTree x Empty _) = Just x
findMin (BinarySearchTree _ l _) = findMin l

findMax :: Ord a => BinarySearchTree a -> Maybe a
findMax Empty = Nothing
findMax (BinarySearchTree x _ Empty) = Just x
findMax (BinarySearchTree _ _ r) = findMax r

size :: Ord a => BinarySearchTree a -> Int
size Empty = 0
size (BinarySearchTree _ l r) = 1 + size l + size r
