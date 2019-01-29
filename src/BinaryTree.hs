module BinaryTree where

-- Binary search trees. Each left child is less than the root and each right child is greater.

data BinaryTree a =
    BinaryTree a (BinaryTree a) (BinaryTree a)
  | Empty
  deriving (Show, Eq)

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert x Empty = BinaryTree x Empty Empty
insert x cur@(BinaryTree y l r)
  | x < y = BinaryTree y (insert x l) r
  | x > y = BinaryTree y l (insert x r)
  | otherwise = cur

member :: Ord a => a -> BinaryTree a -> Bool
member _ Empty = False
member x (BinaryTree y l r)
  | x < y = member x l
  | x > y = member x r
  | otherwise = True

findMin :: Ord a => BinaryTree a -> a
findMin Empty = error "Can't find minimum element of empty tree"
findMin (BinaryTree x Empty _) = x
findMin (BinaryTree _ l _) = findMin l

findMax :: Ord a => BinaryTree a -> a
findMax Empty = error "Can't find maximum element of empty tree"
findMax (BinaryTree x _ Empty) = x
findMax (BinaryTree _ _ r) = findMax r

size :: Ord a => BinaryTree a -> Int
size Empty = 0
size (BinaryTree _ l r) = 1 + size l + size r
