module SplayHeap where

-- Splay heaps. Does a little bit of rebalancing on each operation, based on partitioning.

data SplayHeap a =
    SplayHeap a (SplayHeap a) (SplayHeap a)
  | Empty

empty :: SplayHeap a
empty = Empty

isEmpty :: SplayHeap a -> Bool
isEmpty Empty = True
isEmpty _ = False

partition :: Ord a => a -> SplayHeap a -> (SplayHeap a, SplayHeap a)
partition _ Empty = (Empty, Empty)
partition pivot t@(SplayHeap x l r)
  | x < pivot =
    case r of
      Empty -> (t, Empty)
      (SplayHeap y r1 r2) ->
        if y < pivot then 
          let (small, big) = partition pivot r2
          in (SplayHeap y (SplayHeap x l r1) small, big)
        else
          let (small, big) = partition pivot r1
          in (SplayHeap x l small, SplayHeap y big r2)
  | otherwise =
    case l of
      Empty -> (Empty, t)
      (SplayHeap y l1 l2) ->
        if y < pivot then
          let (small, big) = partition pivot l2
          in (SplayHeap y l1 small, SplayHeap x big r) 
        else
          let (small, big) = partition pivot l1
          in (small, SplayHeap y big (SplayHeap x l2 r))

insert :: Ord a => a -> SplayHeap a -> SplayHeap a
insert x t = SplayHeap x l r
  where (l, r) = partition x t

merge :: Ord a => SplayHeap a -> SplayHeap a -> SplayHeap a
merge Empty t = t
merge (SplayHeap x l r) t =
  SplayHeap x (merge tl l) (merge tr r)
  where (tl, tr) = partition x t

findMin :: SplayHeap a -> Maybe a
findMin Empty = Nothing
findMin (SplayHeap x Empty _) = Just x
findMin (SplayHeap x l _) = findMin l

deleteMin :: SplayHeap a -> SplayHeap a
deleteMin Empty = Empty
deleteMin (SplayHeap _ Empty r) = r
deleteMin (SplayHeap x (SplayHeap _ Empty r) r2) = SplayHeap x r r2
deleteMin (SplayHeap y (SplayHeap x l r) r2) =
  SplayHeap x (deleteMin l) (SplayHeap y r r2)
