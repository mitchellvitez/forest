module Map where

import BinaryTree
import Prelude hiding (lookup)

type Map k v = BinaryTree (k, v) 

empty :: Map k v
empty = Empty

bind :: (Ord k, Ord v) => k -> v -> Map k v -> Map k v
bind k v = insert (k, v)

lookup :: Ord k => k -> Map k v -> Maybe v
lookup _ Empty = Nothing
lookup key (BinaryTree (k, v) l r)
  | key < k = lookup key l
  | key > k = lookup key r
  | otherwise = Just v
