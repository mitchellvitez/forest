module TernaryTree where

-- Ternary trees.

data TernaryTree a
  = TernaryTree a (TernaryTree a) (TernaryTree a) (TernaryTree a)
  | Empty
