module RoseTree where

-- Rose trees. Comes with a Traversable instance.

data RoseTree a
  = RoseTree a [RoseTree a]

instance Functor RoseTree where
  fmap f (RoseTree x xs) = RoseTree (f x) $ fmap f <$> xs

instance Foldable RoseTree where
  foldMap f (RoseTree x xs) = f x <> foldMap (foldMap f) xs

instance Traversable RoseTree where
  traverse f (RoseTree x xs) = RoseTree <$> f x <*> traverse (traverse f) xs
