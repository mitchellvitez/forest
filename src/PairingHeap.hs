module PairingHeap where

-- Pairing heaps. Based on heap-ordered multitrees.

data PairingHeap a
  = PairingHeap a [PairingHeap a]
  | Empty
  
empty :: PairingHeap a
empty = Empty

isEmpty :: PairingHeap a -> Bool
isEmpty Empty = True
isEmpty _ = False

merge :: Ord a => PairingHeap a -> PairingHeap a -> PairingHeap a
merge Empty h = h
merge h1@(PairingHeap x h1s) h2@(PairingHeap y h2s)
  | x < y = PairingHeap x (h2:h1s)
  | otherwise = PairingHeap y (h1:h2s)

insert :: Ord a => a -> PairingHeap a -> PairingHeap a
insert x h = merge (PairingHeap x []) h

mergePairs :: Ord a => [PairingHeap a] -> PairingHeap a
mergePairs [] = Empty
mergePairs [h] = h
mergePairs (h1:h2:hs) = merge (merge h1 h2) (mergePairs hs)

findMin :: PairingHeap a -> Maybe a
findMin Empty = Nothing
findMin (PairingHeap x _) = Just x

deleteMin :: Ord a => PairingHeap a -> PairingHeap a
deleteMin Empty = Empty
deleteMin (PairingHeap _ hs) = mergePairs hs
