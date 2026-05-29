-- based on Hinze, Paterson (2006)
-- https://www.cs.ox.ac.uk/ralf.hinze/publications/FingerTrees.pdf

module FingerTree
  ( FingerTree
  -- on the left
  , (<|) -- cons
  , head
  , tail

  -- on the right
  , (|>) -- snoc
  , last
  , init

  , isEmpty
  -- plus (Monoid, Applicative, Monad) instances
  -- and deriving (Eq, Show, Functor, Foldable, Traversable)
  )
where

import Prelude hiding (head, tail, last, init)

----- BASIC STRUCTURE -----

data FingerTree a
  = Empty -- no elements
  | Single a -- just one element
  | Deep (Digit a) (FingerTree (Node a)) (Digit a) -- two or more elements
  -- structured so we have quick access to both ends via the Digit "buffers"
  -- at the left and right ends, surrounding the internal FingerTree
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- Digit is like a buffer of elements placed at both the left/right ends
data Digit a
  = One   a
  | Two   a a
  | Three a a a
  | Four  a a a a
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- branch factor is 2 or 3 (2-3 trees)
-- avoids getting stuck with only pairs (a, a) but having an odd number of elements
data Node a
  = Node2 a a
  | Node3 a a a
  deriving (Eq, Show, Functor, Foldable, Traversable)

{-

Some example `FingerTree X`s, where `x :: X`,
by increasing number of elements (count the `x`s) if we keep applying `<|`

Empty
Single x
Deep (One x) Empty (One x)
Deep (Two x x) Empty (One x)
Deep (Three x x x) Empty (One x)
Deep (Four x x x x) Empty (One x)
Deep (Two x x) (Single (Node3 x x x)) (One x)
Deep (Three x x x) (Single (Node3 x x x)) (One x)
Deep (Four x x x x) (Single (Node3 x x x)) (One x)
Deep (Two x x) (Deep (One (Node3 x x x)) Empty (One (Node3 x x x))) (One x)
Deep (Three x x x) (Deep (One (Node3 x x x)) Empty (One (Node3 x x x))) (One x)

-}


----- HELPERS -----

class Reduce f where
  reduceR :: (a -> b -> b) -> f a -> b   -> b
  reduceL :: (b -> a -> b) -> b   -> f a -> b

instance Reduce [] where
  reduceR f x z = foldr f z x
  reduceL f x z = foldl f x z

-- pr, m, sf = prefix, middle, suffix -- convention from the paper above
-- i chose `f` for the operators
instance Reduce FingerTree where
  reduceR _ Empty z = z
  reduceR f (Single x) z = x `f` z
  reduceR f (Deep pr m sf) z = pr `f'` (m `f''` (sf `f'` z))
    where f' = reduceR f
          f'' = reduceR (reduceR f)

  reduceL _ z Empty = z
  reduceL f z (Single x) = z `f` x
  reduceL f z (Deep pr m sf) = ((z `f'` pr) `f''` m) `f'` sf
    where f' = reduceL f
          f'' = reduceL (reduceL f)

instance Reduce Node where
  reduceR f (Node2 a b) z = a `f` (b `f` z)
  reduceR f (Node3 a b c) z = a `f` (b `f` (c `f` z))

  reduceL f z (Node2 b a) = (z `f` b) `f` a
  reduceL f z (Node3 c b a) = ((z `f` c) `f` b) `f` a

instance Reduce Digit where
  reduceR f (One a) z = a `f` z
  reduceR f (Two a b) z = a `f` (b `f` z)
  reduceR f (Three a b c) z = a `f` (b `f` (c `f` z))
  reduceR f (Four a b c d) z = a `f` (b `f` (c `f` (d `f` z)))

  reduceL f z (One a) = z `f` a
  reduceL f z (Two b a) = (z `f` b) `f` a
  reduceL f z (Three c b a) = ((z `f` c) `f` b) `f` a
  reduceL f z (Four d c b a) = (((z `f` d) `f` c) `f` b) `f` a

toFingerTree :: Reduce f => f a -> FingerTree a
toFingerTree s = s <|^ Empty

-- toList :: Reduce f => f a -> [a]
-- toList s = reduceR (:) s []

-- convert node to digit
nodeToDigit :: Node a -> Digit a
nodeToDigit (Node2 x y) = Two x y
nodeToDigit (Node3 x y z) = Three x y z

-- head and tail for Digit (buffer of 1-4 elements)
headDigit :: Digit a -> a
headDigit (One x) = x
headDigit (Two x _) = x
headDigit (Three x _ _) = x
headDigit (Four x _ _ _) = x

tailDigit :: Digit a -> Maybe (Digit a)
tailDigit (One _) = Nothing
tailDigit (Two _ x) = Just $ One x
tailDigit (Three _ x y) = Just $ Two x y
tailDigit (Four _ x y z) = Just $ Three x y z

-- chunk a list into nodes
nodes :: [a] -> [Node a]
nodes [a, b] = [Node2 a b]
nodes [a, b, c] = [Node3 a b c]
nodes [a, b, c, d] = [Node2 a b, Node2 c d]
nodes (a:b:c:xs) = Node3 a b c : nodes xs
nodes _ = error "not enough elements to convert to Nodes"

digitToList :: Digit a -> [a]
digitToList (One a) = [a]
digitToList (Two a b) = [a, b]
digitToList (Three a b c) = [a, b, c]
digitToList (Four a b c d) = [a, b, c, d]

app3 :: FingerTree a -> [a] -> FingerTree a -> FingerTree a
app3 Empty ts xs = ts <|^ xs
app3 xs ts Empty = xs |>^ ts
app3 (Single x) ts xs = x <| (ts <|^ xs)
app3 xs ts (Single x) = (xs |>^ ts) |> x
app3 (Deep pr m sf) ts (Deep pr' m' sf') =
  Deep pr (app3 m (nodes (digitToList sf <> ts <> digitToList pr')) m') sf'


----- LEFT END -----

-- cons, add elements to the left end
infixr 5 <|

(<|) :: a -> FingerTree a -> FingerTree a
a <| Empty = Single a
a <| Single b = Deep (One a) Empty (One b)
a <| Deep (Four b c d e) m sf = Deep (Two a b) (Node3 c d e <| m) sf
a <| Deep (One pr) m sf = Deep (Two a pr) m sf
a <| Deep (Two pr1 pr2) m sf = Deep (Three a pr1 pr2) m sf
a <| Deep (Three pr1 pr2 pr3) m sf = Deep (Four a pr1 pr2 pr3) m sf

(<|^) :: Reduce f => f a -> FingerTree a -> FingerTree a
(<|^) = reduceR (<|)

-- a list-like "view" for the left end
data ViewL s a = NilL | ConsL a (s a)

viewL :: FingerTree a -> ViewL FingerTree a
viewL Empty = NilL
viewL (Single x) = ConsL x Empty
viewL (Deep pr m sf) = ConsL (headDigit pr) $ deepL (tailDigit pr) m sf

deepL :: Maybe (Digit a) -> FingerTree (Node a) -> Digit a -> FingerTree a
deepL Nothing m sf = case viewL m of
  NilL -> toFingerTree sf
  ConsL a m' -> Deep (nodeToDigit a) m' sf
deepL (Just pr) m sf = Deep pr m sf

head :: FingerTree a -> Maybe a
head x = case viewL x of
  ConsL a _ -> Just a
  _ -> Nothing

tail :: FingerTree a -> FingerTree a
tail x = case viewL x of
  ConsL _ x' -> x'
  NilL -> Empty


----- RIGHT END -----

-- snoc, add elements to the right end
infixl 5 |>

(|>) :: FingerTree a -> a -> FingerTree a
Empty |> a = Single a
Single b |> a = Deep (One b) Empty (One a)
Deep pr m (Four e d c b) |> a = Deep pr (m |> Node3 e d c) (Two b a)
Deep pr m (One sf) |> a = Deep pr m (Two sf a)
Deep pr m (Two sf1 sf2) |> a = Deep pr m (Three sf1 sf2 a)
Deep pr m (Three sf1 sf2 sf3) |> a = Deep pr m (Four sf1 sf2 sf3 a)

(|>^) :: Reduce f => FingerTree a -> f a -> FingerTree a
(|>^) = reduceL (|>)

-- a list-like "view" for the right end
data ViewR s a = NilR | ConsR (s a) a

viewR :: FingerTree a -> ViewR FingerTree a
viewR Empty = NilR
viewR (Single x) = ConsR Empty x
viewR (Deep pr m sf) = ConsR (deepR pr m (tailDigit sf)) $ headDigit sf

deepR :: Digit a -> FingerTree (Node a) -> Maybe (Digit a) -> FingerTree a
deepR pr m Nothing = case viewR m of
  NilR -> toFingerTree pr
  ConsR m' a -> Deep pr m' (nodeToDigit a)
deepR pr m (Just sf) = Deep pr m sf

last :: FingerTree a -> Maybe a
last x = case viewR x of
  ConsR _ a -> Just a
  _ -> Nothing

init :: FingerTree a -> FingerTree a
init x = case viewR x of
  ConsR x' _ -> x'
  NilR -> Empty


----- USEFUL FUNCTIONS -----

-- if the left view is empty, the whole tree is empty
isEmpty :: FingerTree a -> Bool
isEmpty x = case viewL x of
  NilL -> True
  ConsL _ _ -> False

-- this is |><| in the paper
instance Semigroup (FingerTree a) where
  xs <> ys = app3 xs [] ys

instance Monoid (FingerTree a) where
  mempty = Empty

instance Applicative FingerTree where
  pure x = Single x
  -- cross product, like []'s Applicative instance
  fs <*> xs = reduceR (\f -> reduceR (\x -> (f x <|)) xs) fs Empty

instance Monad FingerTree where
  xs >>= f = reduceR (\x -> (f x <>)) xs Empty

-- TODO: add the `Measured` thing from the paper
-- and functions like `lookup`
