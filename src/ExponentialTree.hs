{-# LANGUAGE
    DataKinds
  , KindSignatures
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module ExponentialTree where

import Data.Vector.Fixed hiding (Empty)
import Data.Vector.Fixed.Cont
import GHC.TypeLits

-- Exponential trees. Dimensionality limited by statically known vector length types.

-- Type-level multiplication, n * m
type family Mul (n :: PeanoNum) (m :: PeanoNum) :: PeanoNum where
  Mul 'Z _ = 'Z
  Mul _ 'Z = 'Z
  Mul n ('S m) = Add n (Mul n m)

-- Type-level exponentiation, a^b
type family Exp (a :: PeanoNum) (b :: PeanoNum) :: PeanoNum where
  Exp Z (S Z) = Z
  Exp a Z = S Z
  Exp a (S b) = Mul a (Exp a b)

data ExponentialTree a (d :: PeanoNum)
  = Empty
  | ExponentialTree a (VecPeano d (ExponentialTree a (Exp (S (S Z)) d)))

instance Show a => Show (ExponentialTree a d) where
  show Empty = "Empty"
  show (ExponentialTree x v) =
    "ExponentialTree " ++ show x ++ " (" ++ show v ++ ")"

instance Show x => Show (VecPeano n x) where
  show Nil = "Nil"
  show (Cons x y) = "Cons (" ++ show x ++ ") (" ++ show y ++ ")"

empty :: ExponentialTree a d
empty = Empty
