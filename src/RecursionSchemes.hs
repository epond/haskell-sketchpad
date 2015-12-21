{-# LANGUAGE DeriveFunctor #-}

-- https://medium.com/@jaredtobin/practical-recursion-schemes-c10648ec1c29#.5dtf55ob6

module RecursionSchemes where

import Data.Functor.Foldable
import Data.List.Ordered (merge)
import Prelude hiding (Foldable, succ)

-- Natural numbers can be defined recursively as
data Natural =
    Zero
  | Succ Natural

-- Recursion can be factored out, giving us a pattern functor
data NatF r =
    ZeroF
  | SuccF r
  deriving (Show, Functor)

-- Pattern functors can be defined for lists and trees in the same way
data ListF a r =
    NilF
  | ConsF a r
  deriving (Show, Functor)

data TreeF a r =
    EmptyF
  | LeafF a
  | NodeF r r
  deriving (Show, Functor)

-- Recursion can be added to these pattern functors using Fix
type Nat    = Fix NatF
type List a = Fix (ListF a)
type Tree a = Fix (TreeF a)

-- The need to add Fix everywhere can be hidden behind constructors
zero :: Nat
zero = Fix ZeroF

succ :: Nat -> Nat
succ = Fix . SuccF

nil :: List a
nil = Fix NilF

cons :: a -> List a -> List a
cons x xs = Fix (ConsF x xs)

-- The 'Fix' type from Data.Functor.Foldable defines a generic recursive structure
-- newtype Fix f = Fix (f (Fix f))