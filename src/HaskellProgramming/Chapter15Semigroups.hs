module HaskellProgramming.Chapter15Semigroups where

import Data.Semigroup

-- Semigroup exercises

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup (Identity a) where
    Identity x <> _ = Identity x