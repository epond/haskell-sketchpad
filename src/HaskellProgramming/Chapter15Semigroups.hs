module HaskellProgramming.Chapter15Semigroups where

import Data.Semigroup

-- Semigroup exercises

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup (Identity a) where
    Identity x <> _ = Identity x

data Two a b = Two a b deriving (Eq, Show)

instance Semigroup (Two a b) where
    (Two x y) <> _ = Two x y

data Three a b c = Three a b c deriving (Eq, Show)

instance Semigroup (Three a b c) where
    (Three x y z) <> _ = Three x y z