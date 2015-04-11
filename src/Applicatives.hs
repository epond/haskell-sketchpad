module Applicatives where

import Control.Applicative

{-
class Functor f where
    fmap :: (a -> b) -> f a -> f b

class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
-}

data Validation a b = Failure [a] | Success b deriving (Eq, Ord, Read, Show)

instance Functor (Validation a) where
    fmap _ (Failure x) = Failure x
    fmap f (Success x) = Success (f x)

instance Applicative (Validation a) where
    pure = Success
    Failure e1 <*> Failure e2 = Failure (e1 ++ e2)
    Failure e <*> Success _ = Failure e
    Success f <*> r = fmap f r