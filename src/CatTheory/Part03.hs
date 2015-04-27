module CatTheory.Part03 where

import Data.Monoid

-- Categories Great and Small
-- http://bartoszmilewski.com/2014/12/05/categories-great-and-small/

instance Monoid Bool where
    mempty = True
    mappend = (&&)
{-
instance Monoid Bool where
    mempty = False
    mappend = (||)
-}
instance Monoid Int where
    mempty = 0
    mappend a b = mod (a + b) 3