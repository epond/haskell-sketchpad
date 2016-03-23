-- https://www.youtube.com/watch?v=52VsgyexS8Q

{-# LANGUAGE ScopedTypeVariables #-}

module HoleDrivenDevelopment where

-- A noisy hole that always provokes a type error
data Hole = Hole
-- A silent hole which will always be accepted anywhere
hole = undefined
-- If we need many holes at a time we should use one noisy hole with the rest as silent holes
-- so we can tell which hole caused the type error

-- The forall denotes the scope to which we bind the type variables a, b and c
compose :: forall a b c. (b -> c) -> (a -> b) -> a -> c
compose f g x = hole -- Replace this with a noisy hole to begin hole-driven development
    where
        -- This where clause with dummy bindings allows the compiler to check
        -- type assertions made on the function arguments
        _ = f :: b -> c
        _ = g :: a -> b
        _ = x :: a

apply :: Monad m => m (a -> b) -> m a -> m b
apply = undefined

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' = undefined