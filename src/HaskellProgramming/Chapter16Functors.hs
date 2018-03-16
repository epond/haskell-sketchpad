module HaskellProgramming.Chapter16Functors where

-- Commonly used functors
-- Wait, how does that even typecheck? Work through the types to see how the
-- compiler arrived at the type for (fmap . fmap)
-- (unfinished)

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- fmap :: Functor f => (m -> n) -> (f m -> f n)
-- fmap :: Functor g => (x -> y) -> (g x -> g y)
--
-- (.) fmap :: Functor f =>                    (a -> a1 -> b) -> a        -> (f a1 -> f b)
-- (fmap . fmap) :: (Functor f, Functor f1) => (a -> b)       -> f (f1 a) -> f (f1 b)