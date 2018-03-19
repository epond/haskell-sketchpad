module HaskellProgramming.Chapter16Functors where

-- Commonly used functors
-- Wait, how does that even typecheck? Work through the types to see how the
-- compiler arrived at the type for (fmap . fmap)
-- (.) fmap fmap :: (Functor f, Functor f1) => (a -> b) -> f (f1 a) -> f (f1 b)


-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- fmap :: Functor f => (m -> n) -> (f m -> f n)
-- fmap :: Functor g => (x -> y) -> (g x -> g y)

-- b = (m -> n)
-- c = (f m -> f n)
-- (.) :: ((m -> n) -> (f m -> f n)) -> (a -> (m -> n)) -> a -> (f m -> f n)
-- (.) fmap :: (a -> (m -> n)) -> a -> (f m -> f n)

-- m = g x
-- n = g y
-- (.) :: (g x -> g y) -> (f (g x) -> f (g y)) -> (a -> (g x -> g y)) -> a -> (f (g x) -> f (g y))

-- a = (x -> y)
-- (.) :: (g x -> g y) -> (f (g x) -> f (g y)) -> ((x -> y) -> (g x -> g y)) -> (x -> y) -> (f (g x) -> f (g y))
-- (.) fmap :: ((x -> y) -> (g x -> g y)) -> (x -> y) -> (f (g x) -> f (g y))
-- (.) fmap fmap :: (x -> y) -> (f (g x) -> f (g y))
-- (.) fmap fmap :: (x -> y) -> f (g x) -> f (g y)