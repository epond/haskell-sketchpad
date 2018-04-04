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

-- Exercises: Heavy Lifting

-- a = (+1) $ read "[1]" :: [Int]
-- Expected a == [2]
a :: [Int]
a = fmap (+1) $ read "[1]"

-- b = (++ "lol") (Just ["Hi,", "Hello"])
-- Expected b == Just ["Hi,lol","Hellolol"]
b :: Maybe [String]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- c = (*2) (\x -> x - 2)
-- Expected c 1 == -2
c :: Num a => a -> a
c = fmap (*2) (\x -> x - 2)

-- d = ((return '1' ++) . show) (\x -> [x, 1..3])
-- Expected d 0 == "1[0,1,2,3]"
d :: (Enum t, Num t, Show t) => t -> [Char]
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

-- e :: IO Integer
-- e = let ioi = readIO "1" :: IO Integer
--         changed = read ("123"++) show ioi
--     in (*3) changed
-- Expected e == 3693