{-# LANGUAGE ExistentialQuantification #-}

-- https://en.wikibooks.org/wiki/Haskell/Existentially_quantified_types
module ExistentialTypes where

-- For any a and b we can imagine, map takes the type (a -> b) -> [a] -> [b].
-- This is equivalent to having omitted the forall since lower case type variables allow any type to fill the role.
mymap :: forall a b. (a -> b) -> [a] -> [b]
mymap f l = map f l

-- Whenever you want existential types, you must wrap them up in a datatype constructor
-- A heterogeneous list can be constructed using a 'type box' defined as an existential type
-- The class constraint limits the types we are unioning over
data ShowBox = forall s. Show s => SB s

heteroList :: [ShowBox]
heteroList = [SB (), SB 5, SB True]

-- We don't know the type of each underlying element in the list but we do know we can call the show function on it
showAll :: [ShowBox] -> IO ()
showAll xs = mapM_ (\(SB x) -> print x) xs

-- The type box can delegate the typeclass functions to the underlying element
instance Show ShowBox where
  show (SB s) = show s
showAll' :: [ShowBox] -> IO ()
showAll' xs = mapM_ print xs