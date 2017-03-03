module HaskellProgramming.Chapter09 where

eftBool :: Bool -> Bool -> [Bool]
eftBool x y
    | x == y    = [x]
    | x < y     = [x, y]
    | otherwise = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y
    | x == y    = [x]
    | x < y     = x : eftOrd (succ x) y
    | otherwise = []