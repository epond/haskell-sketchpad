module HaskellProgramming.Chapter09 where

eftBool :: Bool -> Bool -> [Bool]
eftBool x y
    | x == y    = [x]
    | x < y     = [x, y]
    | otherwise = []