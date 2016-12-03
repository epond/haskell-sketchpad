module HaskellProgramming.Chapter6Typeclasses where

data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn y) = x == y

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
  (==) (Two x x') (Two y y') = x == y && x' == y'