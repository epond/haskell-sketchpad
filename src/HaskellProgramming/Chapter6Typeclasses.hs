module HaskellProgramming.Chapter6Typeclasses where

data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn x') = x == x'

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
  (==) (Two x y) (Two x' y') = x == x' && y == y'

data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
  (==) (TisAnInt x) (TisAnInt x') = x == x'
  (==) (TisAString x) (TisAString x') = x == x'
  (==) _ _ = False

data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') = x == x' && y == y'

data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'

data Which a = ThisOne a | ThatOne a
instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne x') = x == x'
  (==) (ThatOne x) (ThatOne x') = x == x'
  (==) (ThisOne _) (ThatOne _) = False
  (==) (ThatOne _) (ThisOne _) = False