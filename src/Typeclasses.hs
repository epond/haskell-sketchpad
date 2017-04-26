module Typeclasses where

class Weighable a where
    weight :: a -> Int

data Fuel = Petrol | Diesel

data Car = Car Fuel
data Motorbike = Motorbike

instance Weighable Car where
    weight (Car Petrol) = 20
    weight (Car Diesel) = 22

instance Weighable Motorbike where
    weight _ = 10

sumWeighable :: (Weighable a, Weighable b) => a -> b -> Int
sumWeighable a b = weight a + weight b