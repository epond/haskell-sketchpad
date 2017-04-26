{-# LANGUAGE OverloadedStrings #-}

module TypeclassesSpec where

import Test.Hspec
import Typeclasses

spec :: Spec
spec = do
    describe "The Weighable typeclass" $ do
        it "can be used to find the weight of a Petrol Car" $ do
            weight (Car Petrol) `shouldBe` 20
        it "can be used to find the weight of a Diesel Car" $ do
            weight (Car Diesel) `shouldBe` 22
        it "can be used to find the weight of a Motorbike" $ do
            weight Motorbike `shouldBe` 10
        it "can be used to find the weight of two Weighable things" $ do
            sumWeighable (Car Diesel) Motorbike `shouldBe` 32