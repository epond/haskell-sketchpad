module HaskellProgramming.Chapter15MonoidsSpec where

import Test.Hspec
import HaskellProgramming.Chapter15Monoids
import Data.Monoid

spec :: Spec
spec = do
    describe "The Monoid instance for our Optional type (which behaves like Maybe)" $ do
        it "combines two Sum values to give a value" $ do
            Only (Sum 1) `mappend` Only (Sum 1) `shouldBe` Only (Sum 2)
        it "combines two Product values to give a value" $ do
            Only (Product 4) `mappend` Only (Product 2) `shouldBe` Only (Product 8)
        it "combines one Sum value with Nada to give a value" $ do
            Only (Sum 1) `mappend` Nada `shouldBe` Only (Sum 1)
        it "combines one List value with Nada to give a value" $ do
            Only [1] `mappend` Nada `shouldBe` Only [1]
        it "combines Nada with one Sum value to give a value" $ do
            Nada `mappend` Only (Sum 1) `shouldBe` Only (Sum 1)
