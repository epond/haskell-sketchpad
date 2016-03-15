module Algorithms.MergesortSpec where

import Test.Hspec
import Algorithms.Mergesort

spec :: Spec
spec = do
    describe "Mergesort" $ do
        it "sorts an empty list" $ do
            mergesort [] `shouldBe` []
        it "sorts a list with one element" $ do
            mergesort [5] `shouldBe` [5]
        it "sorts a list with many elements" $ do
            mergesort [4,19,12,1,9] `shouldBe` [1,4,9,12,19]
