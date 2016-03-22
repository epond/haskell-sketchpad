module Algorithms.MergesortSpec where

import Test.Hspec
import Algorithms.Mergesort

spec :: Spec
spec = do
    describe "MergeSort" $ do
        it "sorts an empty list" $ do
            mergeSort ([] :: [Int]) `shouldBe` []
        it "sorts a list with one element" $ do
            mergeSort ([5] :: [Int]) `shouldBe` [5]
        it "sorts a list with many elements" $ do
            mergeSort ([4,19,12,1,9] :: [Int]) `shouldBe` [1,4,9,12,19]
