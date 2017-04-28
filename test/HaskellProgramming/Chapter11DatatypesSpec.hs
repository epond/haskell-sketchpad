module HaskellProgramming.Chapter11DatatypesSpec where

import Test.Hspec
import HaskellProgramming.Chapter11Datatypes

spec :: Spec
spec = do
    describe "A binary tree" $ do
        it "can insert to create a new tree" $ do
            insert' 0 Leaf `shouldBe` Node Leaf 0 Leaf
        it "can insert to a tree of one level" $ do
            insert' 5 (insert' 0 Leaf) `shouldBe` Node Leaf 0 (Node Leaf 5 Leaf)
        it "can insert to a tree of two levels" $ do
            insert' 3 (insert' 5 (insert' 0 Leaf)) `shouldBe` Node Leaf 0 (Node (Node Leaf 3 Leaf) 5 Leaf)